use std::sync::Arc;

use itertools::EitherOrBoth;
use num_traits::Signed;

use crate::{core::{
    constant,
    index::FindDefError,
    protocol,
    resolve::expr::{lvalue_dn, lvalue_up, LValueSrc},
    rexpr, rexpr_tup,
    step::{analyze_unambiguous, AltDnArm, AltUpArm, Step, StepInfo},
    value, Dir, Expr, ExprDn, Item, LeafItem, Predicate, Scope, Shape, ShapeMsg, StepId, Type,
    ValueSrcId, expr::ExprKind, ConcatElem, lexpr,
}, Value, diagnostic::{DiagnosticContext, Diagnostics}};
use crate::diagnostic::{ErrorReported, Span};
use crate::entitymap::{entity_key, EntityMap};
use crate::runtime::instantiate_primitive;
use crate::syntax::ast::{self, AstNode};
use crate::tree::Tree;
use crate::{Diagnostic, FileSpan, Index, SourceFile, TypeTree};

use super::expr::{TryFromConstant, lvalue_const, zip_tuple_ast, lexpr_tup, zip_tuple_ast_fields};

enum AltMode {
    Const,
    Var(Dir),
}

impl TryFrom<Value> for AltMode {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, ()> {
        match value.as_symbol() {
            Some("const") => Ok(AltMode::Const),
            Some("dn") => Ok(AltMode::Var(Dir::Dn)),
            Some("up") => Ok(AltMode::Var(Dir::Up)),
            _ => Err(()),
        }
    }
}

impl TryFromConstant for AltMode {
    const EXPECTED_MSG: &'static str = "#up | #dn | #const";
}

#[derive(Clone, Copy)]
struct ResolveCx<'a> {
    scope: &'a Scope,
    shape_down: &'a Shape,
    shape_up: Option<&'a Shape>,
}

impl<'a> ResolveCx<'a> {
    fn with_lower<'b>(&'b self, scope: &'b Scope, shape_down: &'b Shape) -> ResolveCx<'b> {
        ResolveCx { scope, shape_down, ..*self }
    }

    fn with_upper<'b>(&'b self, scope: &'b Scope, shape_up: Option<&'b Shape>) -> ResolveCx<'b> {
        ResolveCx { scope, shape_up, ..*self }
    }

    fn with_scope<'b>(&'b self, scope: &'b Scope) -> ResolveCx<'b> {
        ResolveCx { scope, ..*self }
    }
}

entity_key!(pub ValueSinkId);

pub struct Builder<'a> {
    dcx: DiagnosticContext,
    index: &'a Index,
    steps: EntityMap<StepId, Step>,
    value_src: EntityMap<ValueSrcId, ()>,
    value_sink: EntityMap<ValueSinkId, ()>,
    upvalues: Vec<(ValueSinkId, ExprDn)>,
}

impl<'a> Builder<'a> {
    pub fn new(index: &'a Index) -> Self {
        Self {
            dcx: DiagnosticContext::new(),
            index,
            steps: EntityMap::new(),
            value_src: EntityMap::new(),
            value_sink: EntityMap::new(),
            upvalues: Vec::new(),
        }
    }

    fn add_step(&mut self, step: Step) -> StepId {
        self.steps.push(step)
    }

    fn err_step(&mut self, diag: Diagnostic) -> StepId {
        let r = self.dcx.report(diag);
        self.add_step(Step::Invalid(r))
    }

    fn add_value_src(&mut self) -> ValueSrcId {
        self.value_src.push(())
    }

    fn add_value_sink(&mut self) -> ValueSinkId {
        self.value_sink.push(())
    }

    fn up_value_src(&mut self, e: &Expr) -> ValueSrcId {
        let src = self.add_value_src();
        e.up(ExprDn::Variable(src), &mut |snk, x| self.set_upvalue(snk, x));
        src
    }

    pub fn set_upvalue(&mut self, snk: ValueSinkId, val: ExprDn) {
        self.upvalues.push((snk, val))
    }

    pub fn take_upvalue_optional(&mut self, snk: ValueSinkId, file: &Arc<SourceFile>, span: FileSpan) -> Option<ExprDn> {
        let mut i = 0;
        let mut ret = None;
        let mut multiple = false;
        while i < self.upvalues.len() {
            if self.upvalues[i].0 == snk {
                if ret.is_some() {
                    multiple = true;
                }
                ret = Some(self.upvalues.remove(i).1);
            } else {
                i += 1;
            }
        }

        if multiple {
            self.dcx.report(Diagnostic::UpValueMultiplyProvided {
                span: Span::new(file, span),
            });
        }

        ret
    }

    pub fn take_upvalue(&mut self, snk: ValueSinkId, file: &Arc<SourceFile>, span: FileSpan) -> ExprDn {
        self.take_upvalue_optional(snk, file, span).unwrap_or_else(|| {
            self.dcx.report(Diagnostic::UpValueNotProvided {
                span: Span::new(file, span),
            });
            ExprDn::invalid()
        })
    }

    fn require_down(&mut self, scope: &Scope, span: FileSpan, v: &Expr) -> ExprDn {
        v.down().unwrap_or_else(|| {
            self.dcx.report(Diagnostic::RequiredDownValue {
                span: scope.span(span),
                found: v.to_string(),
            });
            ExprDn::invalid()
        })
    }

    fn resolve_action(&mut self, sb: ResolveCx<'_>, action: &ast::Action) -> StepId {
        match action {
            ast::Action::Process(ref node) => {
                let (step, shape_up) = self.resolve_process(sb, &node);
                assert!(shape_up.is_none());
                step
            }

            ast::Action::On(ref node @ ast::ActionOn { args: Some(args), ..}) => {
                let Some(shape_up) = sb.shape_up else {
                    let r = self.dcx.report(Diagnostic::OnBlockWithoutUpSignal{
                        span: sb.scope.span(node.span)
                    });
                    return self.add_step(Step::Invalid(r));
                };

                let Some(msg_def) = shape_up.variant_named(&node.name.name) else {
                    let r = self.dcx.report(Diagnostic::NoVariantNamed {
                        span: sb.scope.span(node.name.span),
                        protocol_name: shape_up.def.ast().name.name.to_owned(),
                        name: node.name.name.to_owned(),
                    });
                    return self.add_step(Step::Invalid(r));
                };

                let mut body_scope = sb.scope.child();

                let mut dn_vars = Vec::new();
                let mut dn = Vec::new();
                let mut up_inner = Vec::new();

                for m in zip_tuple_ast_fields(&mut self.dcx, &sb.scope.file, &args, &msg_def.params) {
                    let (expr, param) = match m {
                        EitherOrBoth::Both(expr, param) => (expr, param),
                        EitherOrBoth::Left(expr) => {
                            // unexpected param: declare its variables with invalid
                            // reported in zip_tuple_ast_fields
                            let reported = ErrorReported::error_reported();
                            lexpr(&mut self.dcx, &mut body_scope, expr, &reported.into());
                            continue;
                        },
                        EitherOrBoth::Right(param) => {
                            // missing param: pad `up` or `down`
                            match param.direction {
                                Dir::Dn => {
                                    param.ty.for_each(&mut |_| {
                                        let id = self.add_value_src();
                                        dn_vars.push(id);
                                        dn.push(Predicate::Any);
                                    });
                                }
                                Dir::Up => {
                                    param.ty.for_each(&mut |_| {
                                        up_inner.push(LValueSrc::Val(ExprDn::invalid()));
                                    });
                                }
                            }
                            continue;
                        }
                    };

                    match param.direction {
                        Dir::Dn => {
                            let item = param.ty.map_leaf(&mut |ty| {
                                let id = self.add_value_src();
                                dn_vars.push(id);
                                LeafItem::Value(Expr::var_dn(id, ty.clone()))
                            });
                            self.bind_tree_fields_dn(&mut body_scope, expr, &item, &mut dn);
                        }
                        Dir::Up => {
                            self.bind_tree_fields_up(&mut body_scope,  expr, &param.ty, &mut up_inner);
                        }
                    };
                }

                let inner = if let &Some(ref body) = &node.block {
                    self.resolve_seq(sb.with_upper(&body_scope, msg_def.child.as_ref()), body)
                } else {
                    self.add_step(Step::Seq(vec![]))
                };

                let up = self.bind_tree_fields_up_finish(up_inner, &sb.scope.file);

                if msg_def.child.is_some() {
                    assert!(dn_vars.is_empty());
                    assert!(dn.is_empty());
                    assert!(up.is_empty());
                    self.add_step(Step::TokenTopTransaction { inner_mode: shape_up.mode, variant: msg_def.tag, inner })
                } else {
                    self.add_step(Step::TokenTop { inner_mode: shape_up.mode, variant: msg_def.tag, dn_vars, dn, up, inner })
                }
            }

            ast::Action::On(ref node @ ast::ActionOn { args: None, ..}) => {
                let Some(shape_up) = sb.shape_up else {
                    let r = self.dcx.report(Diagnostic::OnBlockWithoutUpSignal{
                        span: sb.scope.span(node.span)
                    });
                    return self.add_step(Step::Invalid(r));
                };

                let Some(inner_shape_up) = shape_up.child_named(&node.name.name) else {
                    let r = self.dcx.report(Diagnostic::NoChildNamed {
                        span: sb.scope.span(node.name.span),
                        protocol_name: shape_up.def.ast().name.name.to_owned(),
                        name: node.name.name.to_owned(),
                    });
                    return self.add_step(Step::Invalid(r));
                };

                if let &Some(ref body) = &node.block {
                    self.resolve_seq(sb.with_upper(&sb.scope, Some(inner_shape_up)), body)
                } else {
                    self.add_step(Step::Seq(vec![]))
                }
            }

            ast::Action::Repeat(ref node) => {
                let (dir, count) = match &node.dir_count {
                    Some((dir_ast, count_ast)) => {
                        let count = value(&mut self.dcx, sb.scope, count_ast);
                        let dir = constant::<Dir>(&mut self.dcx, sb.scope, dir_ast);
                        (dir, count)
                    }
                    None => (Ok(Dir::Up), Ok(Expr::ignored()))
                };

                let upvalues_scope = self.upvalues.len();

                let inner = self.resolve_seq(sb, &node.block);

                if self.upvalues.len() > upvalues_scope {
                    panic!("Upvalues set in repeat loop");
                }

                let count_span = node.dir_count.as_ref().map_or(node.span, |t| t.1.span());

                let count = match count {
                    Ok(count) => count,
                    Err(r) => return self.add_step(Step::Invalid(r)),
                };

                match count.get_type() {
                    Type::Number(nt)
                        if nt.is_integer() && !nt.min().is_negative() => {}
                    Type::NumberSet(s) if s.iter().all(|v| v.is_integer() && !v.is_negative()) => {}
                    Type::Ignored => {}
                    found => {
                        let r = self.dcx.report(Diagnostic::InvalidRepeatCountType {
                            span: sb.scope.span(count_span),
                            found,
                        });
                        return self.add_step(Step::Invalid(r));
                    }
                }

                match dir {
                    Ok(Dir::Up) => {
                        let (min, max) = match count.predicate() {
                            Some(Predicate::Any) => (0, None),
                            Some(Predicate::Number(n))
                                if n.is_integer()
                                && !n.is_negative() => (*n.numer(), Some(*n.numer() + 1)),
                            Some(Predicate::Range(min, max))
                                if min.is_integer()
                                && !min.is_negative()
                                && max.is_integer() => (*min.numer(), Some(*max.numer())),
                            _ => {
                                let r = self.dcx.report(Diagnostic::InvalidRepeatCountPredicate {
                                    span: sb.scope.span(count_span),
                                });
                                return self.add_step(Step::Invalid(r));
                            }
                        };
                        let count_src = self.up_value_src(&count);
                        self.add_step(Step::RepeatUp { min, max, inner, count: count_src })
                    }
                    Ok(Dir::Dn) => {
                        let count = self.require_down(&sb.scope, count_span, &count);
                        self.add_step(Step::RepeatDn { count, inner })
                    }
                    Err(r) => return self.add_step(Step::Invalid(r))
                }
            }

            ast::Action::For(ref node) => {
                enum LoopVar {
                    Const(Vec<Value>),
                    Expr {
                        ty: Type,
                        dn: Option<ExprDn>,
                        up_id: ValueSinkId,
                        up_expr: ExprKind,
                        up_collected: Option<Vec<ExprDn>>,
                        span: FileSpan
                    },
                    Invalid(ErrorReported)
                }
                
                let mut count = None;
                let mut vars = Vec::new();
                
                for &(ref name, ref expr) in &node.vars {
                    let Ok(e) = value(&mut self.dcx, sb.scope, expr) else { continue; };
                    let (c, ty) = match e.get_type() {
                        Type::Vector(c, ty) => (c, *ty),
                        other => {
                            let r = self.dcx.report(Diagnostic::ExpectedVector {
                                span: sb.scope.span(expr.span()),
                                found: other,
                            });
                            vars.push((name, LoopVar::Invalid(r)));
                            continue;
                        }
                    };

                    match count {
                        None => count = Some(c),
                        Some(count) if count == c => {},
                        Some(count) => {
                            let r = self.dcx.report(Diagnostic::ForLoopVectorWidthMismatch {
                                span1: sb.scope.span(node.vars[0].1.span()),
                                width1: count,
                                span2: sb.scope.span(expr.span()),
                                width2: c
                            });
                            vars.push((name, LoopVar::Invalid(r)));
                            continue;
                        }
                    }

                    let var = match e {
                        Expr::Const(Value::Vector(v)) => LoopVar::Const(v),
                        Expr::Const(_) => unreachable!(), // Checked that type is vector
                        Expr::Expr(_, e) => LoopVar::Expr{
                            ty,
                            dn: e.down(),
                            up_id: self.add_value_sink(),
                            up_expr: e,
                            up_collected: Some(Vec::new()),
                            span: name.span
                        },
                    };

                    vars.push((name, var));
                }

                let mut seq = Vec::new();

                for iter in 0..count.unwrap_or(0) {
                    let mut body_scope = sb.scope.child();
                    let upvalues_scope = self.upvalues.len();

                    for (name, var) in &vars {
                        let value = match var {
                            LoopVar::Const(v) => {
                                LeafItem::Value(Expr::Const(v[iter as usize].clone()))
                            }
                            LoopVar::Expr { ty, dn, up_id, ..} => {
                                let up = ExprKind::VarUp(*up_id);
                                let e = if let Some(dn) = dn {
                                    let elem = ExprDn::Index(Box::new(dn.clone()), iter);
                                    ExprKind::Flip(Box::new(ExprKind::VarDn(elem)), Box::new(up))
                                } else { up };
                                LeafItem::Value(Expr::Expr(ty.clone(), e))
                            }
                            LoopVar::Invalid(r) => LeafItem::Invalid(r.clone())
                        };

                        body_scope.bind(&name.name, Item::Leaf(value));
                    }

                    let inner = self.resolve_seq(sb.with_scope(&body_scope), &node.block);

                    for (_, var) in &mut vars {
                        if let LoopVar::Expr { up_id, up_collected, span, ..} = var {
                            if let Some(up) = self.take_upvalue_optional(*up_id, &sb.scope.file, *span) {
                                if let Some(collected) = up_collected {
                                    collected.push(up);
                                }
                            } else {
                                *up_collected = None;
                            }
                        };
                    }

                    if self.upvalues.len() > upvalues_scope {
                        panic!("Upvalues set in for loop: {:?}", self.upvalues);
                    }

                    seq.push(inner);
                }

                for (_, var) in &mut vars {
                    if let LoopVar::Expr{ up_expr, up_collected, ..} = var {
                        if let Some(collected) = up_collected {
                            let concat = ExprDn::Concat(collected.drain(..).map(ConcatElem::Elem).collect());
                            up_expr.up(concat, &mut |snk, x| self.set_upvalue(snk, x));
                        }
                    }
                }

                self.add_step(Step::Seq(seq))
            }

            ast::Action::Alt(ref node) => {
                let dir = constant::<AltMode>(&mut self.dcx, sb.scope, &node.dir);
                let scrutinee = rexpr(&mut self.dcx, sb.scope, &node.expr);

                if node.arms.is_empty() {
                    let r = self.dcx.report(Diagnostic::AltZeroArms {
                        span: sb.scope.span(node.span)
                    });
                    return self.add_step(Step::Invalid(r));
                }

                match dir {
                    Ok(AltMode::Const) => {
                        for arm in &node.arms {
                            let mut body_scope = sb.scope.child();
                            if self.bind_tree_fields_const(&mut body_scope, &arm.discriminant, &scrutinee) {
                                return self.resolve_seq(sb.with_scope(&body_scope), &arm.block);
                            }
                        }
                        self.err_step(Diagnostic::AltNoArmsMatched {
                            span: sb.scope.span(node.span)
                        })
                    }
                    Ok(AltMode::Var(Dir::Dn)) => {
                        let scrutinee_dn = scrutinee.flatten(&mut |e| {
                            match e {
                                LeafItem::Value(v) => {
                                    self.require_down(&sb.scope, node.expr.span(), v)
                                }
                                LeafItem::Invalid(_) => ExprDn::invalid(),
                                t => {
                                    self.dcx.report(Diagnostic::ExpectedValue {
                                        span: sb.scope.span(node.expr.span()),
                                        found: t.to_string()
                                    });
                                    ExprDn::invalid()
                                }
                            }
                        });

                        let arms = node.arms.iter().map(|arm| {
                            let mut body_scope = sb.scope.child();
                            let mut vals = Vec::new();
                            self.bind_tree_fields_dn(&mut body_scope, &arm.discriminant, &scrutinee, &mut vals);

                            let upvalues_scope = self.upvalues.len();
                            let body = self.resolve_seq(sb.with_scope(&body_scope), &arm.block);
                            if self.upvalues.len() > upvalues_scope {
                                // TODO: could support if all arms set the same upvalues, and we insert phi nodes to join them
                                panic!("Upvalues set in alt arm");
                            }
                            AltDnArm { vals, body }
                        }).collect();
                        self.add_step(Step::AltDn(scrutinee_dn, arms))
                    }
                    Ok(AltMode::Var(Dir::Up)) => {
                        let scrutinee_src = scrutinee.flatten(&mut |e| {
                            match e {
                                LeafItem::Value(v) => self.up_value_src(&v),
                                LeafItem::Invalid(_) => self.add_value_src(),
                                t => {
                                    self.dcx.report(Diagnostic::ExpectedValue {
                                        span: sb.scope.span(node.expr.span()),
                                        found: t.to_string()
                                    });
                                    self.add_value_src()
                                }
                            }
                        });
                        let arms = node.arms.iter().map(|arm| {
                            let mut body_scope = sb.scope.child();
                            
                            let mut up_inner = Vec::new();
                            let ty = scrutinee.as_type_tree().unwrap_or(Tree::Leaf(Type::Ignored));
                            self.bind_tree_fields_up(&mut body_scope, &arm.discriminant, &ty, &mut up_inner);

                            let upvalues_scope = self.upvalues.len();
                            let body = self.resolve_seq(sb.with_scope(&body_scope), &arm.block);

                            let vals = self.bind_tree_fields_up_finish(up_inner, &sb.scope.file);

                            if self.upvalues.len() > upvalues_scope {
                                // TODO: could support if all arms set the same upvalues, and we insert phi nodes to join them
                                panic!("Upvalues set in alt arm");
                            }
                            AltUpArm { vals, body }
                        }).collect();
                        self.add_step(Step::AltUp(arms, scrutinee_src))
                    }
                    Err(r) => {
                        return self.add_step(Step::Invalid(r));
                    }
                }
            }
            ast::Action::Error(r) => self.add_step(Step::Invalid(ErrorReported::from_ast(r))),
        }
    }

    fn bind_tree_fields_dn(
        &mut self,
        scope: &mut Scope,
        pat: &ast::Expr,
        rhs: &Item,
        dn: &mut Vec<Predicate>,
    ) {
        match (pat, rhs) {
            (ast::Expr::Tup(pat_tup), r) => {
                for m in zip_tuple_ast(&mut self.dcx, &scope.file, pat_tup, r) {
                    match m {
                        EitherOrBoth::Both(p, t) => self.bind_tree_fields_dn(scope, p, t, dn),
                        EitherOrBoth::Left(p) => {
                            // reported in zip_tuple_ast
                            let reported = ErrorReported::error_reported();
                            self.bind_tree_fields_dn(scope, p, &reported.into(), dn);
                        }
                        EitherOrBoth::Right(t) => {
                            // pad `dn` for missing fields
                            t.for_each(&mut |_| { dn.push(Predicate::Any) })
                        }
                    }
                }
            }
            (pat, Tree::Leaf(LeafItem::Value(r))) => {
                match lvalue_dn(&mut self.dcx, scope, pat, r.clone()){
                    Ok(p) => { dn.push(p); }
                    Err(_) => { dn.push(Predicate::Any); }
                }
            }
            (ast::Expr::Ignore(_), t) => {
                t.for_each(&mut |_| {
                    dn.push(Predicate::Any)
                });
            }
            (ast::Expr::Var(ref name), t @ Tree::Tuple(..)) => {
                t.for_each(&mut |_| {
                    dn.push(Predicate::Any)
                });
                scope.bind(&name.name, t.clone());
            }
            (_, Tree::Leaf(LeafItem::Invalid(_))) => {
                dn.push(Predicate::Any);
            }
            (pat, t) => {
                t.for_each(&mut |_| {
                    dn.push(Predicate::Any)
                });
                
                self.dcx.report(Diagnostic::InvalidItemForPattern {
                    span: scope.span(pat.span()),
                    found: t.to_string(),
                });
            }
        }
    }

    fn bind_tree_fields_const(
        &mut self,
        scope: &mut Scope,
        pat: &ast::Expr,
        rhs: &Item,
    ) -> bool {
        match (pat, rhs) {
            (ast::Expr::Tup(pat_tup), r) => {
                let mut matched = true;
                for m in zip_tuple_ast(&mut self.dcx, &scope.file, pat_tup, r) {
                    match m {
                        EitherOrBoth::Both(p, t) => {
                            matched &= self.bind_tree_fields_const(scope, p, t);
                        }
                        EitherOrBoth::Left(_) => {
                            matched = false;
                            // reported in zip_tuple_ast
                            let reported = ErrorReported::error_reported();
                            self.bind_tree_fields_const(scope, pat, &reported.into());
                        }
                        EitherOrBoth::Right(_) => {
                            matched = false;
                        }
                    }
                }
                matched
            }
            (ast::Expr::Ignore(_), _) => true,
            (ast::Expr::Var(ref name), t) => {
                scope.bind(&name.name, t.clone());
                true
            }
            (pat, Tree::Leaf(LeafItem::Value(Expr::Const(c)))) => {
                match lvalue_const(&mut self.dcx, scope, pat, c) {
                    Ok(p) => p,
                    Err(_) => false,
                }
            }
            (_, Tree::Leaf(LeafItem::Value(e))) => {
                self.dcx.report(Diagnostic::ExpectedConst {
                    span: scope.span(pat.span()),
                    found: e.to_string(),
                    expected: "value".into(),
                });
                false
            }
            (_, Tree::Leaf(LeafItem::Invalid(_))) => false,
            (pat, e) => {
                self.dcx.report(Diagnostic::InvalidItemForPattern {
                    span: scope.span(pat.span()),
                    found: e.to_string(),
                });
                false
            }
        }
    }

    fn bind_tree_fields_up(
        &mut self,
        scope: &mut Scope,
        pat: &ast::Expr,
        rhs: &TypeTree,
        up: &mut Vec<LValueSrc>,
    ) {
        match (pat, rhs) {
            (ast::Expr::Tup(pat_tup), r) => {
                for m in zip_tuple_ast(&mut self.dcx, &scope.file, pat_tup, r) {
                    match m {
                        EitherOrBoth::Both(p, t) => self.bind_tree_fields_up(scope, p, t, up),
                        EitherOrBoth::Left(p) => {
                            self.bind_tree_fields_up(scope, p, &Tree::Leaf(Type::Ignored), up);
                        }
                        EitherOrBoth::Right(_) => {
                            // pad `up` for missing fields
                            up.push(LValueSrc::Val(ExprDn::invalid()))
                        }
                    }
                }
            }
            (pat, Tree::Leaf(t)) => {
                match lvalue_up(&mut self.dcx, scope, pat, t.clone(), &mut || self.value_sink.push(())) {
                    Ok(x) => { up.push(x); },
                    Err(_) => { up.push(LValueSrc::Val(ExprDn::invalid())); }
                }
            }
            (ast::Expr::Var(ref name), tup @ Tree::Tuple(..)) => {
                let e = tup.map_leaf(&mut |ty| {
                    let id = self.value_sink.push(());
                    up.push(LValueSrc::Var(id, name.span));
                    LeafItem::Value(Expr::var_up(id, ty.clone()))
                });
                scope.bind(&name.name, e);
            }
            (pat, ty) => {
                up.push(LValueSrc::Val(ExprDn::invalid()));
                self.dcx.report(Diagnostic::InvalidItemForPattern {
                    span: scope.span(pat.span()),
                    found: ty.to_string(),
                });
            }
        }
    }

    fn bind_tree_fields_up_finish(&mut self, up_inner: Vec<LValueSrc>, file: &Arc<SourceFile>) -> Vec<ExprDn> {
        up_inner.into_iter().map(|v| {
            self.finish_lvalue_src(v, file)
        }).collect()
    }

    fn finish_lvalue_src(&mut self, v: LValueSrc, file: &Arc<SourceFile>) -> ExprDn {
        match v {
            LValueSrc::Var(snk, span) => self.take_upvalue(snk, file, span),
            LValueSrc::Val(e) => e,
            LValueSrc::Concat(c) => {
                ExprDn::Concat(c.into_iter().map(|e| {
                    e.map_elem_owned(|v| self.finish_lvalue_src(v, file))
                }).collect())
            }
        }
    }

    fn resolve_process(&mut self, sb: ResolveCx<'_>, process_ast: &ast::Process) -> (StepId, Option<Shape>) {
        match process_ast {
            ast::Process::Call(node) => {
                if let Some(msg_def) = sb.shape_down.variant_named(&node.name.name) {
                    let (dn, up) = self.resolve_token(msg_def, sb.scope, &node);
                    if msg_def.child.is_none() {
                        (self.add_step(Step::Token { variant: msg_def.tag, dn, up }), None)
                    } else {
                        (self.add_step(Step::TokenTransaction { variant: msg_def.tag }), msg_def.child.clone() )
                    }
                } else {
                    let def = match self.index.find_def(sb.shape_down, &node.name.name) {
                        Ok(res) => res,
                        Err(FindDefError::NoDefinitionWithName) => {
                            let r = self.dcx.report(Diagnostic::NoDefNamed {
                                span: sb.scope.span(node.name.span),
                                protocol_name: sb.shape_down.def.ast().name.name.to_owned(),
                                def_name: node.name.name.to_owned(),
                            });
                            return (self.add_step(Step::Invalid(r)), None)
                        }
                    };

                    let args = rexpr_tup(&mut self.dcx, sb.scope, &node.args);

                    let mut scope = def.file.scope();
                    lexpr(&mut self.dcx, &mut scope, &def.protocol.param, &sb.shape_down.param);
                    lexpr_tup(&mut self.dcx, &mut scope, &def.params, &args);

                    self.resolve_process(sb.with_scope(&scope), &def.implementation)
                }
            }

            ast::Process::Child(node) => {
                let Some(child_shape) = sb.shape_down.child_named(&node.name.name) else {
                    let r = self.dcx.report(Diagnostic::NoDefNamed {
                        span: sb.scope.span(node.name.span),
                        protocol_name: sb.shape_down.def.ast().name.name.to_owned(),
                        def_name: node.name.name.to_owned(),
                    });
                    return (self.add_step(Step::Invalid(r)), None)
                };

                (self.add_step(Step::Pass), Some(child_shape.clone()))
            }

            ast::Process::Primitive(node) => {
                let arg = rexpr_tup(&mut self.dcx, sb.scope, &node.args);
                debug!("instantiating primitive {} with {}", node.name.name, arg);
                let prim = match instantiate_primitive(&node.name, arg, sb.shape_down, sb.shape_up) {
                    Ok(p) => p,
                    Err(msg) => {
                        let r = self.dcx.report(Diagnostic::ErrorInPrimitiveProcess {
                            span: sb.scope.span(node.span),
                            msg
                        });
                        return (self.add_step(Step::Invalid(r)), None);

                    }
                };
                (self.add_step(Step::Primitive(prim)), None)
            }

            ast::Process::New(node) => {
                let top_shape = match protocol::resolve(&mut self.dcx, self.index, sb.scope, &node.top, 1) {
                    Ok(shape) => shape,
                    Err(r) => return (self.add_step(Step::Invalid(r)), None),
                };
                let block = self.resolve_seq(sb.with_upper(sb.scope, Some(&top_shape)), &node.block);
                (block, Some(top_shape))
            }

            ast::Process::Seq(ref block) => {
                let block = self.resolve_seq(sb.with_upper(sb.scope, None), block);
                (block, None)
            }

            ast::Process::Stack(node) => {
                let (lo, shape) = self.resolve_process(sb, &node.lower);

                let Some(shape) = shape else {
                    if let Step::Invalid(_) = &self.steps[lo] {
                        return (lo, shape);
                    } else {
                        let r = self.dcx.report(Diagnostic::StackWithoutBaseSignal {
                            span: sb.scope.span(node.lower.span()),
                        });
                        return (self.add_step(Step::Invalid(r)), None)
                    }
                };

                let (hi, shape_up) = self.resolve_process(sb.with_lower(sb.scope, &shape), &node.upper);

                match self.steps[lo] {
                    Step::Pass => (hi, shape_up),
                    _ => {
                        let stack = self.add_step(Step::Stack { lo, shape, hi });
                        (stack, shape_up)
                    }
                }
            }
        }
    }

    pub fn resolve_token(&mut self, msg_def: &ShapeMsg, scope: &Scope, node: &ast::ProcessCall) -> (Vec<ExprDn>, Vec<(Predicate, ValueSrcId)>) {
        let mut dn = Vec::new();
        let mut up = Vec::new();

        for m in zip_tuple_ast_fields(&mut self.dcx, &scope.file, &node.args, &msg_def.params) {
            let (arg, param, span) = match m {
                EitherOrBoth::Both(arg_ast, param) => {
                    let arg = rexpr(&mut self.dcx, scope, arg_ast);
                    (arg, param, arg_ast.span())
                }
                EitherOrBoth::Left(_) => {
                    // unexpected parameter passed and already reported: ignore it
                    continue
                }
                EitherOrBoth::Right(param) => {
                    // missing expected parameter
                    // reported in zip_tuple_ast_fields
                    let reported = ErrorReported::error_reported();
                    (reported.into(), param, node.args.span)
                },
            };

            let mut valid = true;
            param.ty.zip(&arg, &mut |m| { match m {
                crate::tree::Zip::Both(
                    &Tree::Leaf(ref ty),
                    &Item::Leaf(LeafItem::Value(ref v))
                ) if v.get_type().is_subtype(ty) => {
                    match param.direction {
                        Dir::Dn => {
                            dn.push(self.require_down(scope, span, v));
                        }
                        Dir::Up => {
                            let predicate = v.predicate().expect("Value cannot be up-evaluated as a predicate");
                            let src = self.up_value_src(&v);
                            up.push((predicate, src));
                        }
                    }
                },

                crate::tree::Zip::Both(_, &Item::Leaf(LeafItem::Invalid(_))) => {
                    match param.direction {
                        Dir::Dn => dn.push(ExprDn::invalid()),
                        Dir::Up => up.push((Predicate::Any, self.add_value_src())),
                    }
                }

                _ => {
                    valid = false;
                }
            }});

            if !valid {
                self.dcx.report(Diagnostic::ArgMismatchType {
                    span: scope.span(span),
                    def_name: node.name.name.clone(),
                    expected: format!("{}", param.ty),
                    found: format!("{arg}")
                });
            }
        }

        (dn, up)
    }

    fn resolve_seq(&mut self, sb: ResolveCx<'_>, block: &ast::Block) -> StepId {
        let mut scope = sb.scope.child();

        for ld in &block.lets {
            resolve_letdef(&mut self.dcx, &mut scope, &ld);
        }

        let steps = block.actions.iter().map(|action| {
            self.resolve_action(sb.with_scope(&scope), &action)
        }).collect();

        self.add_step(Step::Seq(steps))
    }
}


pub fn resolve_letdef(dcx: &mut DiagnosticContext, scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef { ref name, ref expr, .. } = ld;
    let item = rexpr(dcx, scope, expr);
    scope.bind(&name.name, item);
}

pub struct ProcessChain {
    pub steps: EntityMap<StepId, Step>,
    pub root: StepId,
    pub vars: EntityMap<ValueSrcId, ()>,
    pub step_info: EntityMap<StepId, StepInfo>,
    pub shape_dn: Shape,
    pub shape_up: Option<Shape>,
}

pub fn compile_process(index: &Index, scope: &Scope, shape_dn: Shape, ast: &ast::Process) -> Result<ProcessChain, Diagnostics> {
    let mut builder = Builder::new(index);
    let sb = ResolveCx { scope, shape_up: None, shape_down: &shape_dn };
    let (step, shape_up) = builder.resolve_process(sb, ast);

    if log_enabled!(log::Level::Debug) {
        let mut buf = String::new();
        crate::core::step::write_tree(&mut buf, 0, &builder.steps, step).unwrap();
        debug!("Steps:\n{}", buf);
    }

    let step_info = analyze_unambiguous(&builder.steps);

    if builder.dcx.has_errors() {
        return Err(builder.dcx.diagnostics());
    }

    Ok(ProcessChain {
        steps: builder.steps,
        vars: builder.value_src,
        step_info,
        root: step,
        shape_dn,
        shape_up,
    })
}
