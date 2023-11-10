use std::sync::Arc;

use num_traits::Signed;

use crate::core::{
    constant,
    index::FindDefError,
    protocol,
    resolve::expr::{lvalue_dn, lvalue_up, zip_ast, LValueSrc},
    rexpr, rexpr_tup,
    step::{analyze_unambiguous, AltDnArm, AltUpArm, Step, StepInfo},
    value, Dir, Expr, ExprDn, Item, LeafItem, Predicate, Scope, Shape, ShapeMsg, StepId, Type,
    ValueSrcId,
};
use crate::diagnostic::{ErrorReported, Span};
use crate::entitymap::{entity_key, EntityMap};
use crate::runtime::instantiate_primitive;
use crate::syntax::ast::{self, AstNode};
use crate::tree::Tree;
use crate::{Diagnostic, DiagnosticHandler, FileSpan, Index, SourceFile, TypeTree};

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
    ui: &'a dyn DiagnosticHandler,
    index: &'a Index,
    steps: EntityMap<StepId, Step>,
    value_src: EntityMap<ValueSrcId, ()>,
    value_sink: EntityMap<ValueSinkId, ()>,
    upvalues: Vec<(ValueSinkId, ExprDn)>,
}

impl<'a> Builder<'a> {
    pub fn new(ui: &'a dyn DiagnosticHandler, index: &'a Index) -> Self {
        Self {
            ui,
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

    pub fn take_upvalue(&mut self, snk: ValueSinkId, file: &Arc<SourceFile>, span: FileSpan) -> ExprDn {
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
            self.ui.report(Diagnostic::UpValueMultiplyProvided {
                span: Span::new(file, span),
            });
        }

        ret.unwrap_or_else(|| {
            self.ui.report(Diagnostic::UpValueNotProvided {
                span: Span::new(file, span),
            });
            ExprDn::invalid()
        })
    }

    fn require_down(&self, scope: &Scope, span: FileSpan, v: &Expr) -> ExprDn {
        v.down().unwrap_or_else(|| {
            self.ui.report(Diagnostic::RequiredDownValue {
                span: Span::new(&scope.file, span),
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
                    let r = self.ui.report(Diagnostic::OnBlockWithoutUpSignal{
                        span: Span::new(&sb.scope.file, node.span)
                    });
                    return self.add_step(Step::Invalid(r));
                };

                let Some(msg_def) = shape_up.variant_named(&node.name.name) else {
                    let r = self.ui.report(Diagnostic::NoVariantNamed {
                        span: Span::new(&sb.scope.file, node.name.span),
                        protocol_name: shape_up.def.ast().name.name.to_owned(),
                        name: node.name.name.to_owned(),
                    });
                    return self.add_step(Step::Invalid(r));
                };

                if args.items.len() != msg_def.params.len() {
                    self.ui.report(Diagnostic::ArgsMismatchCount {
                        span: Span::new(&sb.scope.file, args.span),
                        def_name: node.name.name.clone(),
                        expected: msg_def.params.len(),
                        found: args.items.len(),
                    });
                }

                let mut body_scope = sb.scope.child();

                let mut dn_vars = Vec::new();
                let mut dn = Vec::new();
                let mut up_inner = Vec::new();

                for (param, expr) in msg_def.params.iter().zip(&args.items) {
                    match param.direction {
                        Dir::Dn => {
                            let item = param.ty.map_leaf(&mut |ty| {
                                let id = self.add_value_src();
                                dn_vars.push(id);
                                LeafItem::Value(Expr::var_dn(id, ty.clone()))
                            });
                            self.bind_tree_fields_dn(&mut body_scope, &item, expr, &mut dn).ok();
                        }
                        Dir::Up => {
                            self.bind_tree_fields_up(&mut body_scope,  &param.ty, expr, &mut up_inner).ok();
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
                    let r = self.ui.report(Diagnostic::OnBlockWithoutUpSignal{
                        span: Span::new(&sb.scope.file, node.span)
                    });
                    return self.add_step(Step::Invalid(r));
                };

                let Some(inner_shape_up) = shape_up.child_named(&node.name.name) else {
                    let r = self.ui.report(Diagnostic::NoChildNamed {
                        span: Span::new(&sb.scope.file, node.name.span),
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
                        let count = value(self.ui, sb.scope, count_ast);
                        let dir = constant::<Dir>(self.ui, sb.scope, dir_ast);
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
                    Type::Number(lo, hi)
                        if lo.is_integer() && hi.is_integer() && !lo.is_negative() => {}
                    Type::Ignored => {}
                    found => {
                        let r = self.ui.report(Diagnostic::InvalidRepeatCountType {
                            span: Span::new(&sb.scope.file, count_span),
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
                                let r = self.ui.report(Diagnostic::InvalidRepeatCountPredicate {
                                    span: Span::new(&sb.scope.file, count_span),
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
                let mut body_scope = sb.scope.child();
                let mut count = None;

                let mut vars_dn: Vec<(ExprDn, ValueSrcId)> = Vec::new();
                let mut vars_up_inner:Vec<(ValueSinkId, Expr, FileSpan)> = Vec::new();
                
                for &(ref name, ref expr) in &node.vars {
                    let Ok(e) = value(self.ui, sb.scope, expr) else { continue; };
                    let (c, ty) = match e.get_type() {
                        Type::Vector(c, ty) => (c, ty),
                        other => {
                            self.ui.report(Diagnostic::ExpectedVector {
                                span: Span::new(&sb.scope.file, expr.span()),
                                found: other,
                            });

                            continue;
                        }
                    };

                    match count {
                        None => count = Some(c),
                        Some(count) if count == c => {},
                        Some(count) => {
                            self.ui.report(Diagnostic::ForLoopVectorWidthMismatch {
                                span1: Span::new(&sb.scope.file, node.vars[0].1.span()),
                                width1: count,
                                span2: Span::new(&sb.scope.file, expr.span()),
                                width2: c
                            });
                            continue;
                        }
                    }

                    let value = if let Some(e_dn) = e.down() {
                        let id = self.add_value_src(); // The element of the vector inside the loop
                        vars_dn.push((e_dn, id));
                        Expr::var_dn(id, *ty)
                    } else {
                        let id = self.add_value_sink();
                        vars_up_inner.push((id, e, name.span));
                        Expr::var_up(id, *ty)
                    };

                    body_scope.bind(&name.name, Item::Leaf(LeafItem::Value(value)));
                }

                let upvalues_scope = self.upvalues.len();

                let inner = self.resolve_seq(sb.with_scope(&body_scope), &node.block);

                let vars_up: Vec<_> = vars_up_inner.into_iter().map(|(snk, outer_expr, span)| {
                    (self.take_upvalue(snk, &sb.scope.file, span), outer_expr)
                }).collect();

                if self.upvalues.len() > upvalues_scope {
                    panic!("Upvalues set in for loop: {:?}", self.upvalues);
                }

                // up_value_src may set more upvalues, so this needs to happen after taking the body upvalues
                // and checking that the body didn't set any others.
                let vars_up: Vec<(ExprDn, ValueSrcId)> = vars_up.into_iter().map(|(val, outer_expr)| {
                    let src = self.up_value_src(&outer_expr);
                    (val, src)
                }).collect();

                let iters = count.unwrap_or(0) as u32;
                self.add_step(Step::Foreach { iters, inner, dn: vars_dn, up: vars_up })
            }

            ast::Action::Alt(ref node) => {
                let dir = constant::<Dir>(self.ui, sb.scope, &node.dir);
                let scrutinee = rexpr(self.ui, sb.scope, &node.expr);

                if node.arms.is_empty() {
                    let r = self.ui.report(Diagnostic::AltZeroArms{
                        span: Span::new(&sb.scope.file, node.span)
                    });
                    return self.add_step(Step::Invalid(r));
                }

                match dir {
                    Ok(Dir::Dn) => {
                        let scrutinee_dn = scrutinee.flatten(&mut |e| {
                            match e {
                                LeafItem::Value(v) => {
                                    self.require_down(&sb.scope, node.expr.span(), v)
                                }
                                LeafItem::Invalid(_) => ExprDn::invalid(),
                                t => {
                                    self.ui.report(Diagnostic::ExpectedValue {
                                        span: Span::new(&sb.scope.file, node.expr.span()),
                                        found: t.to_string()
                                    });
                                    ExprDn::invalid()
                                }
                            }
                        });
                        let arms = node.arms.iter().map(|arm| {
                            let mut body_scope = sb.scope.child();
                            let mut vals = Vec::new();
                            self.bind_tree_fields_dn(&mut body_scope, &scrutinee, &arm.discriminant, &mut vals).ok();

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
                    Ok(Dir::Up) => {
                        let scrutinee_src = scrutinee.flatten(&mut |e| {
                            match e {
                                LeafItem::Value(v) => self.up_value_src(&v),
                                LeafItem::Invalid(_) => self.add_value_src(),
                                t => {
                                    self.ui.report(Diagnostic::ExpectedValue {
                                        span: Span::new(&sb.scope.file, node.expr.span()),
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
                            self.bind_tree_fields_up(&mut body_scope, &ty, &arm.discriminant, &mut up_inner).ok();

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
        rhs: &Item,
        pat: &ast::Expr,
        dn: &mut Vec<Predicate>,
    ) -> Result<(), ErrorReported> {
        zip_ast(self.ui, &scope.file.clone(), pat, rhs, &mut |pat, r| {
            match (pat, r) {
                (pat, Tree::Leaf(LeafItem::Value(r))) => {
                    match lvalue_dn(self.ui, scope, pat, r.clone()){
                        Ok(p) => { dn.push(p); Ok(()) }
                        Err(r) => { dn.push(Predicate::Any); Err(r) }
                    }
                }
                (ast::Expr::Ignore(_), t) => {
                    t.for_each(&mut |_| {
                        dn.push(Predicate::Any)
                    });
                    Ok(())
                }
                (ast::Expr::Var(ref name), t @ Tree::Tuple(_)) => {
                    t.for_each(&mut |_| {
                        dn.push(Predicate::Any)
                    });
                    scope.bind(&name.name, t.clone());
                    Ok(())
                }
                (_, Tree::Leaf(LeafItem::Invalid(r))) => {
                    dn.push(Predicate::Any);
                    Err(r.clone())
                }
                (pat, e) => {
                    dn.push(Predicate::Any);
                    Err(self.ui.report(Diagnostic::InvalidItemForPattern {
                        span: Span::new(&scope.file, pat.span()),
                        found: e.to_string(),
                    }))
                }
            }  
        })
    }

    fn bind_tree_fields_up(
        &mut self,
        scope: &mut Scope,
        rhs: &TypeTree,
        pat: &ast::Expr,
        up: &mut Vec<LValueSrc>,
    ) -> Result<(), ErrorReported> {
        zip_ast(self.ui, &scope.file.clone(), pat, rhs, &mut |pat, r| {
            match (pat, r) {
                (pat, Tree::Leaf(t)) => {
                    match lvalue_up(self.ui, scope, pat, t.clone(), &mut || self.add_value_sink()) {
                        Ok(x) => { up.push(x); Ok(()) },
                        Err(r) => { up.push(LValueSrc::Val(ExprDn::invalid())); Err(r) }
                    }
                }
                (ast::Expr::Var(ref name), tup @ Tree::Tuple(_)) => {
                    let e = tup.map_leaf(&mut |ty| {
                        let id = self.add_value_sink();
                        up.push(LValueSrc::Var(id, name.span));
                        LeafItem::Value(Expr::var_up(id, ty.clone()))
                    });
                    scope.bind(&name.name, e);
                    Ok(())
                }
                (pat, ty) => {
                    up.push(LValueSrc::Val(ExprDn::invalid()));
                    Err(self.ui.report(Diagnostic::InvalidItemForPattern {
                        span: Span::new(&scope.file, pat.span()),
                        found: ty.to_string(),
                    }))
                }
            }
        })
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
                        (self.add_step(Step::TokenTransaction { variant: msg_def.tag, inner: None }), msg_def.child.clone() )
                    }
                } else {
                    let args: Vec<Item> = node.args.items.iter().map(|a| rexpr(self.ui, sb.scope, a)).collect();
                    let (scope, imp) = match self.index.find_def(sb.shape_down, &node.name.name, args) {
                        Ok(res) => res,
                        Err(FindDefError::NoDefinitionWithName) => {
                            let r = self.ui.report(Diagnostic::NoDefNamed {
                                span: Span::new(&sb.scope.file, node.name.span),
                                protocol_name: sb.shape_down.def.ast().name.name.to_owned(),
                                def_name: node.name.name.to_owned(),
                            });
                            return (self.add_step(Step::Invalid(r)), None)
                        }
                    };
                    self.resolve_process(sb.with_scope(&scope), imp)
                }
            }

            ast::Process::Child(node) => {
                let Some(child_shape) = sb.shape_down.child_named(&node.name.name) else {
                    let r = self.ui.report(Diagnostic::NoDefNamed {
                        span: Span::new(&sb.scope.file, node.name.span),
                        protocol_name: sb.shape_down.def.ast().name.name.to_owned(),
                        def_name: node.name.name.to_owned(),
                    });
                    return (self.add_step(Step::Invalid(r)), None)
                };

                (self.add_step(Step::Pass), Some(child_shape.clone()))
            }

            ast::Process::Primitive(node) => {
                let arg = rexpr_tup(self.ui, sb.scope, &node.args);
                debug!("instantiating primitive {} with {}", node.name.name, arg);
                let prim = match instantiate_primitive(&node.name, arg, sb.shape_down, sb.shape_up) {
                    Ok(p) => p,
                    Err(msg) => {
                        let r = self.ui.report(Diagnostic::ErrorInPrimitiveProcess {
                            span: Span::new(&sb.scope.file, node.span),
                            msg
                        });
                        return (self.add_step(Step::Invalid(r)), None);

                    }
                };
                (self.add_step(Step::Primitive(prim)), None)
            }

            ast::Process::New(node) => {
                let top_shape = match protocol::resolve(self.ui, self.index, sb.scope, &node.top, 0) {
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
                        let r = self.ui.report(Diagnostic::StackWithoutBaseSignal {
                            span: Span::new(&sb.scope.file, node.lower.span()),
                        });
                        return (self.add_step(Step::Invalid(r)), None)
                    }
                };

                let (hi, shape_up) = self.resolve_process(sb.with_lower(sb.scope, &shape), &node.upper);

                match self.steps[lo] {
                    Step::Pass => (hi, shape_up),
                    Step::TokenTransaction { variant, inner } => {
                        assert!(inner.is_none());
                        let id = self.add_step(Step::TokenTransaction { variant, inner: Some(hi) });
                        (id, shape_up)
                    }
                    _ => {
                        let stack = self.add_step(Step::Stack { lo, shape, hi });
                        (stack, shape_up)
                    }
                }
            }
        }
    }

    pub fn resolve_token(&mut self, msg_def: &ShapeMsg, scope: &Scope, node: &ast::ProcessCall) -> (Vec<ExprDn>, Vec<(Predicate, ValueSrcId)>) {
        if node.args.items.len() != msg_def.params.len() {
            self.ui.report(Diagnostic::ArgsMismatchCount {
                span: Span::new(&scope.file, node.args.span),
                def_name: node.name.name.clone(),
                expected: msg_def.params.len(),
                found: node.args.items.len(),
            });
        }

        let mut dn = Vec::new();
        let mut up = Vec::new();

        for (param, arg_ast) in msg_def.params.iter().zip(node.args.items.iter()) {
            let arg = rexpr(self.ui, scope, arg_ast);

            let mut valid = true;
            param.ty.zip(&arg, &mut |m| { match m {
                crate::tree::Zip::Both(
                    &Tree::Leaf(ref ty),
                    &Item::Leaf(LeafItem::Value(ref v))
                ) if v.get_type().is_subtype(ty) => {
                    match param.direction {
                        Dir::Dn => {
                            dn.push(self.require_down(scope, arg_ast.span(), v));
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
                self.ui.report(Diagnostic::ArgMismatchType {
                    span: Span::new(&scope.file, arg_ast.span()),
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
            resolve_letdef(self.ui,&mut scope, &ld);
        }

        let steps = block.actions.iter().map(|action| {
            self.resolve_action(sb.with_scope(&scope), &action)
        }).collect();

        self.add_step(Step::Seq(steps))
    }
}


pub fn resolve_letdef(ui: &dyn DiagnosticHandler, scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef { ref name, ref expr, .. } = ld;
    let item = rexpr(ui, scope, expr);
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

pub fn compile_process(ui: &dyn DiagnosticHandler, index: &Index, scope: &Scope, shape_dn: Shape, ast: &ast::Process) -> ProcessChain {
    let mut builder = Builder::new(ui, index);
    let sb = ResolveCx { scope, shape_up: None, shape_down: &shape_dn };
    let (step, shape_up) = builder.resolve_process(sb, ast);

    let step_info = analyze_unambiguous(&builder.steps);

    ProcessChain {
        steps: builder.steps,
        vars: builder.value_src,
        step_info,
        root: step,
        shape_dn,
        shape_up,
    }
}
