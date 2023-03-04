use num_traits::Signed;

use crate::core::expr_resolve::bind_tree_fields;
use crate::diagnostic::Span;
use crate::entitymap::{ EntityMap, entity_key };
use crate::tree::Tree;
use crate::{Value, Index, DiagnosticHandler, Diagnostic};
use crate::syntax::ast;
use super::index::FindDefError;
use super::step::{Step, StepInfo, analyze_unambiguous, AltUpArm, AltDnArm};
use super::{Expr, ExprDn, Item, Scope, Shape, Type, index::DefImpl, ShapeMsg, protocol};
use super::{Dir, rexpr, value, StepId, LeafItem, Predicate, ValueSrcId};

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

pub fn resolve_dir(e: Expr) -> Dir {
    match e.eval_const() {
        Value::Symbol(s) if s == "up" => Dir::Up,
        Value::Symbol(s) if s == "dn" => Dir::Dn,
        e => panic!("Invalid direction: {}", e)
    }
}

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

    pub fn take_upvalue(&mut self, snk: ValueSinkId) -> ExprDn {
        let mut i = 0;
        let mut ret = None;
        while i < self.upvalues.len() {
            if self.upvalues[i].0 == snk {
                if ret.is_some() {
                    panic!("Upvalue {snk:?} was set multiple times");
                }
                ret = Some(self.upvalues.remove(i).1);
            } else {
                i += 1;
            }
        }
        ret.expect("Upvalue was not set")
    }

    fn resolve_action(&mut self, sb: ResolveCx<'_>, action: &ast::Action) -> StepId {
        match action {
            ast::Action::Process(ref node) => {
                let (step, shape_up) = self.resolve_process(sb, &node);
                assert!(shape_up.is_none());
                step
            }

            ast::Action::On(ref node) => {
                let mut body_scope = sb.scope.child();

                debug!("Upper message, shape: {:?}", sb.shape_up);

                let shape_up = sb.shape_up.expect("`on` block with no upper shape");

                let (variant, msg_def) = if let Some(t) = shape_up.variant_named(&node.name.name) { t } else {
                    panic!("Variant {:?} not found on shape {:?}", node.name.name, shape_up)
                };

                assert_eq!(msg_def.params.len(), node.args.items.len());

                enum UpInner {
                    Val(ExprDn),
                    Var(ValueSinkId),
                }

                let mut dn = Vec::new();
                let mut up_inner = Vec::new();

                for (param, expr) in msg_def.params.iter().zip(&node.args.items) {
                    match param.direction {
                        Dir::Dn => {
                            bind_tree_fields(expr, &param.ty, &mut body_scope, (&mut *self, &mut dn),
                                |(slf, dn), ty| {
                                    let id = slf.add_value_src();
                                    dn.push((Predicate::Any, id));
                                    Expr::VarDn(id, ty.clone())
                                },
                                |&mut (ref mut slf, ref mut dn), e| {
                                    dn.push((e.predicate().expect("on field is not a predicate"), slf.add_value_src()))
                                }
                            );
                        }
                        Dir::Up => {
                            bind_tree_fields(expr, &param.ty, &mut body_scope, (&mut *self, &mut up_inner),
                                |(slf, up_inner), ty| {
                                    let id = slf.add_value_sink();
                                    up_inner.push(UpInner::Var(id));
                                    Expr::VarUp(id, ty.clone())
                                },
                                |(_, up_inner), e| {
                                    up_inner.push(UpInner::Val(e.down().unwrap()));
                                }
                            );
                        }
                    };
                }

                let inner = if let &Some(ref body) = &node.block {
                    self.resolve_seq(sb.with_upper(&body_scope, None), body)
                } else {
                    self.add_step(Step::Seq(vec![]))
                };

                let up = up_inner.into_iter().map(|v| {
                    match v {
                        UpInner::Var(snk) => self.take_upvalue(snk),
                        UpInner::Val(e) => e,
                    }
                }).collect();

                self.add_step(Step::TokenTop { top_dir: shape_up.dir, variant, dn, up, inner })
            }

            ast::Action::Repeat(ref node) => {
                let (dir, count) = match &node.dir_count {
                    Some((dir_ast, count_ast)) => {
                        let count = value(sb.scope, count_ast);
                        let dir = resolve_dir(value(sb.scope, dir_ast));
                        (dir, count)
                    }
                    None => (Dir::Up, Expr::Ignored)
                };

                let upvalues_scope = self.upvalues.len();

                let inner = self.resolve_seq(sb, &node.block);

                if self.upvalues.len() > upvalues_scope {
                    panic!("Upvalues set in repeat loop");
                }

                match dir {
                    Dir::Up => {
                        let (min, max) = match count.predicate() {
                            Some(Predicate::Any) => (0, None),
                            Some(Predicate::Number(n)) if n.is_integer() && !n.is_negative() => (*n.numer(), Some(*n.numer() + 1)),
                            Some(Predicate::Range(min, max)) if min.is_integer() && !min.is_negative() && max.is_integer() => (*min.numer(), Some(*max.numer())),
                            _ => panic!("Invalid repeat count {count}")
                        };
                        let count_src = self.up_value_src(&count);
                        self.add_step(Step::RepeatUp { min, max, inner, count: count_src })
                    }
                    Dir::Dn => {
                        self.add_step(Step::RepeatDn { count: count.down().unwrap(), inner })
                    }
                }
            }

            ast::Action::For(ref node) => {
                let mut body_scope = sb.scope.child();
                let mut count = None;

                let mut vars_dn: Vec<(ExprDn, ValueSrcId)> = Vec::new();
                let mut vars_up_inner:Vec<(ValueSinkId, Expr)> = Vec::new();
                
                for &(ref name, ref expr) in &node.vars {
                    let e = value(sb.scope, expr);
                    let t = e.get_type();
                    if let Type::Vector(c, ty) = t {
                        match count {
                            Some(count) => assert_eq!(count, c),
                            None => count = Some(c),
                        }
                        
                        let value = if let Some(e_dn) = e.down() {
                            let id = self.add_value_src(); // The element of the vector inside the loop
                            vars_dn.push((e_dn, id));
                            Expr::VarDn(id, *ty)
                        } else {
                            let id = self.add_value_sink();
                            vars_up_inner.push((id, e));
                            Expr::VarUp(id, *ty)
                        };
                        
                        body_scope.bind(&name.name, Item::Leaf(LeafItem::Value(value)));
                    } else {
                        panic!("Foreach must loop over vector type, not {:?}", t)
                    }
                }

                let upvalues_scope = self.upvalues.len();

                let inner = self.resolve_seq(sb.with_scope(&body_scope), &node.block);

                let vars_up: Vec<_> = vars_up_inner.into_iter().map(|(snk, outer_expr)| {
                    (self.take_upvalue(snk), outer_expr)
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
                let dir = resolve_dir(value(sb.scope, &node.dir));
                let scrutinee = rexpr(sb.scope, &node.expr);

                match dir {
                    Dir::Dn => {
                        let scrutinee_dn = scrutinee.flatten(&mut |e| {
                            match e {
                                LeafItem::Value(v) => v.down().expect("can't be down-evaluated"),
                                t => panic!("{t:?} not allowed in alt")
                            }
                        });
                        let arms = node.arms.iter().map(|arm| {
                            let mut body_scope = sb.scope.child();
                            let mut vals = Vec::new();

                            bind_tree_fields(&arm.discriminant, &scrutinee, &mut body_scope, &mut vals,
                                |vals, t| {
                                    vals.push(Predicate::Any);
                                    match t {
                                        LeafItem::Value(v) => v.clone(),
                                        t => panic!("{t:?} not allowed in alt")
                                    }
                                },
                                |vals, e| {
                                    vals.push(e.predicate().expect("alt arm {:?} is not a valid predicate"))
                                }
                            );

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
                    Dir::Up => {
                        let scrutinee_src = scrutinee.flatten(&mut |e| {
                            match e {
                                LeafItem::Value(v) => self.up_value_src(&v),
                                t => panic!("{t:?} not allowed in alt")
                            }
                        });
                        let arms = node.arms.iter().map(|arm| {
                            let mut body_scope = sb.scope.child();

                            enum UpInner {
                                Val(ExprDn),
                                Var(ValueSinkId),
                            }
                            
                            let mut up_inner = Vec::new();
                            bind_tree_fields(&arm.discriminant, &scrutinee, &mut body_scope, (&mut *self, &mut up_inner),
                                |(slf, up_inner), t| {
                                    let ty = match t {
                                        LeafItem::Value(v) => v.get_type(),
                                        t => panic!("{t:?} not allowed in alt")
                                    };
                                    let id = slf.add_value_sink();
                                    up_inner.push(UpInner::Var(id));
                                    Expr::VarUp(id, ty)
                                },
                                |(_, up_inner), e| {
                                    let Some(e) = e.down() else {
                                        panic!("{e:?} cannot be down-evaluated in alt");
                                    };
                                    up_inner.push(UpInner::Val(e))
                                }
                            );

                            let upvalues_scope = self.upvalues.len();
                            let body = self.resolve_seq(sb.with_scope(&body_scope), &arm.block);

                            let vals = up_inner.into_iter().map(|v| {
                                match v {
                                    UpInner::Var(snk) => self.take_upvalue(snk),
                                    UpInner::Val(e) => e,
                                }
                            }).collect();

                            if self.upvalues.len() > upvalues_scope {
                                // TODO: could support if all arms set the same upvalues, and we insert phi nodes to join them
                                panic!("Upvalues set in alt arm");
                            }
                            AltUpArm { vals, body }
                        }).collect();
                        self.add_step(Step::AltUp(arms, scrutinee_src))
                    }
                }
            }
            ast::Action::Error(_) => panic!("Syntax error"),
        }
    }

    fn resolve_process(&mut self, sb: ResolveCx<'_>, process_ast: &ast::Process) -> (StepId, Option<Shape>) {
        match process_ast {
            ast::Process::Call(node) => {
                let args: Vec<Item> = node.args.items.iter().map(|a| rexpr(sb.scope, a)).collect();
                if let Some((variant, msg_def)) = sb.shape_down.variant_named(&node.name.name) {
                    let (dn, up) = self.resolve_token(msg_def, &args);
                    (self.add_step(Step::Token { variant, dn, up }), None)
                } else {
                    let (scope, imp) = match self.index.find_def(sb.shape_down, &node.name.name, args) {
                        Ok(res) => res,
                        Err(FindDefError::NoDefinitionWithName) => {
                            self.ui.report(Diagnostic::NoDefNamed {
                                span: Span::new(&sb.scope.file, node.name.span),
                                protocol_name: sb.shape_down.def.ast().name.name.to_owned(),
                                def_name: node.name.name.to_owned(),
                            });
                            return (self.add_step(Step::Invalid), None)
                        }
                    };
                    match *imp {
                        DefImpl::Code(ref callee_ast) => {
                            self.resolve_process(sb.with_scope(&scope), callee_ast)
                        }
                        DefImpl::Primitive(ref primitive, ref shape_up_ast) => {
                            let shape_up = shape_up_ast.as_ref().map(|s| protocol::resolve(self.index, &scope, s));
                            let prim = primitive.instantiate(&scope);
                            (self.add_step(Step::Primitive(prim)), shape_up)
                        }
                    }
                }
            }

            ast::Process::Seq(node) => {
                let top_shape = protocol::resolve(self.index, sb.scope, &node.top);
                let block = self.resolve_seq(sb.with_upper(sb.scope, Some(&top_shape)), &node.block);
                (block, Some(top_shape))
            }

            ast::Process::InferSeq(ref block) => {
                let block = self.resolve_seq(sb.with_upper(sb.scope, None), block);
                (block, None)
            }

            ast::Process::Stack(node) => {
                let (lo, shape) = self.resolve_process(sb, &node.lower);
                let shape = shape.unwrap();
                let (hi, shape_up) = self.resolve_process(sb.with_lower(sb.scope, &shape), &node.upper);
                let stack = self.add_step(Step::Stack { lo, shape, hi });
                (stack, shape_up)
            }
        }
    }

    pub fn resolve_token(&mut self, msg_def: &ShapeMsg, args: &[Item]) -> (Vec<ExprDn>, Vec<(Predicate, ValueSrcId)>) {
        assert_eq!(args.len(), msg_def.params.len(), "wrong number of arguments to message {:?} {:?} {:?}", msg_def.name, msg_def.params, args);

        let mut dn = Vec::new();
        let mut up = Vec::new();

        for (param, arg) in msg_def.params.iter().zip(args) {
            let out = match param.direction {
                Dir::Up => &mut up,
                Dir::Dn => &mut dn,
            };

            use crate::tree::Zip::*;
            param.ty.zip(arg, &mut |m| { match m {
                Both(&Tree::Leaf(_), &Item::Leaf(LeafItem::Value(ref v))) => {
                    out.push(v.clone());
                },
                _ => panic!("Invalid pattern")
            }})
        }

        let dn = dn.into_iter().map(|x| x.down().unwrap()).collect();
        let up = up.into_iter().map(|x| {
            let predicate = x.predicate().expect("Value cannot be up-evaluated as a predicate");
            let src = self.up_value_src(&x);
            (predicate, src)
        }).collect();

        (dn, up)
    }

    fn resolve_seq(&mut self, sb: ResolveCx<'_>, block: &ast::Block) -> StepId {
        let mut scope = sb.scope.child();

        for ld in &block.lets {
            resolve_letdef(&mut scope, &ld);
        }

        let steps = block.actions.iter().map(|action| {
            self.resolve_action(sb.with_scope(&scope), &action)
        }).collect();

        self.add_step(Step::Seq(steps))
    }
}


pub fn resolve_letdef(scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef { ref name, ref expr, .. } = ld;
    let item = rexpr(scope, expr);
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
