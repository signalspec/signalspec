use crate::entitymap::{ EntityMap };
use crate::{Value, Index, DiagnosticHandler, DiagnosticKind, Label};
use crate::syntax::ast;
use super::index::FindDefError;
use super::step::{Step, StepInfo, analyze_unambiguous};
use super::{Expr, ExprDn, Item, Scope, Shape, Type, index::DefImpl, ShapeMsg, protocol};
use super::{Dir, on_expr_message, pattern_match, rexpr, value, VarId, StepId, LeafItem};

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
    vars: EntityMap<VarId, ()>,
}

impl<'a> Builder<'a> {
    pub fn new(ui: &'a dyn DiagnosticHandler, index: &'a Index) -> Self {
        Self { ui, index, steps: EntityMap::new(), vars: EntityMap::new() }
    }

    fn add_step(&mut self, step: Step) -> StepId {
        self.steps.push(step)
    }

    fn add_var(&mut self) -> VarId {
        self.vars.push(())
    }

    fn resolve_action(&mut self, sb: ResolveCx<'_>, action: &ast::Action) -> StepId {
        match action {
            ast::Action::Process(ref node) => {
                let (step, shape_up) = self.resolve_processes(sb, &node.processes);
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

                let (dn, up) = on_expr_message(|| self.add_var(), &mut body_scope, msg_def, &node.args.items);

                let inner = if let &Some(ref body) = &node.block {
                    self.resolve_seq(sb.with_upper(&body_scope, None), body)
                } else {
                    self.add_step(Step::Seq(vec![]))
                };

                self.add_step(Step::TokenTop { top_dir: shape_up.dir, variant, send: dn, receive: up, inner })
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

                let inner = self.resolve_seq(sb, &node.block);

                self.add_step(match dir {
                    Dir::Up => Step::RepeatUp(count, inner),
                    Dir::Dn => Step::RepeatDn(count.down(), inner),
                })
            }

            ast::Action::For(ref node) => {
                let mut body_scope = sb.scope.child();
                let mut count = None;

                let mut vars_dn = Vec::new();
                let mut vars_up = Vec::new();

                for &(ref name, ref expr) in &node.vars {
                    let e = value(sb.scope, expr);
                    let t = e.get_type();
                    let dir = e.dir();
                    if let Type::Vector(c, ty) = t {
                        match count {
                            Some(count) => assert_eq!(count, c),
                            None => count = Some(c),
                        }

                        let id = self.add_var();
                        body_scope.bind(&name.name, Item::Leaf(LeafItem::Value(Expr::Variable(id, *ty, dir))));

                        if dir.down {
                            vars_dn.push((id, e.down()));
                        } else if dir.up {
                            vars_up.push((id, e));
                        }
                    } else {
                        panic!("Foreach must loop over vector type, not {:?}", t)
                    }
                }

                let inner = self.resolve_seq(sb.with_scope(&body_scope), &node.block);

                let iters = count.unwrap_or(0) as u32;
                self.add_step(Step::Foreach { iters, vars_dn, vars_up, inner })
            }

            ast::Action::Alt(ref node) => {
                let dir = resolve_dir(value(sb.scope, &node.dir));
                let r = rexpr(sb.scope, &node.expr);

                let v: Vec<_> = node.arms.iter().map(|arm| {
                    let mut body_scope = sb.scope.child();
                    let mut checks = Vec::new();
                    pattern_match(&mut body_scope, &arm.discriminant, &r, &mut checks);
                    let step = self.resolve_seq(sb.with_scope(&body_scope), &arm.block);
                    (checks, step)
                }).collect();

                self.add_step(match dir {
                     Dir::Up => {
                        let opts = v.into_iter().map(|(e, b)|
                            (e.into_iter().map(|(l, r)| (l.down(), r)).collect(), b)
                        ).collect();
                        Step::AltUp(opts)
                     }
                     Dir::Dn => {
                         let opts = v.into_iter().map(|(e, b)|
                            (e.into_iter().map(|(l, r)| (l, r.down())).collect(), b)
                        ).collect();
                        Step::AltDn(opts)
                     }
                })
            }
            ast::Action::Error(_) => panic!("Syntax error"),
        }
    }

    fn resolve_processes(&mut self, sb: ResolveCx<'_>, processes: &[ast::Process]) -> (StepId, Option<Shape>) {
        let (mut res, mut top_shape) = self.resolve_process(sb, &processes[0]);

        for process_ast in &processes[1..] {
            let shape = top_shape.unwrap();
            let (step, shape_up) = self.resolve_process(sb.with_lower(sb.scope, &shape), process_ast);
            res = self.add_step(Step::Stack { lo: res, shape, hi: step });
            top_shape = shape_up;
        }

         (res, top_shape)
    }

    fn resolve_process(&mut self, sb: ResolveCx<'_>, process_ast: &ast::Process) -> (StepId, Option<Shape>) {
        match process_ast {
            ast::Process::Call(node) => {
                let args: Vec<Item> = node.args.items.iter().map(|a| rexpr(sb.scope, a)).collect();
                if let Some((variant, msg_def)) = sb.shape_down.variant_named(&node.name.name) {
                    let (dn, up) = resolve_token(msg_def, &args);
                    (self.add_step(Step::Token { variant, send: dn, receive: up }), None)
                } else {
                    let (scope, imp) = match self.index.find_def(sb.shape_down, &node.name.name, args) {
                        Ok(res) => res,
                        Err(FindDefError::NoDefinitionWithName) => {
                            self.ui.report(DiagnosticKind::NoDefNamed {
                                protocol_name: sb.shape_down.def.ast().name.name.to_owned(),
                                def_name: node.name.name.to_owned(),
                            }, vec![
                                Label { file: sb.scope.file.clone(), span: node.name.span, label: "not found".into() }
                            ]);
                            return (self.add_step(Step::Invalid), None)
                        }
                    };
                    match *imp {
                        DefImpl::Code(ref callee_ast) => {
                            self.resolve_processes(sb.with_scope(&scope), callee_ast)
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
        }
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

pub fn resolve_token(msg_def: &ShapeMsg, args: &[Item]) -> (Vec<ExprDn>, Vec<Expr>) {
    assert_eq!(args.len(), msg_def.params.len(), "wrong number of arguments to message {:?} {:?} {:?}", msg_def.name, msg_def.params, args);

    let mut dn = Vec::new();
    let mut up = Vec::new();

    for (param, arg) in msg_def.params.iter().zip(args) {
        let out = if param.direction.up {
            &mut up
        } else if param.direction.down {
            &mut dn
        } else {
            unreachable!();
        };

        use crate::tree::Zip::*;
        param.item.zip(arg, &mut |m| { match m {
            Both(&Item::Leaf(LeafItem::Value(Expr::Const(_))), _) => (),
            Both(&Item::Leaf(LeafItem::Value(_)), &Item::Leaf(LeafItem::Value(ref v))) => {
                out.push(v.clone());
            },
            _ => panic!("Invalid pattern")
        }})
    }

    (dn.into_iter().map(|x| x.down()).collect(), up)
}

pub struct ProcessChain {
    pub steps: EntityMap<StepId, Step>,
    pub root: StepId,
    pub vars: EntityMap<VarId, ()>,
    pub step_info: EntityMap<StepId, StepInfo>,
    pub shape_dn: Shape,
    pub shape_up: Option<Shape>,
}

pub fn compile_process_chain(ui: &dyn DiagnosticHandler, index: &Index, scope: &Scope, shape_dn: Shape, ast: &[ast::Process]) -> ProcessChain {
    let mut builder = Builder::new(ui, index);
    let sb = ResolveCx { scope, shape_up: None, shape_down: &shape_dn };
    let (step, shape_up) = builder.resolve_processes(sb, ast);

    let step_info = analyze_unambiguous(&builder.steps);

    ProcessChain {
        steps: builder.steps,
        vars: builder.vars,
        step_info,
        root: step,
        shape_dn,
        shape_up,
    }
}
