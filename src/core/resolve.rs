use crate::Value;
use crate::syntax::ast;
use super::step::{Step, StepInfo, StepId, analyze_unambiguous};
use super::{Ctxt, Expr, ExprDn, Item, Scope, Shape, Type, index::DefImpl, ShapeMsg, resolve_protocol_invoke};
use super::{Dir, on_expr_message, pattern_match, rexpr, value};

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
    ctx: &'a Ctxt<'a>,
    steps: Vec<Step>,
}

impl<'a> Builder<'a> {
    pub fn new(ctx: &'a Ctxt<'a>) -> Self {
        Self { ctx, steps: vec![] }
    }

    fn add_step(&mut self, step: Step) -> StepId {
        let id = StepId(self.steps.len().try_into().expect("step id overflow"));
        self.steps.push(step);
        id
    }

    fn resolve_action(&mut self, sb: ResolveCx<'_>, action: &ast::Action) -> StepId {
        match *action {
            ast::Action::Process(ref processes) => {
                let (step, shape_up) = self.resolve_processes(sb, processes);
                assert!(shape_up.is_none());
                step
            }

            ast::Action::On(ref name, ref exprs, ref body) => {
                let mut body_scope = sb.scope.child();

                debug!("Upper message, shape: {:?}", sb.shape_up);

                let shape_up = sb.shape_up.expect("`on` block with no upper shape");

                let (variant, msg_def) = if let Some(t) = shape_up.variant_named(name) { t } else {
                    panic!("Variant {:?} not found on shape {:?}", name, shape_up)
                };

                let (dn, up) = on_expr_message(self.ctx, &mut body_scope, msg_def, exprs);

                let inner = if let &Some(ref body) = body {
                    self.resolve_seq(sb.with_upper(&body_scope, None), body)
                } else {
                    self.add_step(Step::Seq(vec![]))
                };

                self.add_step(Step::TokenTop { top_dir: shape_up.dir, variant, send: dn, receive: up, inner })
            }

            ast::Action::Repeat(ref count_ast, ref block) => {
                let (dir, count) = match count_ast {
                    Some((dir_ast, count_ast)) => {
                        let count = value(sb.scope, &count_ast.node);
                        let dir = resolve_dir(value(sb.scope, &dir_ast.node));
                        (dir, count)
                    }
                    None => (Dir::Up, Expr::Ignored)
                };

                let inner = self.resolve_seq(sb, block);

                self.add_step(match dir {
                    Dir::Up => Step::RepeatUp(count, inner),
                    Dir::Dn => Step::RepeatDn(count.down(), inner),
                })
            }

            ast::Action::For(ref pairs, ref block) => {
                let mut body_scope = sb.scope.child();
                let mut count = None;

                let mut vars_dn = Vec::new();
                let mut vars_up = Vec::new();

                for &(ref name, ref expr) in pairs {
                    let e = value(sb.scope, expr);
                    let t = e.get_type();
                    let dir = e.dir();
                    if let Type::Vector(c, ty) = t {
                        match count {
                            Some(count) => assert_eq!(count, c),
                            None => count = Some(c),
                        }
                        let id = body_scope.new_variable(self.ctx, name, *ty, dir);

                        if dir.down {
                            vars_dn.push((id, e.down()));
                        } else if dir.up {
                            vars_up.push((id, e));
                        }
                    } else {
                        panic!("Foreach must loop over vector type, not {:?}", t)
                    }
                }

                let inner = self.resolve_seq(sb.with_scope(&body_scope), block);

                let iters = count.unwrap_or(0) as u32;
                self.add_step(Step::Foreach { iters, vars_dn, vars_up, inner })
            }

            ast::Action::Alt(ref dir_ast, ref expr, ref arms) => {
                let dir = resolve_dir(value(sb.scope, dir_ast));
                let r = rexpr(sb.scope, expr);

                let v: Vec<_> = arms.iter().map(|arm| {
                    let mut body_scope = sb.scope.child();
                    let mut checks = Vec::new();
                    pattern_match(self.ctx, &mut body_scope, &arm.discriminant, &r, &mut checks);
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
            ast::Process::Call(ref name, ref args_ast) => {
                let args: Vec<Item> = args_ast.iter().map(|a| rexpr(sb.scope, a)).collect();
                if let Some((variant, msg_def)) = sb.shape_down.variant_named(name) {
                    let (dn, up) = resolve_token(msg_def, &args);
                    (self.add_step(Step::Token { variant, send: dn, receive: up }), None)
                } else {
                    let (scope, imp) = self.ctx.index.find_def(sb.shape_down, name, args);
                    match *imp {
                        DefImpl::Code(ref callee_ast) => {
                            self.resolve_processes(sb.with_scope(&scope), callee_ast)
                        }
                        DefImpl::Primitive(ref primitive, ref shape_up_ast) => {
                            let shape_up = shape_up_ast.as_ref().map(|s| resolve_protocol_invoke(self.ctx, &scope, s));
                            let prim = primitive.instantiate(&scope);
                            (self.add_step(Step::Primitive(prim)), shape_up)
                        }
                    }
                }
            }

            ast::Process::Seq(ref top_shape, ref block) => {
                let top_shape = resolve_protocol_invoke(self.ctx, &sb.scope, top_shape);
                let block = self.resolve_seq(sb.with_upper(&sb.scope, Some(&top_shape)), block);
                (block, Some(top_shape))
            }

            ast::Process::InferSeq(ref block) => {
                let block = self.resolve_seq(sb.with_upper(&sb.scope, None), block);
                (block, None)
            }
        }
    }

    fn resolve_seq(&mut self, sb: ResolveCx<'_>, block: &ast::Block) -> StepId {
        let mut scope = sb.scope.child();

        for ld in &block.lets {
            resolve_letdef(&mut scope, ld);
        }

        let steps = block.actions.iter().map(|action| {
            self.resolve_action(sb.with_scope(&scope), action)
        }).collect();

        self.add_step(Step::Seq(steps))
    }
}


pub fn resolve_letdef(scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef(ref name, ref expr) = ld;
    let item = rexpr(scope, expr);
    scope.bind(&name, item);
}

pub fn resolve_token(msg_def: &ShapeMsg, args: &[Item]) -> (Vec<ExprDn>, Vec<Expr>) {
    fn inner(i: &Item, shape: &Item, out: &mut Vec<Expr>) {
        match shape {
            &Item::Value(Expr::Const(_)) => (),
            &Item::Value(_) => {
                if let Item::Value(v) = i {
                    out.push(v.clone());
                } else {
                    panic!("Expected value but found {:?}", i);
                }
            }
            &Item::Tuple(ref m) => {
                if let Item::Tuple(t) = i {
                    if t.len() == m.len() {
                        for (mi, i) in m.iter().zip(t.into_iter()) {
                            inner(i, mi, out)
                        }
                    } else {
                        panic!("Expected tuple length {}, found {}", m.len(), t.len());
                    }
                } else {
                    panic!("Expected tuple of length {}, found {:?}", m.len(), i);
                }
            }
            _ => panic!("Item {:?} not allowed in shape", shape)
        }
    }

    assert_eq!(args.len(), msg_def.params.len(), "wrong number of arguments to message {:?} {:?} {:?}", msg_def.name, msg_def.params, args);

    let mut dn = Vec::new();
    let mut up = Vec::new();

    for (param, arg) in msg_def.params.iter().zip(args) {
        if param.direction.up {
            inner(arg, &param.item, &mut up)
        } else if param.direction.down {
            inner(arg, &param.item, &mut dn)
        }
    }

    (dn.into_iter().map(|x| x.down()).collect(), up)
}

pub struct ProcessChain {
    pub steps: Vec<Step>,
    pub root: StepId,
    pub step_info: Vec<StepInfo>,
    pub shape_dn: Shape,
    pub shape_up: Option<Shape>,
}

pub fn compile_process_chain(ctx: &Ctxt, scope: &Scope, shape_dn: Shape, ast: &[ast::Process]) -> ProcessChain {
    let mut builder = Builder::new(ctx);
    let sb = ResolveCx { scope, shape_up: None, shape_down: &shape_dn };
    let (step, shape_up) = builder.resolve_processes(sb, ast);

    let step_info = analyze_unambiguous(&builder.steps);

    ProcessChain {
        steps: builder.steps,
        step_info,
        root: step,
        shape_dn,
        shape_up,
    }
}
