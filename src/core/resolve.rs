use crate::Value;
use crate::{PrimitiveProcess, syntax::ast};
use super::{Ctxt, Expr, ExprDn, Item, Scope, Shape, Type, ValueId, index::DefImpl, ShapeMsg, resolve_protocol_invoke};
use super::{Dir, on_expr_message, pattern_match, rexpr, value};

#[derive(Clone, Copy)]
struct ResolveCx<'a> {
    ctx: &'a Ctxt<'a>,
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

pub trait Builder {
    type Res;

    //fn process(&mut self) -> Self::Res;
    fn token(&mut self, variant: usize, dn: Vec<ExprDn>, up: Vec<Expr>) -> Self::Res;
    fn token_top(&mut self, top_dir: Dir, variant: usize, dn: Vec<Expr>, up: Vec<ExprDn>, inner: Self::Res) -> Self::Res;
    fn stack(&mut self, lo: Self::Res, shape: Shape, hi: Self::Res) -> Self::Res;
    fn primitive(&mut self, prim: Box<dyn PrimitiveProcess + 'static>) -> Self::Res;
    fn seq(&mut self, steps: Vec<Self::Res>) -> Self::Res;
    fn repeat(&mut self, dir: Dir, count: Expr, inner: Self::Res) -> Self::Res;
    fn foreach(&mut self, length: u32, vars: Vec<(ValueId, Expr)>, inner: Self::Res) -> Self::Res;
    fn alt(&mut self, dir: Dir, opts: Vec<(Vec<(Expr, Expr)>, Self::Res)>) -> Self::Res;
}

pub fn resolve_dir(e: Expr) -> Dir {
    match e.eval_const() {
        Value::Symbol(s) if s == "up" => Dir::Up,
        Value::Symbol(s) if s == "dn" => Dir::Dn,
        e => panic!("Invalid direction: {}", e)
    }
}


fn resolve_action<B: Builder>(sb: ResolveCx<'_>, builder: &mut B, action: &ast::Action) -> B::Res {
    match *action {
        ast::Action::Process(ref processes) => {
            let (step, shape_up) = resolve_processes(sb, builder, processes);
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

            let (dn, up) = on_expr_message(sb.ctx, &mut body_scope, msg_def, exprs);

            let step = if let &Some(ref body) = body {
                resolve_seq(sb.with_upper(&body_scope, None), builder, body)
            } else {
                builder.seq(Vec::new())
            };

            builder.token_top(shape_up.dir, variant, dn, up, step)
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

            let inner = resolve_seq(sb, builder, block);
            builder.repeat(dir, count, inner)
        }
        ast::Action::For(ref pairs, ref block) => {
            let mut body_scope = sb.scope.child();
            let mut count = None;
            let mut inner_vars = Vec::with_capacity(pairs.len());

            for &(ref name, ref expr) in pairs {
                let e = value(sb.scope, expr);
                let t = e.get_type();
                let dir = e.dir();
                if let Type::Vector(c, ty) = t {
                    match count {
                        Some(count) => assert_eq!(count, c),
                        None => count = Some(c),
                    }
                    let id = body_scope.new_variable(sb.ctx, name, *ty, dir);
                    inner_vars.push((id, e));
                } else {
                    panic!("Foreach must loop over vector type, not {:?}", t)
                }
            }

            let step = resolve_seq(sb.with_scope(&body_scope), builder, block);

            builder.foreach(count.unwrap_or(0) as u32, inner_vars, step)
        }
        ast::Action::Alt(ref dir_ast, ref expr, ref arms) => {
            let dir = resolve_dir(value(sb.scope, dir_ast));
            let r = rexpr(sb.scope, expr);

            let v = arms.iter().map(|arm| {
                let mut body_scope = sb.scope.child();
                let mut checks = Vec::new();
                pattern_match(sb.ctx, &mut body_scope, &arm.discriminant, &r, &mut checks);
                let step = resolve_seq(sb.with_scope(&body_scope), builder, &arm.block);
                (checks, step)
            }).collect();

            builder.alt(dir, v)
        }
    }
}

fn resolve_processes<B: Builder>(sb: ResolveCx<'_>, builder: &mut B, processes: &[ast::Process]) -> (B::Res, Option<Shape>) {
    let (mut res, mut top_shape) = resolve_process(sb, builder, &processes[0]);

    for process_ast in &processes[1..] {
        let shape = top_shape.unwrap();
        let (step, shape_up) = resolve_process(sb.with_lower(sb.scope, &shape), builder, process_ast);
        res = builder.stack(res, shape, step);
        top_shape = shape_up;
    }

     (res, top_shape)
}

fn resolve_process<B: Builder>(sb: ResolveCx<'_>, builder: &mut B, process_ast: &ast::Process) -> (B::Res, Option<Shape>) {
    match process_ast {
        ast::Process::Call(ref name, ref args_ast) => {
            let args: Vec<Item> = args_ast.iter().map(|a| rexpr(sb.scope, a)).collect();
            if let Some((variant, msg_def)) = sb.shape_down.variant_named(name) {
                let (dn, up) = resolve_token(msg_def, &args);
                (builder.token(variant, dn, up), None)
            } else {
                let (scope, imp) = sb.ctx.index.find_def(sb.shape_down, name, args);
                match *imp {
                    DefImpl::Code(ref callee_ast) => {
                        resolve_processes(sb.with_scope(&scope), builder, callee_ast)
                    }
                    DefImpl::Primitive(ref primitive, ref shape_up_ast) => {
                        let shape_up = shape_up_ast.as_ref().map(|s| resolve_protocol_invoke(sb.ctx, &scope, s));
                        let prim = primitive.instantiate(&scope);
                        (builder.primitive(prim), shape_up)
                    }
                }
            }
        }

        ast::Process::Seq(ref top_shape, ref block) => {
            let top_shape = resolve_protocol_invoke(sb.ctx, &sb.scope, top_shape);
            let block = resolve_seq(sb.with_upper(&sb.scope, Some(&top_shape)), builder, block);
            (block, Some(top_shape))
        }

        ast::Process::InferSeq(ref block) => {
            let block = resolve_seq(sb.with_upper(&sb.scope, None), builder, block);
            (block, None)
        }
    }
}

fn resolve_seq<B: Builder>(sb: ResolveCx<'_>, builder: &mut B, block: &ast::Block) -> B::Res {
    let mut scope = sb.scope.child();

    for ld in &block.lets {
        resolve_letdef(&mut scope, ld);
    }

    let steps = block.actions.iter().map(|action| {
        resolve_action(sb.with_scope(&scope), builder, action)
    }).collect();

    builder.seq(steps)
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

use super::step::{StepBuilder, StepInfo};

pub struct ProcessChain {
    pub step: StepInfo,
    pub shape_up: Option<Shape>,
}

pub fn compile_process_chain(ctx: &Ctxt, scope: &Scope, shape_down: &Shape, ast: &[ast::Process]) -> ProcessChain {
    let builder = &mut StepBuilder;
    let sb = ResolveCx { ctx, scope, shape_up: None, shape_down};
    let (step, shape_up) = resolve_processes(sb, builder, ast);
    ProcessChain { step, shape_up }
}