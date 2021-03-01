use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use crate::{PrimitiveProcess, syntax::ast};
use super::{Ctxt, Expr, Item, MatchSet, Scope, Shape, Type, ValueId, index::DefImpl, resolve_protocol_invoke};
use super::{ on_expr_message, value, pattern_match, rexpr };

pub type Message = Vec<Option<Expr>>;

#[derive(Debug)]
pub struct StepInfo {
    pub(crate) step: Step,
    pub(crate) first: MatchSet,
}

impl StepInfo {
    pub fn fake(step: Step) -> StepInfo {
        StepInfo {
            step,
            first: MatchSet::null()
        }
    }
}

#[derive(Debug)]
pub enum Step {
    Nop,
    Chain(Vec<StepInfo>, Vec<Shape>),
    Token(Message),
    TokenTop(Message, Box<StepInfo>),
    Primitive(Box<dyn PrimitiveProcess + 'static>),
    Seq(Vec<StepInfo>),
    Repeat(Expr, Box<StepInfo>),
    Foreach(u32, Vec<(ValueId, Expr)>, Box<StepInfo>),
    Alt(Vec<(Vec<(Expr, Expr)>, StepInfo)>),
}

impl StepInfo {
    pub fn write_tree(&self, f: &mut dyn Write, indent: u32) -> IoResult<()> {
        let i: String = repeat(" ").take(indent as usize).collect();
        match self.step {
            Step::Nop => {},
            Step::Chain(ref c, _) => {
                writeln!(f, "{}Chain:", i)?;
                for step in c {
                    step.write_tree(f, indent+2)?;
                }
            }
            Step::Primitive(_) => {
                writeln!(f, "{} Primitive", i)?
            }
            Step::Token(ref message) => {
                writeln!(f, "{} Token: {:?}", i, message)?
            }
            Step::TokenTop(ref message, ref body) => {
                writeln!(f, "{}Up: {:?}", i, message)?;
                body.write_tree(f, indent+1)?;
            }
            Step::Seq(ref steps) => {
                writeln!(f, "{}Seq", i)?;
                for c in steps.iter() {
                    c.write_tree(f, indent+1)?;
                }
            }
            Step::Repeat(ref count, ref inner) => {
                writeln!(f, "{}Repeat: {:?}", i, count)?;
                inner.write_tree(f, indent + 1)?;
            }
            Step::Foreach(width, ref vars, ref inner) => {
                write!(f, "{}For: {} ", i, width)?;
                for &(id, ref expr) in vars { write!(f, "{}={:?}, ", id, expr)?; }
                writeln!(f, "")?;
                inner.write_tree(f, indent + 1)?;
            }
            Step::Alt(ref arms) => {
                writeln!(f, "{}Alt:", i)?;
                for &(ref cond, ref inner) in arms {
                    writeln!(f, "{} {:?} =>", i, cond)?;
                    inner.write_tree(f, indent + 2)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct StepBuilder<'a> {
    ctx: &'a Ctxt<'a>,
    scope: &'a Scope,
    shape_down: &'a Shape,
    shape_up: Option<&'a Shape>,
}

impl<'a> StepBuilder<'a> {
    fn nop(&self) -> StepInfo {
        StepInfo {
            first: MatchSet::epsilon(),
            step: Step::Nop
        }
    }

    fn chain(&self, steps: Vec<StepInfo>, shapes: Vec<Shape>) -> StepInfo {
        assert_eq!(shapes.len() + 1, steps.len());
        
        let bottom_first = &steps.first().unwrap().first;
        let top_first = &steps.last().unwrap().first;

        StepInfo {
            first: MatchSet::join(&bottom_first, &top_first),
            step: Step::Chain(steps, shapes)
        }
    }

    fn primitive(&self, prim: Box<dyn PrimitiveProcess + 'static>) -> StepInfo {
        StepInfo {
            first: MatchSet::null(),
            step: Step::Primitive(prim),
        }
    }

    fn token(&self, message: Message) -> StepInfo {
        StepInfo {
            first: MatchSet::lower(message.clone()),
            step: Step::Token(message)
        }
    }

    fn token_top(&self, message: Message, inner: StepInfo) -> StepInfo {
        StepInfo {
            first: MatchSet::upper(message.clone()).followed_by(inner.first.clone()),
            step: Step::TokenTop(message, Box::new(inner))
        }
    }

    fn seq(&self, steps: Vec<StepInfo>) -> StepInfo {
        //TODO: check that each adjacent followlast and first are non-overlapping
        StepInfo {
            first: steps.iter().fold(MatchSet::epsilon(), |a, s| a.followed_by(s.first.clone())),
            step: Step::Seq(steps)
        }
    }

    fn repeat(&self, count: Expr, inner: StepInfo) -> StepInfo {
        let count_includes_zero = match count.get_type() {
            Type::Integer(lo, hi) => lo <= 0 && hi >= 0,
            count_type => {
                warn!("Loop count type is {:?} not int", count_type);
                false
            }
        };

        // TODO: check that inner followlast and first are nonoverlapping
        // TODO: require that inner is non-nullable?

        StepInfo {
            first: if count_includes_zero {
                inner.first.clone().alternative(MatchSet::epsilon())
            } else {
                inner.first.clone()
            },
            step: Step::Repeat(count, Box::new(inner))
        }
    }

    fn foreach(&self, length: u32, vars: Vec<(ValueId, Expr)>, inner: StepInfo) -> StepInfo {
        //TODO: check that inner followlast and first are non-overlapping

        StepInfo {
            first: inner.first.clone(),
            step: Step::Foreach(length, vars, Box::new(inner))
        }
    }

    fn alt(&self, opts: Vec<(Vec<(Expr, Expr)>, StepInfo)>) -> StepInfo {
        // TODO: check that first is nonoverlapping
        StepInfo {
            first: opts.iter().fold(MatchSet::null(), |a, &(_, ref inner)| a.alternative(inner.first.clone())),
            step: Step::Alt(opts)
        }
    }

    fn with_lower<'b>(&'b self, scope: &'b Scope, shape_down: &'b Shape) -> StepBuilder<'b> {
        StepBuilder { scope, shape_down, ..*self }
    }

    fn with_upper<'b>(&'b self, scope: &'b Scope, shape_up: Option<&'b Shape>) -> StepBuilder<'b> {
        StepBuilder { scope, shape_up, ..*self }
    }

    fn with_scope<'b>(&'b self, scope: &'b Scope) -> StepBuilder<'b> {
        StepBuilder { scope, ..*self }
    }
}

fn resolve_action(sb: StepBuilder<'_>, action: &ast::Action) -> StepInfo {
    match *action {
        ast::Action::Process(ref processes) => {
            let (step, shape_up) = resolve_processes(sb, processes);
            assert!(shape_up.is_none());
            step
        }
        ast::Action::On(ref name, ref exprs, ref body) => {
            let mut body_scope = sb.scope.child();

            debug!("Upper message, shape: {:?}", sb.shape_up);

            let shape_up = sb.shape_up.expect("`on` block with no upper shape");

            let msginfo = on_expr_message(sb.ctx, &mut body_scope, shape_up, name, &exprs[..]);

            let step = if let &Some(ref body) = body {
                resolve_seq(sb.with_upper(&body_scope, None), body)
            } else {
                sb.nop()
            };

            let msg = msginfo.into_iter().collect();

            sb.token_top(msg, step)
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = value(sb.scope, count_ast.as_ref().map_or(&ast::Expr::Ignore, |s| &s.node));
            sb.repeat(count, resolve_seq(sb, block))
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

            let step = resolve_seq(sb.with_scope(&body_scope), block);

            sb.foreach(count.unwrap_or(0) as u32, inner_vars, step)
        }
        ast::Action::Alt(ref expr, ref arms) => {
            let r = rexpr(sb.scope, expr);

            let v = arms.iter().map(|arm| {
                let mut body_scope = sb.scope.child();
                let mut checks = Vec::new();
                pattern_match(sb.ctx, &mut body_scope, &arm.discriminant, &r, &mut checks);
                let step = resolve_seq(sb.with_scope(&body_scope), &arm.block);
                (checks, step)
            }).collect();

            sb.alt(v)
        }
    }
}

fn resolve_processes(sb: StepBuilder<'_>, processes: &[ast::Process]) -> (StepInfo, Option<Shape>) {
    let (step1, shape1) = resolve_process(sb, &processes[0]);

    if processes.len() == 1 {
        (step1, shape1)
    } else {
        let mut top_shape = shape1;
        let mut steps = vec![step1];
        let mut shapes = vec![];

        for process_ast in &processes[1..] {
            let shape_down = top_shape.unwrap();
            let (step, shape_up) = resolve_process(sb.with_lower(sb.scope, &shape_down), process_ast);
            shapes.push(shape_down);
            steps.push(step);
            top_shape = shape_up;
        }

        (sb.chain(steps, shapes), top_shape)
    }
}

fn resolve_process(sb: StepBuilder<'_>, process_ast: &ast::Process) -> (StepInfo, Option<Shape>) {
    match process_ast {
        ast::Process::Call(ref name, ref args_ast) => {
            let args = args_ast.iter().map(|a| rexpr(sb.scope, a)).collect();
            if sb.shape_down.has_variant_named(name) {
                let token_proc = resolve_token(sb.shape_down, name, args);
                (sb.token(token_proc), None)
            } else {
                let (scope, imp) = sb.ctx.index.find_def(sb.shape_down, name, args);
                match *imp {
                    DefImpl::Code(ref callee_ast) => {
                        resolve_processes(sb.with_scope(&scope), callee_ast)
                    }
                    DefImpl::Primitive(ref primitive, ref shape_up_ast) => {
                        let shape_up = shape_up_ast.as_ref().map(|s| resolve_protocol_invoke(sb.ctx, &scope, s));
                        let prim = primitive.instantiate(&scope);
                        (sb.primitive(prim), shape_up)
                    }
                }
            }
        }

        ast::Process::Seq(ref top_shape, ref block) => {
            let top_shape = resolve_protocol_invoke(sb.ctx, &sb.scope, top_shape);
            let block = resolve_seq(sb.with_upper(&sb.scope, Some(&top_shape)), block);
            (block, Some(top_shape))
        }

        ast::Process::InferSeq(ref block) => {
            let block = resolve_seq(sb.with_upper(&sb.scope, None), block);
            (block, None)
        }
    }
}

fn resolve_seq(sb: StepBuilder<'_>, block: &ast::Block) -> StepInfo {
    let mut scope = sb.scope.child();

    for ld in &block.lets {
        resolve_letdef(&mut scope, ld);
    }

    let steps = block.actions.iter().map(|action| {
        resolve_action(sb.with_scope(&scope), action)
    }).collect();

    sb.seq(steps)
}

pub fn resolve_letdef(scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef(ref name, ref expr) = ld;
    let item = rexpr(scope, expr);
    scope.bind(&name, item);
}

pub fn resolve_token(shape: &Shape, variant_name: &str, args: Vec<Item>) -> Message {
    fn inner(i: Item, shape: &Item, push: &mut dyn FnMut(Expr)) {
        match shape {
            &Item::Value(Expr::Const(_)) => (),
            &Item::Value(_) => {
                if let Item::Value(v) = i {
                    push(v);
                } else {
                    panic!("Expected value but found {:?}", i);
                }
            }
            &Item::Tuple(ref m) => {
                if let Item::Tuple(t) = i {
                    if t.len() == m.len() {
                        for (mi, i) in m.iter().zip(t.into_iter()) {
                            inner(i, mi, push)
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

    shape.build_variant_fields(variant_name, |msg_params, push| {
        assert_eq!(args.len(), msg_params.len(), "wrong number of arguments to message {:?} {:?} {:?}", variant_name, msg_params, args);

        for (param, arg) in msg_params.iter().zip(args) {
            if param.direction.up || param.direction.down {
                inner(arg, &param.item, push)
            }
        }
    })
}

pub struct ProcessChain {
    pub step: StepInfo,
    pub shape_up: Option<Shape>,
}

pub fn compile_process_chain(ctx: &Ctxt, scope: &Scope, shape_down: &Shape, ast: &[ast::Process]) -> ProcessChain {
    let sb = StepBuilder { ctx, scope, shape_up: None, shape_down };
    let (step, shape_up) = resolve_processes(sb, ast);
    ProcessChain { step, shape_up }
}

