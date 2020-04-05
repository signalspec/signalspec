use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use crate::syntax::ast;
use super::{ Scope, Item, Expr, Shape, ValueId,
    Ctxt, Process, ProcessInfo, ProcessChain, MatchSet, ResolveInfo, Type };
use super::{ on_expr_message, value, pattern_match, rexpr };
use super::process::resolve_process;

pub type Message = Vec<Option<Expr>>;

#[derive(Debug)]
pub struct StepInfo {
    pub(crate) step: Step,
    pub(crate) dir: ResolveInfo,
    pub(crate) first: MatchSet,
}

impl StepInfo {
    pub fn fake(step: Step) -> StepInfo {
        StepInfo {
            step,
            dir: ResolveInfo::new(),
            first: MatchSet::null()
        }
    }
}

#[derive(Debug)]
pub enum Step {
    Nop,
    Process(ProcessChain),
    TokenTop(Message, Box<StepInfo>),
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
            Step::Process(ref processes) => {
                writeln!(f, "{}Process:", i)?;
                for p in &processes.processes {
                    match p.process {
                        Process::Token(ref message) => writeln!(f, "{} Token: {:?}", i, message)?,
                        Process::Seq(ref step) => {
                            writeln!(f, "{} Seq:", i)?;
                            step.write_tree(f, indent+2)?;
                        }
                        Process::Primitive(_) => writeln!(f, "{} Primitive", i)?
                    }
                }
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
            dir: ResolveInfo::new(),
            step: Step::Nop
        }
    }

    fn process(&self, p: ProcessChain) -> StepInfo {
        if p.processes.len() == 1 {
            if let Process::Seq(_) = p.processes[0].process {
                match p.processes.into_iter().next().unwrap().process {
                    Process::Seq(s) => return s,
                    _ => unreachable!()
                }
            }
        }

        fn process_first(p: &ProcessInfo) -> MatchSet {
            match p.process {
                Process::Token(ref msg) => MatchSet::lower(msg.clone()),
                Process::Seq(ref s) => s.first.clone(),
                Process::Primitive(_) => unimplemented!(),
            }
        }

        let bottom_first = process_first(p.processes.first().unwrap());
        let top_first = process_first(p.processes.last().unwrap());

        let mut dir = ResolveInfo::new();
        for process in &p.processes {
            match process.process {
                Process::Token(ref msg) => {
                    dir = dir.merge_seq(&ResolveInfo::from_message(&msg, &self.shape_down));
                }
                Process::Seq(ref s) => dir = dir.merge_seq(&s.dir),
                Process::Primitive(_) => unimplemented!(),
            }
        }

        StepInfo {
            first: MatchSet::join(&bottom_first, &top_first),
            dir,
            step: Step::Process(p)
        }
    }

    fn token_top(&self, message: Message, inner: StepInfo) -> StepInfo {
        StepInfo {
            first: MatchSet::upper(message.clone()).followed_by(inner.first.clone()),
            dir: inner.dir.with_up(),
            step: Step::TokenTop(message, Box::new(inner))
        }
    }

    fn seq(&self, steps: Vec<StepInfo>) -> StepInfo {
        //TODO: check that each adjacent followlast and first are non-overlapping
        StepInfo {
            first: steps.iter().fold(MatchSet::epsilon(), |a, s| a.followed_by(s.first.clone())),
            dir: ResolveInfo::steps(&steps),
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
            dir: inner.dir.repeat(&count),
            step: Step::Repeat(count, Box::new(inner))
        }
    }

    fn foreach(&self, length: u32, vars: Vec<(ValueId, Expr)>, inner: StepInfo) -> StepInfo {
        //TODO: check that inner followlast and first are non-overlapping

        StepInfo {
            first: inner.first.clone(),
            dir: inner.dir.foreach(&vars),
            step: Step::Foreach(length, vars, Box::new(inner))
        }
    }

    fn alt(&self, opts: Vec<(Vec<(Expr, Expr)>, StepInfo)>) -> StepInfo {
        // TODO: check that first is nonoverlapping
        StepInfo {
            first: opts.iter().fold(MatchSet::null(), |a, &(_, ref inner)| a.alternative(inner.first.clone())),
            dir: ResolveInfo::alt(&opts),
            step: Step::Alt(opts)
        }
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
            sb.process(resolve_process(sb.ctx, sb.scope, sb.shape_down, &processes[..]))
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
                if let Type::Vector(c, ty) = t {
                    match count {
                        Some(count) => assert_eq!(count, c),
                        None => count = Some(c),
                    }
                    let id = body_scope.new_variable(sb.ctx, name, *ty);
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

pub fn compile_block(ctx: &Ctxt,
                     scope: &Scope,
                     shape_down: &Shape,
                     shape_up: Option<Shape>,
                     seq: &ast::Block,
                     name: &str) -> ProcessInfo {
    let step = {
        let sb = StepBuilder { ctx, scope,
            shape_up: shape_up.as_ref(), shape_down: shape_down,
        };

        resolve_seq(sb, seq)
    };

    if let Some(mut f) = ctx.debug_file(|| format!("{}.steps", name)) {
        step.write_tree(&mut f, 0).unwrap_or_else(|e| error!("{}", e));
    }

    ProcessInfo { shape_up, process: Process::Seq(step) }
}

