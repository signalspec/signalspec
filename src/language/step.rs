use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use super::ValueID;
use super::{ ast, expr };
use super::scope::{ Scope, Item };
use super::eval::Expr;
use protocol::{ Shape, Fields };
use super::protocol::{ ProtocolScope, resolve_protocol_invoke };
use super::program::resolve_process;
use super::module_loader::Ctxt;
use process::{ PrimitiveProcess, Process, ProcessInfo, ProcessChain };
use connection::{ Connection };
use super::exec;
use super::matchset::MatchSet;
use super::direction_infer::{ ResolveInfo, infer_top_fields };

use data::{ DataMode, Type };

pub type Message = Vec<Option<Expr>>;

#[derive(Debug)]
pub struct StepInfo {
    pub step: Step,
    pub dir: ResolveInfo,
    pub first: MatchSet,
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
    Foreach(u32, Vec<(ValueID, Expr)>, Box<StepInfo>),
    Alt(Vec<(Vec<(Expr, Expr)>, StepInfo)>),
}



impl StepInfo {
    pub fn write_tree(&self, f: &mut Write, indent: u32) -> IoResult<()> {
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
            Step::TokenTop(ref message, box ref body) => {
                try!(writeln!(f, "{}Up: {:?}", i, message));
                try!(body.write_tree(f, indent+1));
            }
            Step::Seq(ref steps) => {
                try!(writeln!(f, "{}Seq", i));
                for c in steps.iter() {
                    try!(c.write_tree(f, indent+1));
                }
            }
            Step::Repeat(ref count, box ref inner) => {
                try!(writeln!(f, "{}Repeat: {:?}", i, count));
                try!(inner.write_tree(f, indent + 1));
            }
            Step::Foreach(width, ref vars, box ref inner) => {
                try!(write!(f, "{}For: {} ", i, width));
                for &(id, ref expr) in vars { try!(write!(f, "{}={:?}, ", id, expr)); }
                try!(writeln!(f, ""));
                try!(inner.write_tree(f, indent + 1));
            }
            Step::Alt(ref arms) => {
                try!(writeln!(f, "{}Alt:", i));
                for &(ref cond, ref inner) in arms {
                    try!(writeln!(f, "{} {:?} =>", i, cond));
                    try!(inner.write_tree(f, indent + 2));
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct StepBuilder<'a> {
    ctx: &'a Ctxt,
    scope: &'a Scope,
    protocol_scope: &'a ProtocolScope,
    shape_down: &'a Shape,
    shape_up: &'a Shape,
    fields_down: &'a Fields,
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
        let mut fields_down = self.fields_down;
        for process in &p.processes {
            match process.process {
                Process::Token(ref msg) => dir = dir.merge_seq(&ResolveInfo::from_message(&msg, fields_down)),
                Process::Seq(ref s) => dir = dir.merge_seq(&s.dir),
                Process::Primitive(_) => unimplemented!(),
            }
            fields_down = &process.fields_up;
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

    fn foreach(&self, length: u32, vars: Vec<(ValueID, Expr)>, inner: StepInfo) -> StepInfo {
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

    fn with_upper<'b>(&'b self, scope: &'b Scope, shape_up: &'b Shape) -> StepBuilder<'b> {
        StepBuilder { scope, shape_up, ..*self }
    }

    fn with_scope<'b>(&'b self, scope: &'b Scope) -> StepBuilder<'b> {
        StepBuilder { scope, ..*self }
    }
}

fn resolve_action(sb: StepBuilder, action: &ast::Action) -> StepInfo {
    match *action {
        ast::Action::Process(ref processes) => {
            sb.process(resolve_process(sb.ctx, sb.scope, sb.protocol_scope, sb.shape_down, sb.fields_down, &processes[..]))
        }
        ast::Action::On(ref name, ref expr, ref body) => {
            let mut body_scope = sb.scope.child();

            debug!("Upper message, shape: {:?}", sb.shape_up);
            let msginfo = expr::on_expr_message(sb.ctx, &mut body_scope, sb.shape_up, name, expr);

            let step = if let &Some(ref body) = body {
                resolve_seq(sb.with_upper(&body_scope, &Shape::None), body)
            } else {
                sb.nop()
            };

            let msg = msginfo.into_iter().collect();

            sb.token_top(msg, step)
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = expr::value(sb.ctx, sb.scope, count_ast.as_ref().map_or(&ast::Expr::Ignore, |s| &s.node));
            sb.repeat(count, resolve_seq(sb, block))
        }
        ast::Action::For(ref pairs, ref block) => {
            let mut body_scope = sb.scope.child();
            let mut count = None;
            let mut inner_vars = Vec::with_capacity(pairs.len());

            for &(ref name, ref expr) in pairs {
                let e = expr::value(sb.ctx, sb.scope, expr);
                let t = e.get_type();
                if let Type::Vector(c, box ty) = t {
                    match count {
                        Some(count) => assert_eq!(count, c),
                        None => count = Some(c),
                    }
                    let id = body_scope.new_variable(sb.ctx, name, ty);
                    inner_vars.push((id, e));
                } else {
                    panic!("Foreach must loop over vector type, not {:?}", t)
                }
            }

            let step = resolve_seq(sb.with_scope(&body_scope), block);

            sb.foreach(count.unwrap_or(0) as u32, inner_vars, step)
        }
        ast::Action::Alt(ref expr, ref arms) => {
            let r = expr::rexpr(sb.ctx, sb.scope, expr);

            let v = arms.iter().map(|arm| {
                let mut body_scope = sb.scope.child();
                let mut checks = Vec::new();
                expr::pattern_match(sb.ctx, &mut body_scope, &arm.discriminant, &r, &mut checks);
                let step = resolve_seq(sb.with_scope(&body_scope), &arm.block);
                (checks, step)
            }).collect();

            sb.alt(v)
        }
    }
}

fn resolve_seq(sb: StepBuilder, block: &ast::Block) -> StepInfo {
    let mut scope = sb.scope.child();

    for ld in &block.lets {
        resolve_letdef(sb.ctx, &mut scope, ld);
    }

    let steps = block.actions.iter().map(|action| {
        resolve_action(sb.with_scope(&scope), action)
    }).collect();

    sb.seq(steps)
}

pub fn resolve_letdef(ctx: &Ctxt, scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef(ref name, ref expr) = ld;
    let item = expr::rexpr(ctx, scope, expr);
    scope.bind(&name, item);
}

pub fn resolve_token(shape: &Shape, variant_name: &str, item: Item) -> Message {
    fn inner(i: Item, shape: &Item, push: &mut FnMut(Expr)) {
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

    shape.build_variant_fields(variant_name, |variant_shape, push| {
        inner(item, variant_shape, push)
    })
}

pub fn compile_block(ctx: &Ctxt,
                     scope: &Scope,
                     protocol_scope: &ProtocolScope,
                     shape_down: &Shape,
                     fields_down: &Fields,
                     shape_up: Shape,
                     seq: &ast::Block,
                     name: &str) -> ProcessInfo {

    let mut fields_up = shape_up.fields(DataMode { down: false, up: true });

    let step = {
        let sb = StepBuilder { ctx, scope, protocol_scope,
            shape_up: &shape_up, shape_down: &shape_down,
            fields_down
        };

        resolve_seq(sb, seq)
    };

    infer_top_fields(&step, &mut fields_up);

    if let Some(mut f) = ctx.debug_file(|| format!("{}.steps", name)) {
        step.write_tree(&mut f, 0).unwrap_or_else(|e| error!("{}", e));
    }

    ProcessInfo { fields_up: fields_up.clone(), shape_up, process: Process::Seq(step) }
}

pub fn make_literal_process(ctx: &Ctxt,
                        scope: &Scope,
                        protocol_scope: &ProtocolScope,
                        is_up: bool,
                        shape_up_expr: &ast::ProtocolRef,
                        block: &ast::Block) -> ProcessChain {

    let shape_up = resolve_protocol_invoke(ctx, scope, shape_up_expr);
    let shape_down = Shape::None;

    let fields_up = shape_up.fields(DataMode { down: !is_up, up: is_up });
    let fields_flip = shape_up.fields(DataMode { down: is_up, up: !is_up });

    let step = {
        let sb = StepBuilder { ctx, scope, protocol_scope,
            shape_down: &shape_up, shape_up: &shape_down,
            fields_down: &fields_flip
        };

        super::step::resolve_seq(sb, block)
    };

    let inner_process =  ProcessChain {
        processes: vec![ProcessInfo {
            shape_up: Shape::None,
            fields_up: Fields::null(),
            process: Process::Seq(step)
        }]
    };

    ProcessChain {
        processes: vec![ProcessInfo { fields_up, shape_up,
            process: Process::Primitive(Box::new(ProgramFlip { inner_process }))
        }]
    }
}

#[derive(Debug)]
pub struct ProgramFlip {
    pub inner_process: ProcessChain
}

impl PrimitiveProcess for ProgramFlip {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        exec::run(&self.inner_process, upwards, downwards)
    }
}
