use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use session::ValueID;
use super::{ ast, expr };
use super::scope::{ Scope, Item };
use super::eval::Expr;
use protocol::{ Shape, Fields };
use super::protocol::{ ProtocolScope, DefImpl, resolve_protocol_invoke };
use super::module_loader::Ctxt;
use process::{ Process, ProcessInfo };
use connection::{ Connection };
use super::exec;
use super::matchset::{ self, MatchSet };
use super::direction_infer::{ ResolveInfo, infer_direction, infer_top_fields };

use data::{ DataMode, Type };

pub type Message = Vec<Option<Expr>>;

#[derive(Debug)]
pub struct StepInfo {
    pub step: Step,
    pub dir: ResolveInfo,
    pub first: MatchSet,
}

#[derive(Debug)]
pub enum Step {
    Nop,
    Token(Message),
    TokenTop(Message, Box<StepInfo>),
    Seq(Vec<StepInfo>),
    Repeat(Expr, Box<StepInfo>),
    Foreach(u32, Vec<(ValueID, Expr)>, Box<StepInfo>),
    Alt(Vec<(Vec<(Expr, Expr)>, StepInfo)>),
    Fork(Box<StepInfo>, Fields, Box<StepInfo>)
}

impl StepInfo {
    pub fn write_tree(&self, f: &mut Write, indent: u32) -> IoResult<()> {
        let i: String = repeat(" ").take(indent as usize).collect();
        match self.step {
            Step::Nop => {},
            Step::Token(ref message) => {
                try!(writeln!(f, "{}Token: {:?}", i, message));
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
            Step::Fork(ref bottom, ref fields, ref top) => {
                try!(writeln!(f, "{}Fork: {:?}", i, fields));
                try!(bottom.write_tree(f, indent+1));
                try!(top.write_tree(f, indent+1));
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy)]
struct StepBuilder<'a> {
    ctx: &'a Ctxt<'a>,
    scope: &'a Scope,
    protocol_scope: &'a ProtocolScope,
    shape_down: &'a Shape,
    shape_up: &'a Shape,
    fields_down: &'a Fields,
}

impl<'a> StepBuilder<'a> {
    fn step(&self, step: Step) -> StepInfo {
        let first = matchset::first(&step);
        let dir = infer_direction(&step, self.fields_down);
        StepInfo { step, first, dir }
    }

    fn with_upper<'b>(&'b self, scope: &'b Scope, shape_up: &'b Shape) -> StepBuilder<'b> {
        StepBuilder { scope, shape_up, ..*self }
    }

    fn with_lower<'b>(&'b self, shape_down: &'b Shape, fields_down: &'b Fields) -> StepBuilder<'b> {
        StepBuilder { shape_down, fields_down, ..*self }
    }

    fn with_scope<'b>(&'b self, scope: &'b Scope) -> StepBuilder<'b> {
        StepBuilder { scope, ..*self }
    }
}

fn resolve_action(sb: StepBuilder, action: &ast::Action) -> StepInfo {
    match *action {
        ast::Action::Call(ref name, ref param_ast, ref body) => {
            let param = expr::rexpr(sb.ctx, sb.scope, param_ast);

            if sb.shape_down.has_variant_named(name) {
                if body.is_some() {
                    panic!("Primitives have no upward shape");
                }

                sb.step(Step::Token(resolve_token(sb.shape_down, name, param)))
            } else {
                let (scope, imp, shape_up) = sb.protocol_scope.find(sb.ctx, sb.shape_down, name, param);

                let inner_step = match *imp {
                    DefImpl::Code(ref seq) => resolve_seq(sb.with_upper(&scope, &shape_up), seq),
                    DefImpl::Primitive(..) => panic!("Primitive not allowed here"),
                };

                match body {
                    Some(ref body_block) => {
                        let mut fields_up = shape_up.fields(DataMode { down: false, up: true });
                        infer_top_fields(&inner_step, &mut fields_up);
                        let child_step = resolve_seq(sb.with_lower(&shape_up, &fields_up), body_block);
                        sb.step(Step::Fork(Box::new(inner_step), fields_up, Box::new(child_step)))
                    }
                    None => inner_step
                }
            }
        }
        ast::Action::On(ref name, ref expr, ref body) => {
            let mut body_scope = sb.scope.child();

            debug!("Upper message, shape: {:?}", sb.shape_up);
            let msginfo = expr::on_expr_message(sb.ctx, &mut body_scope, sb.shape_up, name, expr);

            let step = if let &Some(ref body) = body {
                resolve_seq(sb.with_upper(&body_scope, &Shape::None), body)
            } else {
                sb.step(Step::Nop)
            };

            let msg = msginfo.into_iter().collect();

            sb.step(Step::TokenTop(msg, box step))
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = expr::value(sb.ctx, sb.scope, count_ast);
            sb.step(Step::Repeat(count, box resolve_seq(sb, block)))
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
                    let id = body_scope.new_variable(sb.ctx.session, name, ty);
                    inner_vars.push((id, e));
                } else {
                    panic!("Foreach must loop over vector type, not {:?}", t)
                }
            }

            let step = resolve_seq(sb.with_scope(&body_scope), block);

            sb.step(Step::Foreach(count.unwrap_or(0) as u32, inner_vars, box step))
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

            sb.step(Step::Alt(v))
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

    sb.step(Step::Seq(steps))
}

pub fn resolve_letdef(ctx: &Ctxt, scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef(ref name, ref expr) = ld;
    let item = expr::rexpr(ctx, scope, expr);
    scope.bind(&name, item);
}

fn resolve_token(shape: &Shape, variant_name: &str, item: Item) -> Message {
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

    if let Some(mut f) = ctx.session.debug_file(|| format!("{}.steps", name)) {
        step.write_tree(&mut f, 0).unwrap_or_else(|e| error!("{}", e));
    }

    ProcessInfo { fields_up, shape_up, implementation: Box::new(Program{step}) }
}


pub fn make_literal_process(ctx: &Ctxt,
                        scope: &Scope,
                        protocol_scope: &ProtocolScope,
                        is_up: bool,
                        shape_up_expr: &ast::ProtocolRef,
                        block: &ast::Block) -> ProcessInfo {

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

    ProcessInfo { fields_up, shape_up, implementation: Box::new(ProgramFlip { step })}
}


pub struct Program {
    pub step: StepInfo
}

impl Process for Program {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        exec::run(&self.step, downwards, upwards)
    }
}

pub struct ProgramFlip {
    pub step: StepInfo
}

impl Process for ProgramFlip {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        exec::run(&self.step, upwards, downwards)
    }
}
