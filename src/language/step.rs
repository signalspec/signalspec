use bit_set::BitSet;
use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use session::ValueID;
use super::{ ast, expr };
use super::scope::{ Scope, Item };
use super::eval::Expr;
use super::protocol::{ ProtocolScope, DefImpl };
use super::module_loader::Ctxt;
use protocol::{ Shape, Fields };

use data::{ DataMode, Type, Value };

pub type Message = Vec<Option<Expr>>;

#[derive(Debug)]
pub enum Step {
    Nop,
    Token(Message),
    TokenTop(Message, Box<Step>),
    Seq(Vec<Step>),
    Repeat(Expr, Box<Step>, bool),
    Foreach(u32, Vec<(ValueID, Expr, DataMode)>, Box<Step>),
    Alt(Vec<(Vec<(Expr, Expr)>, Step)>, bool),
}

impl Step {
    pub fn write_tree(&self, f: &mut Write, indent: u32) -> IoResult<()> {
        let i: String = repeat(" ").take(indent as usize).collect();
        match *self {
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
            Step::Repeat(ref count, box ref inner, up) => {
                try!(writeln!(f, "{}Repeat: {:?} {}", i, count, up));
                try!(inner.write_tree(f, indent + 1));
            }
            Step::Foreach(width, ref vars, box ref inner) => {
                try!(write!(f, "{}For: {} ", i, width));
                for &(id, ref expr, dir) in vars { try!(write!(f, "{}={:?} {:?}, ", id, expr, dir)); }
                try!(writeln!(f, ""));
                try!(inner.write_tree(f, indent + 1));
            }
            Step::Alt(ref arms, up) => {
                try!(writeln!(f, "{}Alt: {}", i, up));
                for &(ref cond, ref inner) in arms {
                    try!(writeln!(f, "{} {:?} =>", i, cond));
                    try!(inner.write_tree(f, indent + 2));
                }
            }
        }
        Ok(())
    }
}

/// Summary of the usage of values within an block and its children
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ResolveInfo {
    /// The set of variable IDs that will be down-evaluated to produce a value used by the
    /// block. The enclosing expression must set these variables.
    pub vars_down: BitSet,

    /// The set of variable IDs that are produced in up-evaluation within the block.
    pub vars_up: BitSet,

    /// Whether the expression contains blocks that force an enclosing repeat block to always
    /// up-evaluate its count.
    pub repeat_up_heuristic: bool,
}

impl ResolveInfo {
    fn new() -> ResolveInfo {
        ResolveInfo {
            vars_down: BitSet::new(),
            vars_up: BitSet::new(),
            repeat_up_heuristic: false,
        }
    }

    fn mode_of(&self, id: ValueID) -> DataMode {
        DataMode { up: self.vars_up.contains(id), down: self.vars_down.contains(id)}
    }

    fn use_expr(&mut self, e: &Expr, dir: DataMode) {
        e.each_var(&mut |id| {
            if dir.down { self.vars_down.insert(id); }
            if dir.up { self.vars_up.insert(id); }
        });
        self.repeat_up_heuristic |= dir.up && e.refutable();
    }

    fn merge_seq(&mut self, o: &ResolveInfo) {
        self.vars_down.union_with(&o.vars_down);
        self.vars_up.union_with(&o.vars_up);
        self.repeat_up_heuristic |= o.repeat_up_heuristic;
    }
}

fn resolve_action<'s>(ctx: &'s Ctxt<'s>,
                      scope: &Scope,
                      protocol_scope: &ProtocolScope<'s>,
                      shape_down: &Shape,
                      shape_up: &Shape,
                      action: &'s ast::Action) -> Step {
    match *action {
        ast::Action::Call(ref name, ref param_ast, ref body) => {
            let param = expr::rexpr(ctx, scope, param_ast);

            let (scope, imp, mut shape_up) = protocol_scope.find(ctx, shape_down, name, param);

            let step = match *imp {
                DefImpl::Code(ref seq) => {
                    resolve_seq(ctx, &scope, protocol_scope, shape_down, &mut shape_up, seq)
                }
                DefImpl::Primitive(..) => panic!("Primitive not allowed here"),
            };

            if let &Some(ref _body) = body {
                unimplemented!();
            } else {
                step
            }
        }
        ast::Action::Token(ref expr) => {
            debug!("Token: {:?}", expr);

            let item = expr::rexpr(ctx, scope, expr);
            Step::Token(resolve_token(item, shape_down))
        }
        ast::Action::On(ref expr, ref body) => {
            let mut body_scope = scope.child();

            debug!("Upper message, shape: {:?}", shape_up);
            let msginfo = expr::on_expr_message(ctx, &mut body_scope, shape_up, expr);

            let step = if let &Some(ref body) = body {
                resolve_seq(ctx, &body_scope, protocol_scope, shape_down, &mut Shape::null(), body)
            } else {
                Step::Nop
            };

            let msg = msginfo.into_iter().collect();

            Step::TokenTop(msg, box step)
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = expr::value(ctx, scope, count_ast);
            let step = resolve_seq(ctx, scope, protocol_scope, shape_down, shape_up, block);
            Step::Repeat(count, box step, false)
        }
        ast::Action::For(ref pairs, ref block) => {
            let mut body_scope = scope.child();
            let mut count = None;
            let mut inner_vars = Vec::with_capacity(pairs.len());

            for &(ref name, ref expr) in pairs {
                let e = expr::value(ctx, scope, expr);
                let t = e.get_type();
                if let Type::Vector(c, box ty) = t {
                    match count {
                        Some(count) => assert_eq!(count, c),
                        None => count = Some(c),
                    }
                    let id = body_scope.new_variable(ctx.session, name, ty);
                    inner_vars.push((id, e, DataMode { up: false, down: false}));
                } else {
                    panic!("Foreach must loop over vector type, not {:?}", t)
                }
            }

            let step = resolve_seq(ctx, &body_scope, protocol_scope, shape_down, shape_up, block);

            Step::Foreach(count.unwrap_or(0) as u32, inner_vars, box step)
        }
        ast::Action::Alt(ref expr, ref arms) => {
            let r = expr::rexpr(ctx, scope, expr);

            let v = arms.iter().map(|arm| {
                let mut body_scope = scope.child();
                let mut checks = Vec::new();
                expr::pattern_match(ctx, &mut body_scope, &arm.discriminant, &r, &mut checks);
                let step = resolve_seq(ctx, &body_scope, protocol_scope, shape_down, shape_up, &arm.block);
                (checks, step)
            }).collect();

            Step::Alt(v, false)
        }
    }
}

pub fn resolve_seq<'s>(ctx: &'s Ctxt<'s>,
                  pscope: &Scope,
                  protocol_scope: &ProtocolScope<'s>,
                  shape_down: &Shape,
                  shape_up: &Shape,
                  block: &'s ast::Block) -> Step {
    let mut scope = pscope.child();
    resolve_letdef(ctx, &mut scope, &block.lets);

    let steps = block.actions.iter().map(|action| {
        resolve_action(ctx, &scope, protocol_scope, shape_down, shape_up, action)
    }).collect();

    Step::Seq(steps)
}

pub fn resolve_letdef<'s>(ctx: &'s Ctxt<'s>, scope: &mut Scope, lets: &'s [ast::LetDef]) {
    for &ast::LetDef(ref name, ref expr) in lets.iter() {
        let item = expr::rexpr(ctx, scope, expr);
        scope.bind(&name, item);
    }
}

fn resolve_token<'shape>(item: Item, shape: &'shape Shape) -> Message {
    fn try_variant(shape: &Shape, item: &Item) -> bool {
        match (shape, item) {
            (&Shape::Const(ref c), &Item::Value(Expr::Const(ref v))) => c == v,
            (&Shape::Val(ref t), &Item::Value(ref e)) => t.includes_type(&e.get_type()),
            (&Shape::Tup(ref m), &Item::Tuple(ref t)) if m.len() == t.len() => {
                 m.iter().zip(t.iter()).all(|(i, s)| { try_variant(i, s) })
            }
            (&Shape::Protocol{ ref messages, .. }, i) => messages.iter().any(|m| try_variant(m, i)),
            _ => false,
        }
    }

    fn inner(i: Item, shape: &Shape, msg: &mut Message) {
        match shape {
            &Shape::Val(_) => {
                if let Item::Value(v) = i {
                    msg.push(Some(v));
                } else {
                    panic!("Expected value but found {:?}", i);
                }
            }
            &Shape::Const(..) => (),
            &Shape::Tup(ref m) => {
                if let Item::Tuple(t) = i {
                    if t.len() == m.len() {
                        for (mi, i) in m.iter().zip(t.into_iter()) {
                            inner(i, mi, msg)
                        }
                    } else {
                        panic!("Expected tuple length {}, found {}", m.len(), t.len());
                    }
                } else {
                    panic!("Expected tuple of length {}, found {:?}", m.len(), i);
                }
            }
            &Shape::Protocol { ref messages, ..} => {
                if messages.len() == 1 {
                    inner(i, &messages[0], msg)
                } else {
                    let tag_idx = msg.len();
                    msg.push(None);

                    let mut matching_variants = 0;
                    for (idx, shape) in messages.iter().enumerate() {
                        if try_variant(shape, &i) {
                            msg[tag_idx] = Some(Expr::Const(Value::Integer(idx as i64)));
                            inner(i.clone(), shape, msg);
                            matching_variants += 1;
                        } else {
                            // Create dummy fields for other variants
                            msg.extend((0..shape.count_fields()).map(|_| None));
                        }
                    }

                    match matching_variants {
                        1 => (),
                        0 => panic!("No variant matched {:?}", i),
                        _ => panic!("Multiple variants matched {:?}", i)
                    }
                }
            }
        }
    }

    let mut msg = vec![];
    inner(item, &shape, &mut msg);
    msg
}

pub fn infer_direction(step: &mut Step, bottom_fields: &Fields, top_fields: &mut Fields) -> ResolveInfo {
    use self::Step::*;
    match *step {
        Nop => ResolveInfo::new(),
        Token(ref msg) => {
            assert_eq!(msg.len(), bottom_fields.len());
            let mut ri = ResolveInfo::new();
            for (m, f) in msg.iter().zip(bottom_fields.iter()) {
                if !f.is_tag {
                    if let &Some(ref expr) = m {
                        ri.use_expr(expr, f.dir);
                    }
                }
            }
            ri
        }
        TokenTop(ref msg, ref mut body) => {
            let mut ri = infer_direction(body, bottom_fields, &mut Fields::new(vec![]));
            assert_eq!(msg.len(), top_fields.len());

            // Update the upward shape's direction with results of analyzing the usage of
            // its data in the `on x { ... }` body.
            for (m, f) in msg.iter().zip(top_fields.iter_mut()) {
                if !f.is_tag {
                    if let &Some(ref expr) = m {
                        let constraint = match *expr {
                            Expr::Variable(id, _) => ri.mode_of(id),

                            //TODO: is the down value right?
                            ref e => DataMode { up: e.down_evaluable(), down: false }
                        };
                        f.dir.constrain(constraint);
                    }
                }
            }
            ri.repeat_up_heuristic = true;
            ri
        }
        Seq(ref mut steps) => {
            let mut ri = ResolveInfo::new();
            for step in steps {
                ri.merge_seq(&infer_direction(step, bottom_fields, top_fields));
            }
            ri
        }
        Repeat(ref count, ref mut body, ref mut dir) => {
            let mut ri = infer_direction(body, bottom_fields, top_fields);
            let any_up = ri.repeat_up_heuristic;
            ri.use_expr(&count, DataMode { down: !any_up, up: any_up });
            *dir = any_up;
            ri
        }
        Foreach(_, ref mut inner_vars, ref mut body) => {
            let mut ri = infer_direction(body, bottom_fields, top_fields);
            for &mut (id, ref e, ref mut dir) in inner_vars {
                *dir = ri.mode_of(id);
                ri.use_expr(e, *dir);
            }
            ri
        }
        Alt(ref mut arms, ref mut up) => {
            let mut ri = ResolveInfo::new();
            for &mut (_, ref mut body) in arms.iter_mut() {
                ri.merge_seq(&infer_direction(body, bottom_fields, top_fields)); // TODO: alt != seq ?
            }

            *up = ri.repeat_up_heuristic;

            for &mut (ref checks, _) in arms.iter_mut() {
                for &(_, ref r) in checks {
                    ri.use_expr(r, DataMode { down: !*up, up: *up })
                    // LHS is patterns that don't contain dynamic vars, so no need to mark them
                }
            }
            ri
        }
    }
}
