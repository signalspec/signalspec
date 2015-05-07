use ast;

use session::Session;
use resolve::expr;
pub use exec::Step;
pub use resolve::scope::{ Scope, Item };
use resolve::types::{self, Shape};
use eval::DataMode;


pub fn resolve_module<'s>(session: &'s Session<'s>, ast: &'s ast::Module) -> Scope<'s> {
    let mut scope = session.prelude.clone();

    for _import in ast.imports.iter() {
        panic!("Imports unimplemented");
    }

    resolve_letdef(session, &mut scope, &ast.lets);

    for def in ast.defs.iter() {
        let ed = Item::Def(session.closure_arena.alloc(EventClosure{ ast:def, parent_scope: scope.clone() }));
        scope.names.insert(def.name.to_string(), ed);
    }

    scope
}

/// A user-defined event
pub struct EventClosure<'s> {
    pub ast: &'s ast::Def,
    pub parent_scope: Scope<'s>,
}

impl<'s> EventClosure<'s> {
    pub fn resolve_call(&self,
                        session: &'s Session<'s>,
                        shape_down: &Shape,
                        param: Item<'s>) -> (Shape, Step) {

        let mut scope = self.parent_scope.child(); // Base on lexical parent

        let shape_up = if let Some(ref intf_expr) = self.ast.interface {
            expr::rexpr(session, &scope, intf_expr).into_shape(DataMode{ up: true, down: true })
        } else {
            types::NULL_SHAPE.clone()
        };

        expr::assign(session, &mut scope, &self.ast.param, param);
        let steptree = resolve_seq(session, &scope, shape_down, &shape_up, &self.ast.block);

        // TODO: analyze the data direction and clear direction bits in shape_up

        (shape_up, steptree)
    }
}

/// A body associated with an event call
pub struct EventBodyClosure<'s> {
    ast: &'s ast::Block,
    parent_scope: Scope<'s>,
}

fn resolve_action<'s>(session: &'s Session<'s>,
                      scope: &Scope<'s>,
                      shape_down: &Shape,
                      shape_up: &Shape,
                      action: &ast::Action) -> Step {
    match *action {
        ast::Action::Seq(ref block) => resolve_seq(session, scope, shape_down, shape_up, block),
        ast::Action::Call(ref expr, ref arg, ref body) => {
            let arg = expr::rexpr(session, scope, arg);
            let body = body.as_ref().map(|x| {
                EventBodyClosure { ast: x, parent_scope: scope.child() }
            });

            if body.is_some() {
                unimplemented!();
            }

            match expr::rexpr(session, scope, expr) {
                Item::Def(entity) => {
                    let (_shape_child, step) = entity.resolve_call(session, shape_down, arg);
                    step
                }
                _ => panic!("Not callable"),
            }
        }
        ast::Action::Token(ref expr, ref body) => {
            debug!("Token: {:?}", expr);
            if body.is_some() { panic!("Body unimplemented"); }
            Step::Token(expr::rexpr(session, scope, expr).into_message(shape_down))
        }
        ast::Action::On(ref expr, ref body) => {
            let mut body_scope = scope.child();
            let msg = expr::lexpr(session, &mut body_scope, expr).into_message(shape_up);
            let body_step = match *body {
                Some(ref body) => resolve_seq(session, &body_scope, shape_down, shape_up, body),
                None => Step::Nop,
            };
            Step::TokenTop(msg, box body_step)
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = expr::value(session, scope, count_ast);
            let child = box resolve_seq(session, scope, shape_down, shape_up, block);
            let any_up = child.any_up();
            Step::Repeat(count, child, any_up)
        }
        ast::Action::For(ref pairs, ref block) => {
            let mut body_scope = scope.child();
            let vars = pairs.iter().map(|&(ref name, ref expr)| {
                (body_scope.new_variable(session, name), expr::value(session, scope, expr))
            }).collect();

            let child = box resolve_seq(session, &body_scope, shape_down, shape_up, block);
            Step::Foreach(8, vars, child)
        }
    }
}

pub fn resolve_seq<'s>(session: &'s Session<'s>,
                  pscope: &Scope<'s>,
                  shape_down: &Shape,
                  shape_up: &Shape,
                  block: &ast::Block) -> Step {
    let mut scope = pscope.child();
    resolve_letdef(session, &mut scope, &block.lets);

    let steps = block.actions.iter().map(|action|
        resolve_action(session, &scope, shape_down, shape_up, action)
    ).collect();

    Step::Seq(steps)
}

pub fn resolve_letdef<'s>(session: &'s Session<'s>, scope: &mut Scope<'s>, lets: &[ast::LetDef]) {
    for &ast::LetDef(ref name, ref expr) in lets.iter() {
        let item = expr::rexpr(session, scope, expr);
        scope.bind(&name, item);
    }
}
