use ast;

use session::Session;
use resolve::expr;
pub use exec::Step;
pub use resolve::scope::{ Scope, Item };

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
                        param: Item<'s>,
                        body: Option<&EventBodyClosure<'s>>) -> Step {
        if body.is_some() { panic!("Body unimplemented"); }
        let mut scope = self.parent_scope.child(); // Base on lexical parent
        expr::assign(session, &mut scope, &self.ast.param, param);
        resolve_seq(session, &scope, &self.ast.block)
    }
}

/// A body associated with an event call
pub struct EventBodyClosure<'s> {
    ast: &'s ast::Block,
    parent_scope: Scope<'s>,
}

fn resolve_action<'s>(session: &'s Session<'s>,
                      scope: &Scope<'s>,
                      action: &'s ast::Action) -> Step {
    match *action {
        ast::Action::Seq(ref block) => resolve_seq(session, scope, block),
        ast::Action::Call(ref expr, ref arg, ref body) => {
            let arg = expr::rexpr(session, scope, arg);
            let body = body.as_ref().map(|x| {
                EventBodyClosure { ast: x, parent_scope: scope.child() }
            });

            match expr::rexpr(session, scope, expr) {
                Item::Def(entity) => entity.resolve_call(session, arg, body.as_ref()),
                _ => panic!("Not callable"),
            }
        }
        ast::Action::Token(ref expr, ref body) => {
            debug!("Token: {:?}", expr);
            if body.is_some() { panic!("Body unimplemented"); }
            Step::Token(expr::rexpr(session, scope, expr).into_message())
        }
        ast::Action::On(ref expr, ref body) => {
            let mut body_scope = scope.child();
            let msg = expr::lexpr(session, &mut body_scope, expr).into_message();
            let body_step = match *body {
                Some(ref body) => resolve_seq(session, &body_scope, body),
                None => Step::Nop,
            };
            Step::TokenTop(msg, box body_step)
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = expr::value(session, scope, count_ast);
            Step::Repeat(count, box resolve_seq(session, scope, block))
        }
    }
}

fn resolve_seq<'s>(session: &'s Session<'s>, pscope: &Scope<'s>, block: &'s ast::Block) -> Step {
    let mut scope = pscope.child();
    resolve_letdef(session, &mut scope, &block.lets);

    let steps = block.actions.iter().map(|action| resolve_action(session, &scope, action)).collect();

    Step::Seq(steps)
}

pub fn resolve_letdef<'s>(session: &'s Session<'s>, scope: &mut Scope<'s>, lets: &[ast::LetDef]) {
    for &ast::LetDef(ref name, ref expr) in lets.iter() {
        let item = expr::rexpr(session, scope, expr);
        scope.bind(&name, item);
    }
}
