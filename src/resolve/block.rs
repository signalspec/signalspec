use ast;

use session::Session;
use resolve::context::{ Context, SignalInfo };
use resolve::expr::{resolve_expr, resolve_pattern};
pub use exec::Step;
pub use resolve::scope::{ Scope, Item };

pub fn resolve_module<'s>(session: &'s Session<'s>, ast: &'s ast::Module) -> Scope<'s> {
    let mut scope = session.prelude.clone();

    for _import in ast.imports.iter() {
        panic!("Imports unimplemented");
    }

    let mut ctx = Context::new(session);
    resolve_letdef(&mut ctx, &mut scope, ast.lets.as_slice());

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
                        pctx: &Context<'s>,
                        signals: &mut SignalInfo,
                        param: Item<'s>,
                        body: Option<&EventBodyClosure<'s>>) -> Step {
        if body.is_some() { panic!("Body unimplemented"); }
        let mut ctx = pctx.child();
        let mut scope = self.parent_scope.child(); // Base on lexical parent
        resolve_pattern(&mut ctx, &mut scope, &self.ast.param, param);
        resolve_seq(&mut ctx, signals, &scope, &self.ast.block)
    }
}

/// A body associated with an event call
pub struct EventBodyClosure<'s> {
    ast: &'s ast::Block,
    parent_scope: Scope<'s>,
}

fn resolve_action<'s>(ctx: &mut Context<'s>,
                      signals: &mut SignalInfo,
                      scope: &Scope<'s>,
                      action: &'s ast::Action) -> Step {
    match *action {
        ast::Action::Seq(ref block) => resolve_seq(ctx, signals, scope, block),
        ast::Action::Call(ref expr, ref arg, ref body) => {
            let arg = resolve_expr(ctx, scope, arg);
            let body = body.as_ref().map(|x| {
                EventBodyClosure { ast: x, parent_scope: scope.child() }
            });

            match resolve_expr(ctx, scope, expr) {
                Item::Def(entity) => entity.resolve_call(ctx, signals, arg, body.as_ref()),
                _ => panic!("Not callable"),
            }
        }
        ast::Action::Token(ref expr, ref body) => {
            let mut cctx = ctx.child();
            if body.is_some() { panic!("Body unimplemented"); }
            let i = resolve_expr(&mut cctx, scope, expr);
            let msg = cctx.message_downward(signals, i);
            Step::Token(cctx.into_ops(), msg)
        }
        ast::Action::On(ref expr, ref body) => {
            let mut cctx = ctx.child();
            let mut body_scope = scope.child();
            let (message, item) = cctx.message_upward(signals);
            resolve_pattern(&mut cctx, &mut body_scope, expr, item);
            let body_step = match *body {
                Some(ref body) => resolve_seq(&mut cctx, signals, &body_scope, body),
                None => Step::Nop,
            };
            Step::TokenTop(cctx.into_ops(), message, box body_step)
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = resolve_expr(ctx, scope, count_ast);
            let (_t, d, u) = ctx.item_to_refs(&count);
            Step::Repeat((d, u), box resolve_seq(ctx, signals, scope, block))
        }
    }
}

fn resolve_seq<'s>(ctx: &mut Context<'s>, signals: &mut SignalInfo, pscope: &Scope<'s>, block: &'s ast::Block) -> Step {
    let mut scope = pscope.child();
    resolve_letdef(ctx, &mut scope, block.lets.as_slice());

    let steps = block.actions.iter().map(|action| resolve_action(ctx, signals, &scope, action)).collect();

    Step::Seq(steps)
}

pub fn resolve_letdef<'s>(ctx: &mut Context<'s>, scope: &mut Scope<'s>, lets: &[ast::LetDef]) {
    for &ast::LetDef(ref name, ref expr) in lets.iter() {
        let item = resolve_expr(ctx, scope, expr);
        scope.bind(name[], item);
    }
}
