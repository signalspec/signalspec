use ast;

use session::Session;
use resolve::context::{ Context, SignalInfo };
use resolve::expr::{resolve_expr, resolve_pattern};
pub use exec::{
    Step,
        NopStep,
        SeqStep,
        TokenStep,
        TokenTopStep,
        RepeatStep,
};
pub use resolve::scope::{Scope, Item, ValueItem, DefItem};
// A body associated with an event call
pub struct EventBodyClosure<'s> {
    ast: &'s ast::Block,
    parent_scope: Scope<'s>,
}

pub fn resolve_module<'s>(session: &'s Session<'s>, ast: &'s ast::Module) -> Scope<'s> {
    let mut scope = session.prelude.clone();

    for _import in ast.imports.iter() {
        fail!("Imports unimplemented");
    }

    scope.add_lets(ast.lets.as_slice());

    for def in ast.defs.iter() {
        let ed = DefItem(session.closure_arena.alloc(EventClosure{ ast:def, parent_scope: scope.clone() }));
        scope.names.insert(def.name.to_string(), ed);
    }

    scope
}

fn resolve_action<'s>(ctx: &mut Context<'s>,
                      signals: &mut SignalInfo,
                      scope: &Scope<'s>,
                      action: &'s ast::Action) -> Step {
    match *action {
        ast::ActionSeq(ref block) => resolve_seq(ctx, signals, scope, block),
        ast::ActionCall(ref expr, ref arg, ref body) => {
            let arg = resolve_expr(ctx, scope, arg);
            let body = body.as_ref().map(|x| {
                EventBodyClosure { ast: x, parent_scope: scope.child() }
            });

            match resolve_expr(ctx, scope, expr) {
                DefItem(entity) => entity.resolve_call(ctx, signals, arg, body.as_ref()),
                _ => fail!("Not callable"),
            }
        }
        ast::ActionToken(ref expr, ref body) => {
            let mut cctx = ctx.child();
            if body.is_some() { fail!("Body unimplemented"); }
            let i = resolve_expr(&mut cctx, scope, expr);
            let msg = cctx.message_downward(signals, i);
            TokenStep(cctx.into_ops(), msg)
        }
        ast::ActionOn(ref expr, ref body) => {
            let mut cctx = ctx.child();
            let mut body_scope = scope.child();
            let (message, item) = cctx.message_upward(signals);
            resolve_pattern(&mut cctx, &mut body_scope, expr, item);
            let body_step = match *body {
                Some(ref body) => resolve_seq(&mut cctx, signals, &body_scope, body),
                None => NopStep,
            };
            TokenTopStep(cctx.into_ops(), message, box body_step)
        }
        ast::ActionRepeat(ref count_ast, ref block) => {
            let count = resolve_expr(ctx, scope, count_ast);
            let (_t, d, u) = ctx.item_to_refs(&count);
            RepeatStep((d, u), box resolve_seq(ctx, signals, scope, block))
        }
    }
}

fn resolve_seq<'s>(pctx: &Context<'s>, signals: &mut SignalInfo, pscope: &Scope<'s>, block: &'s ast::Block) -> Step {
    let mut ctx = pctx.child();
    let mut scope = pscope.child();
    scope.add_lets(block.lets.as_slice());

    let steps = block.actions.iter().map(|action| resolve_action(&mut ctx, signals, &scope, action)).collect();

    SeqStep(steps)
}

// A user-defined event
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
        if body.is_some() { fail!("Body unimplemented"); }
        let mut ctx = pctx.child();
        let mut scope = self.parent_scope.child(); // Base on lexical parent
        resolve_pattern(&mut ctx, &mut scope, &self.ast.param, param);
        resolve_seq(&ctx, signals, &scope, &self.ast.block)
    }
}
