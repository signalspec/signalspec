use ast;

use resolve::context::Context;
use resolve::expr::resolve_expr;
pub use exec::{
	Step,
		NopStep,
		SeqStep,
		TokenStep,
		RepeatStep,
};
pub use resolve::scope::{Scope, Item, ValueItem, DefItem};
// A body associated with an event call
pub struct EventBodyClosure<'s> {
	ast: &'s ast::Block,
	parent_scope: Scope<'s>,
}

pub fn resolve_module<'s>(pctx: &Context<'s>, pscope: &Scope<'s>, ast: &'s ast::Module) -> Scope<'s> {
	let ctx = pctx.child();
	let mut scope = pscope.clone();

	for _import in ast.imports.iter() {
		fail!("Imports unimplemented");
	}

	scope.add_lets(ast.lets.as_slice());

	for def in ast.defs.iter() {
		let ed = DefItem(ctx.session.closure_arena.alloc(EventClosure{ ast:def, parent_scope: scope.clone() }));
		scope.names.insert(def.name.to_string(), ed);
	}

	scope
}


fn resolve_action<'s>(ctx: &mut Context<'s>, scope: &Scope<'s>, action: &'s ast::Action) -> Step {
	match *action {
		ast::ActionSeq(ref block) => resolve_seq(ctx, scope, block),
		ast::ActionCall(ref expr, ref arg, ref body) => {
			let arg = resolve_expr(ctx, scope, arg);
			let body = body.as_ref().map(|x| {
				EventBodyClosure { ast: x, parent_scope: scope.child() }
			});

			match resolve_expr(ctx, scope, expr) {
				DefItem(entity) => entity.resolve_call(ctx, &arg, body.as_ref()),
				_ => fail!("Not callable"),
			}
		}
		ast::ActionToken(ref expr, ref body) => {
			if body.is_some() { fail!("Body unimplemented"); }
			let (down, up) = resolve_expr(ctx, scope, expr).flatten();
			TokenStep(down, up)
		}
		ast::ActionRepeat(ref block) => {
			RepeatStep(box resolve_seq(ctx, scope, block))
		}
	}
}

fn resolve_seq<'s>(pctx: &Context<'s>, pscope: &Scope<'s>, block: &'s ast::Block) -> Step {
	let mut ctx = pctx.child();
	let mut scope = pscope.child();
	scope.add_lets(block.lets.as_slice());

	let steps = block.actions.iter().map(|action| resolve_action(&mut ctx, &scope, action)).collect();

	SeqStep(steps)
}

// A user-defined event
pub struct EventClosure<'s> {
	ast: &'s ast::Def,
	parent_scope: Scope<'s>,
}

impl<'s> EventClosure<'s> {
	pub fn resolve_call(&self, pctx: &Context<'s>, param: &Item<'s>, body: Option<&EventBodyClosure<'s>>) -> Step {
		if body.is_some() { fail!("Body unimplemented"); }
		let ctx = pctx.child();
		let mut scope = self.parent_scope.child(); // Base on lexical parent

		scope.add_param(&self.ast.param, param.clone());
		resolve_seq(&ctx, &scope, &self.ast.block)
	}
}
