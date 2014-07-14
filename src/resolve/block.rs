use ast;

use resolve::context::Context;
use resolve::expr::resolve_expr;
pub use exec::{
	Step,
		NopStep,
		SeqStep,
		PrimitiveStep,
		RepeatStep,
};
pub use resolve::scope::{Scope, Item, ValueItem, SignalItem, DefItem, Params};
// A body associated with an event call
pub struct EventBodyClosure<'s> {
	ast: &'s ast::ActionBody,
	parentScope: Scope<'s>,
}

pub fn resolve_module<'s>(pctx: &Context<'s>, pscope: &Scope<'s>, ast: &'s ast::Module) -> Scope<'s> {
	let ctx = pctx.child();
	let mut scope = pscope.clone();

	for _import in ast.imports.iter() {
		fail!("Imports unimplemented");
	}

	scope.add_lets(ast.lets.as_slice());

	for def in ast.defs.iter() {
		let ed = ctx.session.itemArena.alloc(DefItem(EventClosure{ ast:def, parentScope: scope.clone() }));
		scope.names.insert(def.name.to_string(), ed);
	}

	scope
}

fn resolve_call_params<'s>(ctx: &mut Context<'s>, scope: &Scope<'s>, call: &'s ast::Call) -> Params<'s> {
	Params {
		positional: call.positional.iter().map(|a| resolve_expr(ctx, scope, a)).collect(),
		body: call.body.as_ref().map(|x| {
			EventBodyClosure { ast: x, parentScope: scope.child() }
		})
	}
}

fn resolve_action<'s>(ctx: &mut Context<'s>, scope: &Scope<'s>, action: &'s ast::Action) -> Step {
	match *action {
		ast::ActionSeq(ref block) => resolve_seq(ctx, scope, block),
		ast::ActionCall(ref expr, ref call) => {
			let params = resolve_call_params(ctx, scope, call);
			match *resolve_expr(ctx, scope, expr) {
				DefItem(ref entity) => entity.resolve_call(ctx, &params),
				_ => fail!("Not an action"),
			}
		}
		ast::ActionToken(ref expr, ref method, ref call) => {
			let params = resolve_call_params(ctx, scope, call);
			match *resolve_expr(ctx, scope, expr) {
				SignalItem(ref signal) => signal.resolve_method_call(ctx, method.as_slice(), &params),
				_ => fail!("Not a signal"),
			}
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
	parentScope: Scope<'s>,
}

impl<'s> EventClosure<'s> {
	pub fn resolve_call(&self, pctx: &Context<'s>, params: &Params<'s>) -> Step {
		let ctx = pctx.child();
		let mut scope = self.parentScope.child(); // Base on lexical parent

		scope.add_params(self.ast.params.as_slice(), params);
		resolve_seq(&ctx, &scope, &self.ast.block)
	}
}
