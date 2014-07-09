use ast;

use resolve::context::Context;
use resolve::expr::resolve_expr;
pub use exec::{
	Step,
		NopStep,
		CallStep,
		SeqStep,
		PrimitiveStep,
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

fn resolve_seq<'s>(pctx: &Context<'s>, pscope: &Scope<'s>, block: &'s ast::Block) -> Step {
	let mut ctx = pctx.child();
	let mut scope = pscope.child();
	scope.add_lets(block.lets.as_slice());

	let steps = block.actions.iter().map(|action| {
		let params = &Params {
			positional: action.positional.iter().map(|a| resolve_expr(&mut ctx, &scope, a)).collect(),
			body: action.body.as_ref().map(|x| {
				EventBodyClosure { ast: x, parentScope: scope.child() }
			})
		};

		match action.action {
			ast::ActionDef(ref entityitem) => {
				match *resolve_expr(&mut ctx, &scope, entityitem) {
					DefItem(ref entity) => entity.resolve_call(&ctx, params),
					_ => fail!("Not an action"),
				}
			}
			ast::ActionEntity(ref entityitem, ref method) => {
				match *resolve_expr(&mut ctx, &scope, entityitem) {
					SignalItem(ref signal) => signal.resolve_method_call(&ctx, method.as_slice(), params),
					_ => fail!("Not a signal"),
				}
			}
		}
	}).collect();

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
		CallStep(box resolve_seq(&ctx, &scope, &self.ast.block))
	}
}
