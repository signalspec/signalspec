use context::Context;
use resolve::Params;
use exec::Step;
use expr::Item;

pub trait Entity<'s> {
	fn resolve_call(&self, _pctx: &Context, _params: &Params) -> Step {
		fail!("Entity is not callable");
	}

	fn resolve_method_call(&self, _pctx: &Context, _name: &str, _params: &Params) -> Step {
		fail!("Entity has no methods");
	}

	fn get_property<'a>(&'a self, _ctx: &Context, _property: &str) -> Option<Item<'a>> {
		None
	}
	// fn as_stream(&self, ctx: &mut Context<'s>) -> Stream;
}

impl<'s, 'r> Clone for &'r Entity<'s> {
	fn clone(&self) -> &'r Entity<'s> { *self }
}


pub type PrimitiveCallable = fn (pctx: &Context, params: &Params) -> Step;
impl<'s> Entity<'s> for PrimitiveCallable {
	fn resolve_call(&self, pctx: &Context, params: &Params) -> Step {
		(*self)(pctx, params)
	}
}
