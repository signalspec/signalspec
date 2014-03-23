use context::Context;
use resolve::Params;
use exec::Step;
use expr::Item;

pub trait Entity<'s> {
	fn resolve_call(&self, _pctx: &Context, _params: &Params) -> Step {
		fail!("Entity is not callable");
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


type PrimitiveClosureFn<T> = fn (pctx: &Context, device: &T, params: &Params) -> Step;
pub struct PrimitiveClosure<'s, T> {
	device: &'s T,
	resolvefn: PrimitiveClosureFn<T>,
}
impl<'s, T> PrimitiveClosure<'s, T> {
	pub fn new(device: &'s T, resolvefn: PrimitiveClosureFn<T>) -> PrimitiveClosure<'s, T>{
		PrimitiveClosure {
			device: device,
			resolvefn: resolvefn,
		}
	}
}
impl<'s, T> Entity<'s> for PrimitiveClosure<'s, T> {
	fn resolve_call(&self, pctx: &Context, params: &Params) -> Step {
		(self.resolvefn)(pctx, self.device, params)
	}
}

