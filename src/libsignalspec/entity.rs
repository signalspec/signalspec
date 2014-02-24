use context::Context;
use resolve::Params;
use exec::Step;
use expr::Item;

pub trait Entity<'s> {
	fn resolve_call(&self, _ctx: &mut Context, _params: &Params) -> Step {
		fail!("Entity is not callable");
	}

	//TODO: this should be &'s self, but that's broken (mozilla/rust#5121)
	fn get_property(&self, _ctx: &Context, _property: &str) -> Option<Item<'s>> {
		None
	}
	// fn as_stream(&self, ctx: &mut Context<'s>) -> Stream;
}

impl<'s, 'r> Clone for &'r Entity<'s> {
	fn clone(&self) -> &'r Entity<'s> { *self }
}


pub type PrimitiveCallable = fn (ctx: &mut Context, params: &Params) -> Step;
impl<'s> Entity<'s> for PrimitiveCallable {
	fn resolve_call(&self, ctx: &mut Context, params: &Params) -> Step {
		(*self)(ctx, params)
	}
}


type PrimitiveClosureFn<T> = fn (ctx: &mut Context, device: &T, params: &Params) -> Step;
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
	fn resolve_call(&self, ctx: &mut Context, params: &Params) -> Step {
		(self.resolvefn)(ctx, self.device, params)
	}
}

