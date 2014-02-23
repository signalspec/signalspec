use context::Context;
use resolve::Params;
use exec::Step;
use expr::Item;

pub trait Entity<'s> {
	fn resolve_call(&self, ctx: &mut Context<'s>, params: &Params<'s>) -> Step {
		fail!("Entity is not callable");
	}

	//TODO: this should be &'s self, but that's broken (mozilla/rust#5121)
	fn get_property(&self, ctx: &Context<'s>, property: &str) -> Option<Item<'s>> {
		None
	}
	// fn as_stream(&self, ctx: &mut Context<'s>) -> Stream;
}

impl<'s, 'r> Clone for &'r Entity<'s> {
	fn clone(&self) -> &'r Entity<'s> { *self }
}


pub type PrimitiveCallable = fn<'s>(ctx: &mut Context<'s>, params: &Params<'s>) -> Step;
impl<'s> Entity<'s> for PrimitiveCallable {
	fn resolve_call(&self, ctx: &mut Context<'s>, params: &Params<'s>) -> Step {
		(*self)(ctx, params)
	}
}


type PrimitiveClosureFn<T> = fn <'s> (ctx: &mut Context<'s>, device: &'s T, params: &Params<'s>) -> Step;
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
	fn resolve_call(&self, ctx: &mut Context<'s>, params: &Params<'s>) -> Step {
		(self.resolvefn)(ctx, self.device, params)
	}
}

