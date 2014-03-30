use std::any::{Any, AnyRefExt};

use ast;
use context;
use context::{
	Context,
	ValueRef,
	Domain,
};
use resolve::{
	Params,
	resolve_body_call,
};
use exec::{
	Step,
		NopStep,
		SignalLevelStep,
};
use entity::{
	Entity,
};
use expr::{
	Item,
		EntityItem,
};

pub struct Signal {
	id: uint,
}

impl Signal {
	pub fn new() -> Signal {
		Signal { id: 0 }
	}
}

#[deriving(Clone)]
pub struct VirtualClockDomain {
	constraints: ~[(uint, ValueRef)],
}

impl VirtualClockDomain {
	pub fn new() -> VirtualClockDomain {
		VirtualClockDomain {
			constraints: ~[],
		}
	}
}

impl Domain for VirtualClockDomain {
	fn as_any<'a>(&'a self) -> &'a Any { self as &Any }
}

impl<'s> Entity<'s> for Signal {
	fn resolve_method_call(&self, pctx: &Context, name: &str, params: &Params) -> Step {
		match name {
			&"level" => {
				// TODO: check that it's from the right clock domain
				let body = params.body.as_ref().map_or(NopStep, |b| resolve_body_call(pctx, b, &Params::empty()));
				SignalLevelStep(self.id, false, ~body)
			},
			_ => fail!("Signal has no method `{}`", name)
		}
	}
}
