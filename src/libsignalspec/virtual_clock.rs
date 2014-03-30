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
	Step,
		NopStep,
		PrimitiveStep,
	resolve_body_call,
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

struct TimerHandler {
	constraints: ~[(uint, ValueRef)],
}

impl PrimitiveStep for TimerHandler {
	fn display(&self) -> ~str{
		format!("Time ({})", self.constraints.len())
	}

	fn exec(&self) {
		println!("Time!");
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
	fn resolve_time(&self, pctx: &Context, params: &Params) -> Step {
		PrimitiveStep(~TimerHandler{ constraints: self.constraints.clone() })
	}
}

impl<'s> Entity<'s> for Signal {
	fn resolve_method_call(&self, pctx: &Context, name: &str, params: &Params) -> Step {
		match name {
			&"level" => {
				let mut ctx = pctx.child();
				ctx.domain = match pctx.domain.as_any().as_ref::<VirtualClockDomain>() {
					// TODO: check that they come from the same parent
					Some(d) => pctx.session.arena.alloc(|| {
						let mut domain = d.clone();
						domain.constraints.push((0, context::Constant(ast::SymbolValue(~"h"))));
						domain
					}),
					None => fail!("Signal.level in the wrong clock domain")
				} as &Domain;
				params.body.as_ref().map_or(NopStep, |b| resolve_body_call(&mut ctx, b, &Params::empty()))
			},
			_ => fail!("Signal has no method `{}`", name)
		}
	}
}
