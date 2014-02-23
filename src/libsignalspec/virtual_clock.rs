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
	PrimitiveClosure,
};
use expr::{
	Item,
		EntityItem,
};

pub trait DigitalSource {
	/// Vector of wire names. Indicies in this array are the IDs used in other methods.
	fn wires(&self) -> &[&str];

	/// Iterator of (time, wire, state) events
	fn events<'s>(&'s self) -> &'s Iterator<(u64, uint, bool)>;
}

pub trait DigitalSink {
	/// Set wire names. This must be called only once
	fn init(&mut self, wires: &[&str]);

	/// Append an event. Time must be non-decreasing
	fn event(&mut self, time: u64, wire: uint, state: bool);
}

pub struct Wire {
	id: uint,
}

impl Wire {
	pub fn new() -> Wire {
		Wire { id: 0 }
	}
}

struct WireGroup {
	wire_ids: ~[~str],
}


fn resolve_wire_level<'s>(pctx: &mut Context<'s>, device: &Wire, params: &Params<'s>) -> Step {
	let mut ctx = pctx.child();
	ctx.domain = match pctx.domain.as_any().as_ref::<VirtualClockDomain>() {
		// TODO: check that they come from the same parent
		Some(d) => pctx.session.arena.alloc(|| {
			let mut domain = d.clone();
			domain.constraints.push((0, context::Constant(ast::SymbolValue(~"h"))));
			domain
		}),
		None => fail!("wire.level in the wrong clock domain")
	} as &Domain;
	params.body.as_ref().map_or(NopStep, |b| resolve_body_call(&mut ctx, b, &Params::empty()))
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
	fn resolve_time<'s>(&self, ctx: &mut Context<'s>, params: &Params<'s>) -> Step {
		PrimitiveStep(~TimerHandler{ constraints: self.constraints.clone() })
	}
}

impl<'s> Entity<'s> for Wire {
	fn get_property(&self, ctx: &Context<'s>, prop: &str) -> Option<Item<'s>> {
		// TODO: I wish this could return by value instead of allocating
		// This should at least be cached

		// mozilla/rust#5121 (see comment on the Entity trait)
		let sself: &'s Wire = unsafe{ ::std::cast::transmute_region(self) };

		match prop {
			&"level" => {
				let p = ctx.session.arena.alloc(|| PrimitiveClosure::new(sself, resolve_wire_level));
				Some(EntityItem(p))
			}
			_ => None
		}
	}
}
