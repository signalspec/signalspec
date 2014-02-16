use std::hashmap::HashMap;
use session::Session;
use ast;
use context;
use context::{
	Context,
	ValueRef,
	Domain,
};
use resolve::{
	ScopeItem,
	Entity,
	EventCallable,
	Step,
		NopStep,
		PrimitiveStep,
	StepHandler,
	EventBodyClosure,
	resolve_body_call,
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

struct Wire {
	id: uint,
}

struct WireGroup {
	wire_ids: ~[~str],
}

///

type PrimitiveResolveFn<T> = fn <'s> (ctx: &mut Context<'s>, device: &'s T, params: &[ScopeItem<'s>], body: Option<&EventBodyClosure<'s>>) -> Step;

struct PrimitiveCallable<'s, T> {
	device: &'s T,
	resolvefn: PrimitiveResolveFn<T>,
}

impl<'s, T> PrimitiveCallable<'s, T> {
	fn new(device: &'s T, resolvefn: PrimitiveResolveFn<T>) -> PrimitiveCallable<'s, T>{
		PrimitiveCallable {
			device: device,
			resolvefn: resolvefn,
		}
	}
}

impl<'s, T> EventCallable<'s> for PrimitiveCallable<'s, T> {
	fn resolve_call(&self, ctx: &mut Context<'s>, params: &[ScopeItem<'s>], body: Option<&EventBodyClosure<'s>>) -> Step {
		(self.resolvefn)(ctx, self.device, params, body)
	}
}

fn make_entity<'s, T:'static>(device: &'s T, events: &[(&'static str, PrimitiveResolveFn<T>)]) -> Entity<'s> {
	Entity {
		events: events.iter().map(|&(n, f)| {
			(n.to_owned(), ~PrimitiveCallable::new(device, f) as ~EventCallable:<'s>)
		}).collect()
	}
}

///

fn resolve_wire_level<'s>(pctx: &mut Context<'s>, _: &(), params: &[ScopeItem<'s>], body: Option<&EventBodyClosure<'s>>) -> Step {
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
	body.map_or(NopStep, |b| resolve_body_call(&mut ctx, b, &[]))
}

struct WireLevelHandler {
	dummy: uint,
}


impl StepHandler for WireLevelHandler {}

struct TimerHandler {
	constraints: ~[(uint, ValueRef)],
}

impl StepHandler for TimerHandler {
	fn display(&self) -> ~str{
		format!("Time ({})", self.constraints.len())
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
	fn resolve_time<'s>(&self, ctx: &mut Context<'s>, params: &[ScopeItem<'s>]) -> Step {
		PrimitiveStep(~TimerHandler{ constraints: self.constraints.clone() }, None)
	}
}

static dummy_device: () = ();

pub fn wire_config() -> Entity {
	make_entity(&dummy_device, &[
		("level", resolve_wire_level),
	])
}
