use std::hashmap::HashMap;
use session::Session;
use context::Context;

use resolve::{ScopeItem, Entity, EventCallable,Step,PrimitiveStep,StepHandler,EventBodyClosure, resolve_body_call};

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

struct WireLevel {
	dummy: uint,
}

///

type PrimitiveResolveFn<T> = fn <'s> (ctx: &mut Context, device: &'s T, params: &[ScopeItem<'s>], body: Option<&EventBodyClosure>) -> Step;

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
	fn resolve_call(&self, ctx: &mut Context, params: &[ScopeItem<'s>], body: Option<&EventBodyClosure>) -> Step {
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

fn resolve_wire_level<'s>(ctx: &mut Context, _: &(), params: &[ScopeItem<'s>], body: Option<&EventBodyClosure>) -> Step {
	PrimitiveStep(~WireLevelHandler{dummy: 0}, body.map(|b| ~resolve_body_call(ctx, b, &[])))
}

struct WireLevelHandler {
	dummy: uint,
}


impl StepHandler for WireLevelHandler {}

fn resolve_time<'s>(ctx: &mut Context, _: &(), params: &[ScopeItem<'s>], body: Option<&EventBodyClosure>) -> Step {
	PrimitiveStep(~TimerHandler, None)
}

struct TimerHandler;
impl StepHandler for TimerHandler {}

struct ClockDomain {
	dummy: uint,
}

static dummy_device: () = ();

pub fn timer<'s>() -> ~EventCallable:<'s> {
	~PrimitiveCallable::new(&dummy_device, resolve_time) as ~EventCallable:<'s>
}

pub fn wire_config() -> Entity {
	make_entity(&dummy_device, &[
		("level", resolve_wire_level),
	])
}

