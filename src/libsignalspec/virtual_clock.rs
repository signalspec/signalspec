use ast;
use context;
use context::{
	Context,
	ValueRef,
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

pub struct Signal {
	id: uint,
}

impl Signal {
	pub fn new() -> Signal {
		Signal { id: 0 }
	}
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
