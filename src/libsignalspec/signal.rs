use ast;
use context;
use context::{
	Context,
	ValueRef,
	Constant,
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
use expr::{Item, ValueItem};

pub struct Signal {
	id: uint,
}

impl Signal {
	pub fn new() -> Signal {
		Signal { id: 0 }
	}

	pub fn resolve_method_call(&self, _pctx: &Context, _name: &str, _params: &Params) -> Step {
		fail!("Entity has no methods");
	}

	pub fn get_property<'a>(&'a self, _ctx: &Context, _property: &str) -> Option<&'a Item<'a>> {
		None
	}
}

