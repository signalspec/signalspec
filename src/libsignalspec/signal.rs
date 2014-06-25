use context::Context;
use resolve::Params;
use expr::Item;
use exec::{Step, EventStep};

pub struct Signal {
	id: uint,
}

impl Signal {
	pub fn new() -> Signal {
		Signal { id: 0 }
	}

	pub fn resolve_method_call(&self, _pctx: &Context, name: &str, _params: &Params) -> Step {
		EventStep(name.to_string())
	}

	pub fn get_property<'a>(&'a self, _ctx: &Context, _property: &str) -> Option<&'a Item<'a>> {
		None
	}
}

