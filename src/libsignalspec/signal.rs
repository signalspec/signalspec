use context::Context;
use resolve::Params;
use expr::Item;
use exec::Step;

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

