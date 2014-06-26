use context::Context;
use resolve::Params;
use expr::{Item, ValueItem};
use exec::{Step, EventStep};

pub struct Signal {
	id: uint,
}

impl Signal {
	pub fn new() -> Signal {
		Signal { id: 0 }
	}

	pub fn resolve_method_call(&self, _pctx: &Context, name: &str, params: &Params) -> Step {
		let param_values = params.positional.iter().map(|item| {
				// TODO: type check, check for poison
				match **item {
					ValueItem(_, ref down, ref up) => (down.clone(), up.clone()),
					_ => fail!("Non-values can't be included in a token")
				}
		}).collect();

		EventStep(name.to_string(), param_values)
	}

	pub fn get_property<'a>(&'a self, _ctx: &Context, _property: &str) -> Option<&'a Item<'a>> {
		None
	}
}
