use context::Context;
use scope::{Item, ValueItem, Params};
use exec::{Step, EventStep};
use session::Id;

pub enum SignalIdPhantom{}
pub type SignalId = Id<SignalIdPhantom>;

pub struct Signal {
	id: SignalId,
}

impl Signal {
	pub fn new(id: SignalId) -> Signal {
		Signal { id: id}
	}

	pub fn resolve_method_call(&self, _pctx: &Context, name: &str, params: &Params) -> Step {
		let param_values = params.positional.iter().map(|item| {
				// TODO: type check, check for poison
				match **item {
					ValueItem(_, ref down, ref up) => (down.clone(), up.clone()),
					_ => fail!("Non-values can't be included in a token")
				}
		}).collect();

		EventStep(self.id, name.to_string(), param_values)
	}

	pub fn get_property<'a>(&'a self, _ctx: &Context, _property: &str) -> Option<&'a Item<'a>> {
		None
	}
}
