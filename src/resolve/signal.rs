use resolve::context::Context;
use resolve::scope::{Item, ValueItem};
use resolve::block::{EventBodyClosure};
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

	pub fn resolve_method_call(&self, _pctx: &Context, param: &Item, body: Option<&EventBodyClosure>) -> Step {
		if body.is_some() { fail!("Body unimplemented"); }
		let (down, up) = match *param {
			ValueItem(_, ref down, ref up) => (down.clone(), up.clone()),
			_ => fail!("Non-values can't be included in a token")
		};

		EventStep(self.id, down, up)
	}

	pub fn get_property<'a>(&'a self, _ctx: &Context, _property: &str) -> Option<&'a Item<'a>> {
		None
	}
}
