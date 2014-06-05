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
use entity::{
	Entity,
};
use expr::ValueItem;

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
			"level" => {
				// TODO: check that it's from the right clock domain
				let value = match *params.positional.get(0) {
					ValueItem(_, Constant(ast::SymbolValue(ref v)), _) => (v.as_slice() == "h"),
					_ => fail!("Level arg must currently be constant")
				};
				let body = params.body.as_ref().map_or(NopStep, |b| resolve_body_call(pctx, b, &Params::empty()));
				SignalLevelStep(self.id, value, box body)
			},
			_ => fail!("Signal has no method `{}`", name)
		}
	}
}
