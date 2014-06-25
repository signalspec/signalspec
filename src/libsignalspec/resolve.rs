use ast;
use std::collections::hashmap::HashMap;
use context::Context;

pub use expr::{
	Item,
	ValueItem,
	SignalItem,
	DefItem,
	resolve_expr,
};
pub use exec::{
	Step,
		NopStep,
		CallStep,
		SeqStep,
		PrimitiveStep,
};

#[deriving(Clone)]
pub struct Scope<'s>{
	pub names: HashMap<String, &'s Item<'s>>,
}

impl<'s> Scope<'s> {
	pub fn new() -> Scope<'s> {
		Scope {
			names: HashMap::new(),
		}
	}

	fn add_lets(&mut self, lets: &[ast::LetDef]) {
		for letdef in lets.iter() {
			fail!("Let unimplemented");
		}
	}

	fn add_params(&mut self, param_defs: &[ast::ParamDef], param_values: &Params<'s>) {
		// TODO: keyword args, defaults
		if param_defs.len() != param_values.positional.len() {
			fail!("Wrong number of parameters passed")
		}

		for (def, &val) in param_defs.iter().zip(param_values.positional.iter()) {
			// TODO: type check
			self.names.insert(def.name.to_string(), val);
		}
	}

	pub fn get(&self, name: &str) -> Option<&'s Item<'s>> {
		self.names.find_equiv(&name).map(|x| x.clone())
	}

	pub fn child(&self) -> Scope<'s> {
		Scope {
			names: self.names.clone(),
		}
	}
}

pub struct Params<'s> {
	pub positional: Vec<&'s Item<'s>>,
	pub body: Option<EventBodyClosure<'s>>,
}

impl<'s> Params<'s> {
	pub fn empty() -> Params {
		Params {
			positional: Vec::new(),
			body: None,
		}
	}
}

// A body associated with an event call
pub struct EventBodyClosure<'s> {
	ast: &'s ast::ActionBody,
	parentScope: Scope<'s>,
}

pub fn resolve_module<'s>(pctx: &Context<'s>, pscope: &Scope<'s>, ast: &'s ast::Module) -> Scope<'s> {
	let ctx = pctx.child();
	let mut scope = pscope.clone();

	for import in ast.imports.iter() {
		fail!("Imports unimplemented");
	}

	scope.add_lets(ast.lets.as_slice());

	for def in ast.defs.iter() {
		let ed = ctx.session.itemArena.alloc(DefItem(EventClosure{ ast:def, parentScope: scope.clone() }));
		scope.names.insert(def.name.to_string(), ed);
	}

	scope
}

fn resolve_seq<'s>(pctx: &Context<'s>, pscope: &Scope<'s>, block: &'s ast::Block) -> Step {
	let mut ctx = pctx.child();
	let mut scope = pscope.child();
	scope.add_lets(block.lets.as_slice());

	let steps = block.actions.iter().map(|action| {
		let params = &Params {
			positional: action.positional.iter().map(|a| resolve_expr(&mut ctx, &scope, a)).collect(),
			body: action.body.as_ref().map(|x| {
				EventBodyClosure { ast: x, parentScope: scope.child() }
			})
		};

		match action.action {
			ast::ActionDef(ref entityitem) => {
				match *resolve_expr(&mut ctx, &scope, entityitem) {
					DefItem(ref entity) => entity.resolve_call(&ctx, params),
					_ => fail!("Not an action"),
				}
			}
			ast::ActionEntity(ref entityitem, ref method) => {
				match *resolve_expr(&mut ctx, &scope, entityitem) {
					SignalItem(ref signal) => signal.resolve_method_call(&ctx, method.as_slice(), params),
					_ => fail!("Not a signal"),
				}
			}
		}
	}).collect();

	SeqStep(steps)
}

// A user-defined event
pub struct EventClosure<'s> {
	ast: &'s ast::Def,
	parentScope: Scope<'s>,
}

impl<'s> EventClosure<'s> {
	pub fn resolve_call(&self, pctx: &Context<'s>, params: &Params<'s>) -> Step {
		let ctx = pctx.child();
		let mut scope = self.parentScope.child(); // Base on lexical parent

		scope.add_params(self.ast.params.as_slice(), params);
		CallStep(box resolve_seq(&ctx, &scope, &self.ast.block))
	}
}


pub fn resolve_body_call<'s>(ctx: &Context<'s>, body: &EventBodyClosure<'s>, params: &Params<'s>) -> Step {
	// TODO: parameters
	if params.body.is_some() {
		fail!("bug: body closure called with body");
	}
	CallStep(box resolve_seq(ctx, &body.parentScope, &body.ast.block))
}
