use ast;
use std::hashmap::HashMap;
use session::Session;
use context::{Context};
use arena::{Arena,TypedArena};

pub use ScopeItem = expr::Item;
pub use expr::{
	ValueItem,
	EntityItem,
	resolve_expr,
};
pub use exec::{
	Step,
		NopStep,
		CallStep,
		SeqStep,
		PrimitiveStep,
};
use entity::{
	Entity,
	PrimitiveCallable
};

#[deriving(Clone)]
pub struct Scope<'s>{
	names: HashMap<~str, ScopeItem<'s>>,
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

		for (def, val) in param_defs.iter().zip(param_values.positional.iter()) {
			// TODO: type check
			let v = val.clone();
			self.names.insert(def.name.to_owned(), v);
		}
	}

	pub fn get(&self, name: &str) -> Option<ScopeItem<'s>> {
		self.names.find_equiv(&name).map(|x| x.clone())
	}
}

pub struct Params<'s> {
	positional: ~[ScopeItem<'s>],
	body: Option<EventBodyClosure<'s>>,
}

impl<'s> Params<'s> {
	pub fn empty() -> Params {
		Params {
			positional: ~[],
			body: None,
		}
	}
}

// A body associated with an event call
pub struct EventBodyClosure<'s> {
	ast: &'s ast::ActionBody,
	parentScope: Scope<'s>,
}

pub fn resolve_module<'s>(pctx: &mut Context<'s>, scope: &Scope<'s>, ast: &'s ast::Module) -> Scope<'s> {
	let mut ctx = pctx.child();
	let mut scope: Scope<'s> = scope.clone();

	for import in ast.imports.iter() {
		fail!("Imports unimplemented");
	}

	scope.add_lets(ast.lets);

	for def in ast.defs.iter() {
		let ed = ctx.session.moduleDefArena.alloc(EventClosure{ ast:def, parentScope: scope.clone()});
		scope.names.insert(def.name.to_owned(), EntityItem(ed));
	}

	scope
}

fn resolve_seq<'s>(pctx: &mut Context<'s>, scope: &Scope<'s>, block: &'s ast::Block) -> Step {
	let mut ctx = pctx.child();
	let mut scope = scope.clone();
	scope.add_lets(block.lets);

	let steps = block.actions.iter().map(|action| {
		let entity = resolve_expr(&mut ctx, &scope, &action.entity);

		match entity {
			EntityItem(ref e) => {
				e.resolve_call(&mut ctx, &Params {
					positional: ~[],
					body: action.body.as_ref().map(|x| {
						EventBodyClosure { ast: x, parentScope: scope.clone()
					}}),
				})
			}
			_ => fail!("Not an event"),
		}
	}).collect();

	SeqStep(steps)
}

// A user-defined event
pub struct EventClosure<'s> {
	ast: &'s ast::Def,
	parentScope: Scope<'s>,
}

impl<'s> Entity<'s> for EventClosure<'s> {
	fn resolve_call(&self, pctx: &mut Context<'s>, params: &Params<'s>) -> Step {
		let mut ctx = pctx.child();
		let mut scope = self.parentScope.clone(); // Base on lexical parent

		scope.add_params(self.ast.params, params);
		CallStep(~resolve_seq(&mut ctx, &scope, &self.ast.block))
	}
}

pub fn resolve_body_call<'s>(ctx: &mut Context<'s>, body: &EventBodyClosure<'s>, params: &Params<'s>) -> Step {
	// TODO: parameters
	if params.body.is_some() {
		fail!("bug: body closure called with body");
	}
	CallStep(~resolve_seq(ctx, &body.parentScope, &body.ast.block))
}


pub fn time_call_fn<'s>(pctx: &mut Context<'s>, params: &Params<'s>) -> Step {
	if params.body.is_some() {
		fail!("time() does not accept a body");
	}
	pctx.domain.resolve_time(pctx, params)
}

