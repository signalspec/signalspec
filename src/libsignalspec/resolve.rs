use ast;
use std::hashmap::HashMap;
use session::Session;
use context::Context;
use arena::{Arena,TypedArena};

#[deriving(Clone)]
pub enum ScopeItem<'s> {
	ValueItem(ast::Value),
	EventItem(&'s EventCallable<'s>),
	EntityItem(&'s Entity<'s>),
}

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

	fn add_params(&mut self, param_defs: &[ast::ParamDef], param_values: &[ScopeItem<'s>]) {
		// TODO: keyword args, defaults
		if param_defs.len() != param_values.len() {
			fail!("Wrong number of parameters passed")
		}

		for (def, val) in param_defs.iter().zip(param_values.iter()) {
			// TODO: type check
			let v = val.clone();
			self.names.insert(def.name.to_owned(), v);
		}
	}

	pub fn get(&self, name: &str) -> Option<ScopeItem<'s>> {
		self.names.find_equiv(&name).map(|x| x.clone())
	}
}


pub trait EventCallable<'s> {
	fn resolve_call(&self, ctx: &mut Context, params: &[ScopeItem<'s>], body: Option<&EventBodyClosure>) -> Step ;
}

// A user-defined event
pub struct EventClosure<'s> {
	ast: &'s ast::Def,
	parentScope: Scope<'s>,
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
		scope.names.insert(def.name.to_owned(), EventItem(ed));
	}

	scope
}

fn eval_callable_expr<'s>(expr: &ast::Expr, scope: &Scope<'s>) -> Option<ScopeItem<'s>> {
	match *expr {
		ast::ValueExpr(ref val) => Some(ValueItem(val.clone())),
		ast::VarExpr(ref name) => scope.get(name.as_slice()),
		ast::DotExpr(ref lexpr, ref name) => {
			let l = eval_callable_expr(*lexpr, scope);
			match l {
				Some(EntityItem(ref e)) => e.events.find_equiv(&name.as_slice()).map(|x| EventItem(*x)),
				_ => None,
			}
		}
		_ => None,
	}
}

fn resolve_seq<'s>(pctx: &mut Context, scope: &Scope<'s>, block: &'s ast::Block) -> Step {
	let mut ctx = pctx.child();
	let mut scope = scope.clone();
	scope.add_lets(block.lets);

	let steps = block.actions.iter().map(|action| {
		let entity = eval_callable_expr(&action.entity, &scope);

		let body = action.body.as_ref().map(|x| 
			EventBodyClosure { ast: x, parentScope: scope.clone()
		});


		match entity {
			Some(EventItem(ref e)) => {
				e.resolve_call(&mut ctx, &[], body.as_ref())
			}
			None => fail!("Event not found: {:?} in {:?}", action.entity, scope.names.keys().collect::<~[&~str]>()),
			_ => fail!("Not an event"),
		}
	}).collect();

	SeqStep(steps)
}

impl<'s> EventCallable<'s> for EventClosure<'s> {
	fn resolve_call(&self, pctx: &mut Context, params: &[ScopeItem<'s>], body: Option<&EventBodyClosure>) -> Step {
		let mut ctx = pctx.child();
		let mut scope = self.parentScope.clone(); // Base on lexical parent

		scope.add_params(self.ast.params, params);
		CallStep(~resolve_seq(&mut ctx, &scope, &self.ast.block))
	}
}

pub fn resolve_body_call(ctx: &mut Context, body: &EventBodyClosure, params: &[ScopeItem]) -> Step {
	// TODO: parameters
	CallStep(~resolve_seq(ctx, &body.parentScope, &body.ast.block))
}

impl<'s, 'r> Clone for &'r EventCallable<'s> {
	fn clone(&self) -> &'r EventCallable<'s> { *self }
}

pub struct Entity<'s> {
	events: HashMap<~str, ~EventCallable:<'s> >,
}

pub trait StepHandler {

}

pub enum Step {
	NopStep,
	CallStep(~Step),
	SeqStep(~[Step]),
	PrimitiveStep(~StepHandler, Option<~Step>),
}

pub fn print_step_tree(s: &Step, indent: uint) {
	let i = " ".repeat(indent);
	match *s {
		NopStep => println!("{}NOP", i),
		CallStep(~ref c) => {
			println!("{}Call", i);
			print_step_tree(c, indent+1);
		}
		SeqStep(ref steps) => {
			println!("{}Seq", i)
			for c in steps.iter() {
				print_step_tree(c, indent+1);
			}
		}
		PrimitiveStep(_, ref body) => {
			println!("{}Primitive", i);
			match *body {
				Some(~ref body) => print_step_tree(body, indent+1),
				None => ()
			}
		}
	}
}

