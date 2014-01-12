use ast;
use std::hashmap::HashMap;
use extra::arena::TypedArena;

pub struct Session<'ast, 'ses> {
	defArena: TypedArena<EventDef<'ast, 'ses>>,
}

impl<'ast, 'ses> Session<'ast, 'ses> {
	pub fn new() -> Session {
		Session {
			defArena: TypedArena::new(),
		}
	}
}

struct Module<'ast, 'ses>{
	ast: &'ast ast::Module,
	scope: Scope<'ast, 'ses>,
}

trait EventCallable<'ast, 'ses> {
	fn resolve_call(&self, session: &'ses Session, params: &[ScopeItem<'ast, 'ses>]) -> Step ;
}

// A user-defined event
struct EventDef<'ast, 'ses> {
	ast: &'ast ast::Def,
	parentScope: Scope<'ast, 'ses>,
}

fn eval_callable_expr<'ast, 'ses>(expr: &ast::Expr, scope: &Scope<'ast, 'ses>) -> Option<ScopeItem<'ast, 'ses>> {
	match *expr {
		ast::ValueExpr(ref val) => Some(Value(val.clone())),
		ast::VarExpr(ref name) => scope.get(name.as_slice()),
		ast::DotExpr(ref lexpr, ref name) => {
			let l = eval_callable_expr(*lexpr, scope);
			match l {
				Some(Entity(ref e)) => e.events.find_equiv(&name.as_slice()).map(|x| Event(*x)),
				_ => None,
			}
		}
		_ => None,
	}
}

impl<'ast, 'ses> EventCallable<'ast, 'ses> for EventDef<'ast, 'ses> {
	fn resolve_call(&self, session: &'ses Session, params: &[ScopeItem<'ast, 'ses>]) -> Step {
		let mut scope = self.parentScope.clone(); // Base on lexical parent
		scope.add_params(self.ast.params, params);
		scope.add_lets(self.ast.block.lets);

		let mut steps = ~[];

		for action in self.ast.block.actions.iter() {
			let entity = eval_callable_expr(&action.entity, &scope);
			println!("entity: {:?}, {:?}", &action.entity, &entity);

			match entity {
				Some(Event(ref e)) => {
					steps.push(e.resolve_call(session, &[]));
				}
				None => fail!("Event not found"),
				_ => fail!("Not an event"),
			}
		}

		BlockStep(~SeqStep(steps))
	}
}

impl<'ast, 'ses> Clone for &'ses EventCallable<'ast, 'ses> {
	fn clone(&self) -> &'ses EventCallable<'ast, 'ses> { *self }
}

#[deriving(Clone)]
struct Entity<'ast, 'ses> {
	events: HashMap<~str, &'ses EventCallable<'ast, 'ses>>,
}

#[deriving(Clone)]
pub enum ScopeItem<'ast, 'ses> {
	Value(ast::Value),
	Event(&'ses EventCallable<'ast, 'ses>),
	Entity(Entity<'ast, 'ses>),
}

#[deriving(Clone)]
struct Scope<'ast, 'ses>{
	names: HashMap<~str, ScopeItem<'ast, 'ses>>,
}

impl<'ast, 'ses> Scope<'ast, 'ses> {
	fn new() -> Scope<'ast, 'ses> {
		Scope {
			names: HashMap::new(),
		}
	}

	fn add_lets(&mut self, lets: &'ast [ast::LetDef]) {
		for letdef in lets.iter() {
			fail!("Let unimplemented");
		}
	}

	fn add_params(&mut self, param_defs: &'ast [ast::ParamDef], param_values: &[ScopeItem<'ast, 'ses>]) {
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

	pub fn get(&self, name: &str) -> Option<ScopeItem<'ast, 'ses>> {
		self.names.find_equiv(&name).map(|x| x.clone())
	}
}

pub fn resolve_module<'ast, 'ses>(session: &'ses Session<'ast, 'ses>, ast: &'ast ast::Module) -> Module<'ast, 'ses> {
	let mut scope = Scope::new();

	for import in ast.imports.iter() {
		fail!("Imports unimplemented");
	}

	scope.add_lets(ast.lets);

	for def in ast.defs.iter() {
		let ed = session.defArena.alloc(EventDef{ ast:def, parentScope: scope.clone()});
		scope.names.insert(def.name.to_owned(), Event(ed));
	}

	Module {
		ast: ast,
		scope: scope,
	}
}

trait StepHandler {

}

enum Step {
	BlockStep(~Step),
	SeqStep(~[Step]),
	PrimitiveStep(~StepHandler),
}

