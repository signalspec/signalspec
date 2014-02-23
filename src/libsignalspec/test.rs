#[allow(dead_code)];

extern crate extra;
extern crate arena;

use std::os;
use std::str;
use std::io::fs::File;

mod session;
mod context;
mod expr;
mod eval;
mod grammar;
mod bitv;
mod ast;
mod resolve;
mod entity;
mod virtual_clock;
mod exec;

fn main() {
	let sess = session::Session::new();

	let args = os::args();
	let source_utf8 = File::open(&Path::new(args[1])).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8);
	let module = grammar::module(source.unwrap());

	println!("ast: {:?}\n", module);

	let module = module.unwrap();

	let mut prelude = resolve::Scope::new();
	prelude.names.insert(~"time", expr::EntityItem(&resolve::time_call_fn as &entity::Entity));

	let mut ctx = context::Context::new(&sess);
	ctx.domain = sess.arena.alloc(|| virtual_clock::VirtualClockDomain::new()) as &context::Domain;

	let modscope = resolve::resolve_module(&mut ctx, &prelude, &module);
	println!("modscope: {:?}\n", modscope);

	let main = match modscope.get("main").unwrap() {
		expr::EntityItem(s) => s,
		_ => fail!("Main is not an event"),
	};

	let w = virtual_clock::Wire::new();
	let event = main.resolve_call(&mut ctx, &resolve::Params{ positional: ~[resolve::EntityItem(&w)], body: None});
	println!("main: {:?}\n", event);

	exec::print_step_tree(&event, 0);
}
