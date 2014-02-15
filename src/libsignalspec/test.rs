#[allow(dead_code)];

extern mod extra;
extern mod arena;

use std::os;
use std::str;
use std::io::{stdout,stderr};
use std::io::fs::File;

mod session;
mod context;
mod expr;
mod eval;
mod grammar;
mod bitv;
mod ast;
mod resolve;
mod virtual_clock;

fn main() {
	let mut sess = session::Session::new();

	let args = os::args();
	let source_utf8 = File::open(&Path::new(args[1])).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8);
	let module = grammar::module(source.unwrap());

	println!("ast: {:?}\n", module);

	let module = module.unwrap();

	let mut prelude = resolve::Scope::new();
	let timer = virtual_clock::timer();
	prelude.names.insert(~"time", resolve::EventItem(timer));

	let mut ctx = context::Context::new(&sess);

	let mut modscope = resolve::resolve_module(&mut ctx, &prelude, &module);
	println!("modscope: {:?}\n", modscope);

	let main = match modscope.get("main").unwrap() {
		resolve::EventItem(s) => s,
		_ => fail!("Main is not an event"),
	};

	let w = virtual_clock::wire_config();
	let event = main.resolve_call(&mut ctx, &[resolve::EntityItem(&w)], None);
	println!("main: {:?}\n", event);

	resolve::print_step_tree(&event, 0);
}
