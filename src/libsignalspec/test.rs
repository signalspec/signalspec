#![allow(dead_code)]

extern crate arena;
extern crate collections;

use std::os;
use std::str;
use std::io::fs::File;

mod session;
mod context;
mod expr;
mod eval;
mod grammar;
mod ast;
mod resolve;
mod entity;
mod virtual_clock;
mod exec;
mod vcd;

fn main() {
	let sess = session::Session::new();

	let args = os::args();
	let source_utf8 = File::open(&Path::new(args[1].as_slice())).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8.as_slice());
	let module = grammar::module(source.unwrap());

	let module = module.unwrap();

	let mut prelude = resolve::Scope::new();
	prelude.names.insert(~"time", expr::EntityItem(&resolve::time_call_fn as &entity::Entity));

	let mut ctx = context::Context::new(&sess);

	let modscope = resolve::resolve_module(&mut ctx, &prelude, &module);

	let main = match modscope.get("main").unwrap() {
		expr::EntityItem(s) => s,
		_ => fail!("Main is not an event"),
	};

	let w = virtual_clock::Signal::new();
	let event = main.resolve_call(&mut ctx, &resolve::Params{ positional: ~[resolve::EntityItem(&w)], body: None});

	exec::print_step_tree(&event, 0);

	let mut dest = File::create(&Path::new(args[2].as_slice()));
	let mut vcdwriter = vcd::VCDWriter::new(&mut dest);
	vcdwriter.init(&[&"w"]);
	exec::exec_to_vcd(&event, &mut vcdwriter);
}
