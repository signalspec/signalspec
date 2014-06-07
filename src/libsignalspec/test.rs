#![allow(dead_code)]

extern crate arena;
extern crate collections;
extern crate debug;

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
mod signal;
mod exec;
mod vcd;

fn main() {
	let sess = session::Session::new();

	let args = os::args();
	let source_utf8 = File::open(&Path::new(args.get(1).as_slice())).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8.as_slice());
	let module = grammar::module(source.unwrap());

	let module = module.unwrap();

	let mut prelude = resolve::Scope::new();

	let mut ctx = context::Context::new(&sess);

	let modscope = resolve::resolve_module(&mut ctx, &prelude, &module);

	let main = match modscope.get("main").unwrap() {
		expr::DefItem(s) => s,
		_ => fail!("Main is not an event"),
	};

	let w = signal::Signal::new();
	let event = main.resolve_call(&mut ctx, &resolve::Params{ positional: vec!(resolve::SignalItem(&w)), body: None});

	exec::print_step_tree(&event, 0);

	let mut dest = File::create(&Path::new(args.get(2).as_slice()));
	let mut vcdwriter = vcd::VCDWriter::new(&mut dest);
	vcdwriter.init(&["w"]);
	exec::exec_to_vcd(&event, &mut vcdwriter);
}
