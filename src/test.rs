#![allow(dead_code)]
#![feature(phase)]

extern crate arena;
extern crate collections;
extern crate debug;

#[phase(plugin)]
extern crate peg_syntax_ext;

use std::os;
use std::str;
use std::io::fs::File;
use std::comm;
use std::task;

mod session;
mod context;
mod expr;
mod eval;
mod ast;
mod resolve;
mod signal;
mod exec;
mod dumpfile;

peg_file! grammar("signalspec.rustpeg")

fn main() {
	let sess = session::Session::new();

	let args = os::args();
	let source_utf8 = File::open(&Path::new(args.get(1).as_slice())).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8.as_slice());
	let module = grammar::module(source.unwrap());

	let module = module.unwrap();

	let prelude = resolve::Scope::new();

	let mut ctx = context::Context::new(&sess);

	let modscope = resolve::resolve_module(&mut ctx, &prelude, &module);

	let main = match *modscope.get("main").unwrap() {
		expr::DefItem(ref s) => s,
		_ => fail!("Main is not an event"),
	};

	let w = resolve::SignalItem(signal::Signal::new(sess.make_id()));
	let event = main.resolve_call(&mut ctx, &resolve::Params{ positional: vec!(&w), body: None});

	exec::print_step_tree(&event, 0);

	let (s1, s2) = comm::duplex();
	task::spawn(proc(){
		let event = event;
		let s1 = s1;
		let r = exec::exec(&event, &s1);
		if r {
			println!("Matched");
		} else {
			println!("Failed");
		}
	});

	//dumpfile::write_values(&mut File::create(&Path::new("out.sd")).unwrap(), &s2);
	dumpfile::read_values(&mut File::open(&Path::new("out.sd")).unwrap(), &s2);
}
