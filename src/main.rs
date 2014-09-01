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
use std::task;

mod session;
mod resolve;
mod eval;
mod ast;
mod exec;
mod dumpfile;
mod interner;
#[cfg(test)] mod test;

peg_file! grammar("signalspec.rustpeg")

fn main() {
	let sess = session::Session::new();

	let args = os::args();
	let source_utf8 = File::open(&Path::new(args[1].as_slice())).read_to_end().unwrap();
	let source = str::from_utf8(source_utf8.as_slice());
	let module = grammar::module(source.unwrap()).unwrap();

	let prelude = resolve::Scope::new();

	let mut ctx = resolve::Context::new(&sess);

	let modscope = resolve::resolve_module(&mut ctx, &prelude, &module);

	let main = match *modscope.get("main").unwrap() {
		resolve::scope::DefItem(ref s) => s,
		_ => fail!("Main is not an event"),
	};

	let w = resolve::scope::EmptyItem;
	let event = main.resolve_call(&mut ctx, &w, None);
	exec::print_step_tree(&event, 0);

	let (s1, mut s2) = exec::Connection::new();
	task::spawn(proc(){
		let event = event;
		let mut s1 = s1;
		let r = exec::exec(&event, &mut s1);
		if r {
			println!("Matched");
		} else {
			println!("Failed");
		}
	});

	//dumpfile::write_values(&mut File::create(&Path::new("out.sd")).unwrap(), &s2);
	dumpfile::read_values(&mut File::open(&Path::new("out.sd")).unwrap(), &mut s2);
}
