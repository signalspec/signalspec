#![allow(dead_code)]
#![feature(phase, if_let, slicing_syntax, macro_rules)]

extern crate arena;
extern crate collections;

#[phase(plugin, link)] extern crate log;
#[phase(plugin)] extern crate peg_syntax_ext;

use std::os;
use std::str;
use std::io;
use std::io::fs::File;
use std::task;
use std::default::Default;

#[macro_escape]
mod session;
mod resolve;
mod eval;
mod ast;
mod exec;
mod dumpfile;
mod interner;
mod data_usage;
#[cfg(test)] mod test;

peg_file! grammar("signalspec.rustpeg")

fn main() {
    let sess = session::Session::new();

    let args = os::args();
    let source_utf8 = File::open(&Path::new(args[1][])).read_to_end().unwrap();
    let source = str::from_utf8(source_utf8.as_slice());
    let module = grammar::module(source.unwrap()).unwrap();

    let mut ctx = resolve::Context::new(&sess);

    let modscope = resolve::resolve_module(&sess, &module);

    let main = match modscope.get("main").unwrap() {
        resolve::scope::DefItem(s) => s,
        _ => panic!("Main is not an event"),
    };

    let mut signal_info = resolve::context::SignalInfo {
        downwards: resolve::types::ShapeUnknown(false, true),
        upwards: resolve::types::ShapeVal(resolve::types::Bottom, false, true),
    };

    let mut event = main.resolve_call(&mut ctx, &mut signal_info, Default::default(), None);
    data_usage::pass(&mut event, &mut signal_info);
    //exec::print_step_tree(&event, 0);

    let (s1, mut s2) = exec::Connection::new();
    let (mut t1, t2) = exec::Connection::new();

    task::spawn(proc() {
        let mut s1 = s1;
        let mut i = io::stdin();
        dumpfile::read_values(&mut i, &mut s1);
    });

    task::spawn(proc() {
        let mut t2 = t2;
        let mut o = io::stdout();
        dumpfile::write_values(&mut o, &mut t2);
    });

    let mut state = eval::State::new();
    let r = exec::exec(&mut state, &event, &mut s2, &mut t1);

    os::set_exit_status(if r { 0 } else { 1 });
}
