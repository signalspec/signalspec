#![allow(dead_code)]
#![feature(plugin, box_syntax, box_patterns, rustc_private, collections, core, std_misc, old_io, io, fs, env)]
#![plugin(peg_syntax_ext)]

extern crate arena;
extern crate collections;

#[macro_use] extern crate log;

use std::{env, fs, thread};
use std::io::prelude::*;
use std::default::Default;

#[macro_use] mod session;
mod resolve;
mod eval;
mod ast;
mod exec;
mod dumpfile;
mod data_usage;
#[cfg(test)] mod test;

peg_file! grammar("signalspec.rustpeg");

fn main() {
    let sess = session::Session::new();

    let args: Vec<String> = env::args().collect();
    let mut source = String::new();
    fs::File::open(&args[1]).unwrap().read_to_string(&mut source).unwrap();

    let mut ctx = resolve::Context::new(&sess);
    let modscope = sess.parse_module(&source).unwrap();
    let main = modscope.get_def("main");

    let mut signal_info = resolve::context::SignalInfo {
        downwards: resolve::types::Shape::Unknown(false, true),
        upwards: resolve::types::Shape::Val(resolve::types::Bottom, false, true),
    };

    let mut event = main.resolve_call(&mut ctx, &mut signal_info, Default::default(), None);
    data_usage::pass(&mut event, &mut signal_info);
    //exec::print_step_tree(&event, 0);

    let (mut s1, s2) = exec::Connection::new(&signal_info.downwards);
    let (t1, mut t2) = exec::Connection::new(&signal_info.upwards);

    let reader_thread = thread::scoped(move || {
        let mut s2 = s2;
        let mut i = std::old_io::stdin();
        dumpfile::read_values(&mut i, &mut s2);
    });

    let writer_thread = thread::scoped(move || {
        let mut t1 = t1;
        let mut o = std::old_io::stdout();
        dumpfile::write_values(&mut o, &mut t1);
    });

    let mut state = eval::State::new();
    let r = exec::exec(&mut state, &event, &mut s1, &mut t2);

    drop(s1);
    drop(t2);
    reader_thread.join();
    writer_thread.join();

    env::set_exit_status(if r { 0 } else { 1 });
}
