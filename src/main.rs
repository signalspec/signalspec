#![allow(dead_code)]
#![feature(slicing_syntax, plugin, box_syntax, rustc_private, collections, core, io, std_misc, os, path)]

extern crate arena;
extern crate collections;

#[macro_use] extern crate log;
#[plugin] extern crate peg_syntax_ext;

use std::os;
use std::str;
use std::old_io as io;
use std::thread::Thread;
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

    let args = os::args();
    let source_utf8 = io::File::open(&Path::new(&args[1][])).read_to_end().unwrap();
    let source = str::from_utf8(source_utf8.as_slice());

    let mut ctx = resolve::Context::new(&sess);
    let modscope = sess.parse_module(source.unwrap()).unwrap();
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

    let reader_thread = Thread::scoped(move || {
        let mut s2 = s2;
        let mut i = io::stdin();
        dumpfile::read_values(&mut i, &mut s2);
    });

    let writer_thread = Thread::scoped(move || {
        let mut t1 = t1;
        let mut o = io::stdout();
        dumpfile::write_values(&mut o, &mut t1);
    });

    let mut state = eval::State::new();
    let r = exec::exec(&mut state, &event, &mut s1, &mut t2);

    drop(s1);
    drop(t2);
    reader_thread.join().ok().unwrap();
    writer_thread.join().ok().unwrap();

    os::set_exit_status(if r { 0 } else { 1 });
}
