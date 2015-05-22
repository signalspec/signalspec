#![allow(dead_code)]
#![feature(plugin, box_syntax, box_patterns, rustc_private, collections, exit_status, str_char, scoped, core)]
#![plugin(peg_syntax_ext)]

extern crate arena;
extern crate collections;

#[macro_use] extern crate log;

use std::{env, fs, thread};
use std::io::prelude::*;

use session::Process;

#[macro_use] mod session;
mod resolve;
mod eval;
mod ast;
mod exec;
mod dumpfile;
mod vcd;
mod connection_io;
#[cfg(test)] mod test;

peg_file! grammar("signalspec.rustpeg");

fn main() {
    let sess = session::Session::new();

    let args: Vec<String> = env::args().collect();
    let mut source = String::new();
    fs::File::open(&args[1]).unwrap().read_to_string(&mut source).unwrap();
    let modscope = sess.parse_module(&source).unwrap();

    let mut processes = vec![];
    let mut shape = resolve::types::NULL_SHAPE.clone();
    let scope = resolve::scope::Scope::new();

    for arg in &args[2..] {
        if arg.starts_with("{") {
            let block = grammar::block(&arg)
                .unwrap_or_else(|e| panic!("Error parsing block: {}", e));

            let mut shape_up = resolve::types::NULL_SHAPE.clone();
            let (step, _) = resolve::block::resolve_seq(&sess, &scope, &shape, &mut shape_up, &block);

            processes.push(box session::Program { step: step,
                                                  shape_down: shape.clone(),
                                                  shape_up: shape_up }
                                                  as Box<session::Process>);
        } else {
            let call = grammar::process(&arg)
                .unwrap_or_else(|e| panic!("Error parsing argument: {}", e));
            let arg = resolve::expr::rexpr(&sess, &scope, &call.arg);

            let main = match &call.name[..] {
                "file" => connection_io::file_process(arg),
                "dump" => dumpfile::process(&shape, arg),
                "vcd" => vcd::process(&shape, arg),
                name => (box modscope.compile_call(name, shape, arg)
                  .ok().expect("Failed to compile call") as Box<session::Process>)
            };
            shape = main.shape_up().clone();
            debug!("shape: {:?}", shape);
            processes.push(main);
        }
    }

    let topmost_mode = shape.data_mode();

    if topmost_mode.up {
        processes.push(box dumpfile::ValueDumpPrint(shape));
    }

    let (_, mut connection) = exec::Connection::new(eval::DataMode { down: false, up: false });
    let threads = processes.into_iter().map(|p| {
        let (mut c2, c1) = exec::Connection::new(p.shape_up().data_mode());
        ::std::mem::swap(&mut c2, &mut connection);
        thread::scoped(move || {
            let mut downward = c2;
            let mut upward = c1;
            let mut state = eval::State::new();
            p.run(&mut state, &mut downward, &mut upward)
        })
    }).collect::<Vec<_>>();

    let success = threads.into_iter().all(|x| x.join());

    env::set_exit_status(if success { 0 } else { 1 });
}
