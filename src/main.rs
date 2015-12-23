#![allow(dead_code)]
#![feature(plugin)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(str_char)]
#![feature(iter_arith)]
#![plugin(peg_syntax_ext)]

extern crate typed_arena;
extern crate bit_set;
extern crate vec_map;
extern crate ref_slice;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate env_logger;

use std::{env, process, fs};
use std::io::prelude::*;

use session::Process;

#[macro_use] mod session;
mod data;
mod resolve;
mod eval;
mod ast;
mod exec;
mod dumpfile;
mod vcd;
mod connection_io;
mod test_runner;

peg_file! grammar("signalspec.rustpeg");

fn main() {
    env_logger::init().unwrap();

    let args: Vec<String> = env::args().collect();

    if args[1] == "--test" {
        return test_runner::run(&args[2]);
    }

    let sess = session::Session::new();

    let mut source = String::new();
    fs::File::open(&args[1]).unwrap().read_to_string(&mut source).unwrap();
    let modscope = sess.parse_module(&source).unwrap();

    let mut processes = vec![];
    let mut shape = data::Shape::null();

    for arg in &args[2..] {
        let process_ast = grammar::process(&arg)
            .unwrap_or_else(|e| panic!("Error parsing argument: {}", e));
        let process = session::resolve_process(&sess, &modscope, &shape, &process_ast);
        shape = process.shape_up().clone();
        processes.push(process);
    }

    let topmost_mode = shape.data_mode();

    if topmost_mode.up {
        processes.push(box dumpfile::ValueDumpPrint(shape));
    }

    let success = session::run_process_chain(processes);

    process::exit(if success { 0 } else { 1 });
}
