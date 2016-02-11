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

use std::fs;
use std::io::prelude::*;
use std::path::PathBuf;

mod session;
mod process;
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

pub fn run(source_fname: &String, code: &[String], debug_dir: Option<PathBuf>) -> bool {
    let sess = session::Session::new(debug_dir);
    let loader = resolve::module_loader::ModuleLoader::new(&sess);

    let mut source = String::new();
    fs::File::open(source_fname).unwrap().read_to_string(&mut source).unwrap();
    let modscope = loader.parse_module(&source).unwrap();

    let mut processes = vec![];
    let mut shape = data::Shape::null();

    for arg in code {
        let process_ast = grammar::process(&arg)
            .unwrap_or_else(|e| panic!("Error parsing argument: {}", e));
        let process = process::resolve_process(&sess, &modscope, &shape, &process_ast);
        shape = process.shape_up().clone();
        processes.push(process);
    }

    let topmost_mode = shape.data_mode();

    if topmost_mode.up {
        processes.push(box dumpfile::ValueDumpPrint(shape));
    }

    process::run_process_chain(processes)
}

pub use test_runner::run as run_test;
