#![feature(plugin)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(iter_arith)]
#![plugin(peg_syntax_ext)]

extern crate typed_arena;
extern crate bit_set;
extern crate vec_map;
extern crate ref_slice;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;

mod session;
mod process;
mod data;
mod language;
mod connection;
mod dumpfile;
mod vcd;
mod connection_io;
mod test_runner;

pub use session::Session;
pub use language::{ ModuleLoader, Module, Test };
pub use process::{ Process, ProcessStack };
pub use test_runner::run as run_test;

pub fn add_primitives(loader: &ModuleLoader) {
    loader.add_primitive_def("file", &connection_io::FILE_DEF);
    loader.add_primitive_def("vcd", &vcd::VCD_DEF);
    loader.add_primitive_def("dumpfile", &dumpfile::DUMPFILE_DEF);
}
