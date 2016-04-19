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
mod test_runner;

pub use session::Session;
pub use language::{ ModuleLoader, Module, Test, Item };
pub use process::{ Process, ProcessStack, PrimitiveDef };
pub use connection::Connection;
pub use data::{ Value, Type, DataMode, Shape, ShapeVariant, ShapeData };
pub use test_runner::run as run_test;

mod file_io;
mod dumpfile;

pub fn add_primitives(loader: &ModuleLoader) {
    loader.add_primitive_def("file", &file_io::FILE_DEF);
    loader.add_primitive_def("dumpfile", &dumpfile::DUMPFILE_DEF);
}
