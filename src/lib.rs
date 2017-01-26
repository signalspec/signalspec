#![feature(plugin)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![plugin(peg_syntax_ext)]

extern crate typed_arena;
extern crate bit_set;
extern crate vec_map;
extern crate ref_slice;
extern crate num_complex;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate byteorder;

mod session;
mod util;
mod process;
mod data;
mod language;
mod connection;
mod test_runner;
mod protocol;

pub use session::Session;
pub use language::{ ModuleLoader, Module, Test, Item };
pub use process::{ Process, ProcessStack, PrimitiveDef };
pub use connection::Connection;
pub use data::{ Value, Type, DataMode, Shape, ShapeVariant, ShapeData };
pub use test_runner::run as run_test;

mod file_io;
mod dumpfile;
mod binfile;

pub fn add_primitives(loader: &ModuleLoader) {
    loader.add_primitive_def("file", &file_io::FILE_DEF);
    loader.add_primitive_def("dumpfile", &dumpfile::DUMPFILE_DEF);
    loader.add_primitive_def("binfile", &binfile::BINFILE_DEF);
    language::add_primitive_fns(loader);
}
