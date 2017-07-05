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
mod primitives;

pub use session::Session;
pub use language::{ Ctxt, Module, Test, Item };
pub use process::{ Process, ProcessStack, ProcessInfo };
pub use connection::Connection;
pub use data::{ Value, Type, DataMode };
pub use protocol::{ Shape, Fields };
pub use test_runner::run as run_test;

pub use primitives::add_primitives;
