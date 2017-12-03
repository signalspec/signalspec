#![feature(plugin)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(match_default_bindings)]
#![plugin(peg_syntax_ext)]

extern crate bit_set;
extern crate ref_slice;
extern crate num_complex;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate byteorder;
extern crate vec_map;

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
pub use language::{ Ctxt, Module, Test, Item, Scope, PrimitiveDef, PrimitiveDefFields };
pub use process::{ Process, ProcessStack, ProcessInfo };
pub use connection::Connection;
pub use data::{ Value, Type, DataMode };
pub use protocol::{ Shape, ShapeVariant, Fields, Field };
pub use test_runner::run as run_test;

pub use primitives::add_primitives;
