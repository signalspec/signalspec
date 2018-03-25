#![feature(plugin)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(match_default_bindings)]
#![feature(nll)]
#![plugin(peg_syntax_ext)]

extern crate bit_set;
extern crate ref_slice;
extern crate num_complex;
#[macro_use] extern crate log;
extern crate byteorder;
extern crate vec_map;
extern crate scoped_pool;
extern crate codemap;

mod util;
mod core;
pub mod syntax;
mod runtime;

pub use syntax::Value;
pub use core::{ Ctxt, Config, Scope, Item, DataMode, Fields, Field, PrimitiveDef, PrimitiveDefFields, Type, Shape, ShapeVariant };
pub use runtime::{ Handle, Connection, PrimitiveProcess, run_tests_in_file, add_primitives };
