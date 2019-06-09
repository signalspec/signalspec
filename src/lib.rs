#[macro_use] extern crate log;

mod util;
mod core;
pub mod syntax;
mod runtime;

pub use crate::syntax::Value;
pub use crate::core::{ Ctxt, Config, Scope, Item, DataMode, Fields, Field, PrimitiveDef, PrimitiveDefFields, Type, Shape, ShapeVariant };
pub use crate::runtime::{ Handle, Connection, PrimitiveProcess, run_tests_in_file, add_primitives };
