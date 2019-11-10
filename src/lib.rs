#[macro_use] extern crate log;

mod core;
mod syntax;
mod runtime;

pub use crate::syntax::{ SourceFile, FileSpan, Value };
pub use crate::core::{ Index, Config, Scope, Item, DataMode, PrimitiveDef, Shape, ShapeMsg };
pub use crate::runtime::{ Handle, Connection, PrimitiveProcess, run_tests_in_file, add_primitives };
