#[macro_use] extern crate log;

mod core;
mod syntax;
mod runtime;
mod entitymap;

pub use crate::syntax::{ SourceFile, FileSpan, Value };
pub use crate::core::{ Index, Scope, Item, DataMode, PrimitiveDef, Shape, ShapeMsg };
pub use crate::runtime::{ Channel, ChannelMessage, PrimitiveProcess, parse_compile_run, run_tests_in_file, add_primitives };
