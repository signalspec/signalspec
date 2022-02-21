#[macro_use] extern crate log;

mod core;
mod syntax;
mod runtime;
mod entitymap;
mod tree;
mod diagnostic;

pub use crate::syntax::{ SourceFile, FileSpan, Value };
pub use crate::core::{ Index, Scope, Item, LeafItem, DataMode, PrimitiveDef, Shape, ShapeMsg };
pub use crate::runtime::{ Channel, ChannelMessage, PrimitiveProcess, parse_compile_run, run_tests_in_file, add_primitives };
pub use crate::diagnostic::{ DiagnosticHandler, DiagnosticKind, Label };
