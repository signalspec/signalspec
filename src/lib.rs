#[macro_use] extern crate log;

mod core;
pub mod syntax;
pub mod runtime;
mod entitymap;
mod tree;
pub mod diagnostic;

pub use crate::syntax::{ SourceFile, FileSpan, Value };
pub use crate::core::{ Index, Scope, FileScope, Item, Type, TypeTree, LeafItem, Dir, DataMode, PrimitiveDef, Shape, ShapeMsg, write_tree };
pub use crate::runtime::{ Channel, ChannelMessage, PrimitiveProcess, Handle, add_primitives };
pub use crate::diagnostic::{ DiagnosticHandler, Diagnostic, Label };
