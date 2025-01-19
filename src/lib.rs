#[macro_use] extern crate log;

mod core;
pub mod syntax;
pub mod runtime;
mod entitymap;
mod tree;
pub mod diagnostic;

pub use crate::syntax::{ SourceFile, FileSpan };
pub use crate::core::{ Index, Scope, FileScope, Item, Value, Type, TypeTree, LeafItem, Dir, Shape, ShapeMsg };
pub use crate::runtime::{ Channel, ChannelMessage, Handle };
pub use crate::diagnostic::{ DiagnosticContext, Diagnostic, Label };
