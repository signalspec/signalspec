pub mod ast;
mod scope;
pub mod expr;
mod eval;
mod step;
mod nfa;
mod dfa;
pub mod program;
pub mod module_loader;

peg_file! grammar("signalspec.rustpeg");

pub use self::eval::Expr;
pub use self::module_loader::{ ModuleLoader, Module };
pub use self::scope::Item;
pub use self::expr::rexpr;
pub use self::program::resolve_process;

pub use self::grammar::literal as parse_literal;
pub use self::grammar::process as parse_process;
