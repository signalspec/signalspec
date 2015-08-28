pub use self::scope::Scope;
pub use self::block::resolve_module;

pub mod scope;
pub mod expr;
pub mod block;
mod interface;

pub use self::interface::resolve_interface as interface;
