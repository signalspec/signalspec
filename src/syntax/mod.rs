pub mod ast;
mod ast_node;
mod binop;
mod value;
mod grammar;
mod span;
mod test;

pub use ast_node::{ AstNode, enclosing, dump_tree };
pub use self::binop::BinOp;
pub use self::value::Value;
pub use self::span::{ SourceFile, FilePos, FileSpan, Spanned };

pub type ParseError = peg::error::ParseError<peg::str::LineCol>;

pub use self::grammar::signalspec::{
    literal as parse_literal,
    expr as parse_expr,
    primitive_header as parse_primitive_header,
    process as parse_process,
    module as parse_module,
};
