pub mod ast;
mod ast_node;
mod binop;
mod grammar;
mod span;
mod test;

pub use ast_node::{ AstNode, enclosing, dump_tree };
pub use self::binop::BinOp;
pub use self::span::{ SourceFile, FilePos, FileSpan, Spanned };

pub type Number = num_rational::Rational64;

pub type ParseError = peg::error::ParseError<peg::str::LineCol>;

pub use self::grammar::signalspec::{
    literal as parse_literal,
    expr as parse_expr,
    process as parse_process,
    module as parse_module,
};
