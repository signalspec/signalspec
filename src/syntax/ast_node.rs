use std::{any::Any, io::Write};
use crate::SourceFile;

use super::{FilePos, FileSpan};

/// Trait implemented by AST nodes
///
/// This provides an untyped syntax tree representation overtop of the typed AST.
pub trait AstNode: Any {
    /// Get the entire span of this node
    fn span(&self) -> FileSpan;

    /// Get the child nodes of this node.
    /// 
    /// The nodes are appended to the passed array to avoid the need to allocate a new one.
    /// Child nodes must be added in the order they appear in the source.
    fn children<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>);

    /// A short description of the node
    fn node_name(&self) -> &'static str;
}

/// Get the AstNodes that enclose the specified position, innermost last
pub fn enclosing(mut node: &dyn AstNode, pos: FilePos) -> Vec<&dyn AstNode> {
    let mut parents = Vec::new();
    let mut children: Vec<&dyn AstNode> = Vec::new();

    loop {
        assert!(node.span().contains(pos));
        parents.push(node);
        children.clear();
        node.children(&mut children);

        if let Some(&child) = children.iter().find(|c| { c.span().contains(pos) }) {
            node = child;
        } else {
            break
        }
    }

    parents
}

pub fn dump_tree(out: &mut dyn Write, file: &SourceFile, node: &dyn AstNode, indent: usize) -> Result<(), std::io::Error> {
    let span = node.span();
    let mut lines = file.slice(span).split("\n");
    let first_line = lines.next().unwrap();
    let continues = if lines.next().is_some() { " ..." } else { "" };

    write!(out, "{i}{name} @ line {line} : \"{first_line}\"{continues}\n",
        i = " ".repeat(indent as usize),
        line = file.byte_to_line(span.start) + 1,
        name = node.node_name(),
    )?;
    let mut children = Vec::new();
    node.children(&mut children);
    for child in &children {
        dump_tree(out, file, *child, indent + 2)?;
    }
    Ok(())
}
