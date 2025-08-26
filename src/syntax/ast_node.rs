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

impl dyn AstNode {
    pub fn downcast<T: 'static>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref::<T>()
    }

    pub fn walk_preorder(&self) -> impl Iterator<Item = &dyn AstNode> {
        let mut remaining = vec![self];
        let mut children = Vec::new();

        std::iter::from_fn(move || {
            let next = remaining.pop();
            next.map(|node| {
                children.clear();
                node.children(&mut children);
                remaining.extend(children.iter().rev());
                node
            })
        })
    }

    pub fn walk_preorder_with_parent(&self) -> impl Iterator<Item = (&dyn AstNode, &dyn AstNode)> {
        let mut children = Vec::new();
        self.children(&mut children);
        let mut remaining: Vec<(_, _)> = children.iter().rev().map(|&c| (self, c)).collect();

        std::iter::from_fn(move || {
            let next = remaining.pop();
            next.map(|(parent, node)| {
                children.clear();
                node.children(&mut children);
                remaining.extend(children.iter().rev().map(|&c| (node, c)));
                (parent, node)
            })
        })
    }
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
