use indexmap::IndexMap;

use crate::diagnostic::ErrorReported;
use crate::syntax::ast::{self, AstNode};
use crate::{Index, Dir, DiagnosticContext, Diagnostic};
use crate::core::resolve::expr::{ constant, type_tree };
use crate::core::shape::ShapeMode;
use crate::core::{ Scope, Shape, ShapeMsg, ShapeMsgParam, Item, lexpr, rexpr, };

pub fn resolve(
    dcx: &mut DiagnosticContext,
    index: &Index,
    scope: &Scope,
    ast: &ast::ProtocolRef,
    tag_offset: usize,
) -> Result<Shape, ErrorReported> {
    let args = rexpr(dcx, scope, &ast.param);
    match instantiate(dcx, index, &ast.name.name, args, tag_offset) {
        Ok(shape) => Ok(shape),
        Err(InstantiateProtocolError::ProtocolNotFound) => {
            Err(dcx.report(Diagnostic::NoProtocolNamed {
                span: scope.span(ast.name.span),
                protocol_name: ast.name.name.clone(),
            }))
        },
        Err(InstantiateProtocolError::ErrorInProtocolBody(e)) => Err(e),
    }
}

#[derive(Debug)]
pub enum InstantiateProtocolError {
    ProtocolNotFound,
    ErrorInProtocolBody(ErrorReported),
}

impl From<ErrorReported> for InstantiateProtocolError {
    fn from(e: ErrorReported) -> Self {
        InstantiateProtocolError::ErrorInProtocolBody(e)
    }
}

/// Instantiates a protocol with passed arguments, creating a Shape.
pub fn instantiate(
    dcx: &mut DiagnosticContext,
    index: &Index,
    protocol_name: &str,
    args: Item,
    tag_offset: usize,
) -> Result<Shape, InstantiateProtocolError> {
    let protocol = index.find_protocol(protocol_name)
        .ok_or(InstantiateProtocolError::ProtocolNotFound)?;
    let protocol_ast = protocol.ast();
    let mut scope = protocol.file().scope();
    lexpr(dcx, &mut scope, &protocol_ast.param, &args);

    let mode = constant::<ShapeMode>(dcx, &scope, &protocol_ast.dir)?;

    let mut messages = vec![];
    let mut children = IndexMap::new();
    let mut next_tag = tag_offset;

    for entry in &protocol_ast.entries {
        match *entry {
            ast::ProtocolEntry::Message(ast::ProtocolMessageDef { ref name, ref params, ref child, .. }) => {
                if child.is_some() && !params.is_empty() {
                    dcx.report(Diagnostic::ProtocolMessageWithArgsAndChild {
                        span: scope.span(entry.span())
                    });
                }

                let params = params.iter().map(|p| {
                    let name = p.name.as_ref().map(|n| n.name.clone());
                    let direction = constant::<Dir>(dcx, &scope, &p.direction)?;

                    if !mode.allows_data(direction) {
                        Err(dcx.report(Diagnostic::ProtocolDataModeMismatch {
                            span: scope.span(p.span()),
                            mode,
                            direction,
                        }))?;
                    }

                    let ty = type_tree(dcx, &scope, &p.expr)?;
                    Ok((name, ShapeMsgParam { ty, direction }))
                }).collect::<Result<_, InstantiateProtocolError>>()?;

                let tag = next_tag;

                let child = if let Some(child) = child {
                    let shape = resolve(dcx, index, &scope, &child, tag + 1)?;

                    if !mode.allows_child(shape.mode) {
                        Err(dcx.report(Diagnostic::ProtocolChildModeMismatch {
                            span: scope.span(child.span),
                            child_name: child.name.name.clone(),
                            mode,
                            child_mode: shape.mode,
                        }))?
                    }

                    next_tag += shape.tag_count;
                    Some(shape)
                } else { None };

                debug!("Assigning tag {tag} to {name}", name = name.name);
                messages.push(ShapeMsg { name: name.name.clone(), params, tag, child });
                next_tag += 1;
            }
            ast::ProtocolEntry::Child(ref node) => {
                let inner_shape = resolve(dcx, index, &scope, &node.child_protocol, next_tag)?;

                if !mode.allows_child(inner_shape.mode) {
                    Err(dcx.report(Diagnostic::ProtocolChildModeMismatch {
                        span: scope.span(node.span),
                        child_name: node.name.name.clone(),
                        mode,
                        child_mode: inner_shape.mode,
                    }))?
                }

                debug!("Assigning tag range {next_tag}..={t} to {name}", t = next_tag + inner_shape.tag_count - 1, name = node.name.name);

                next_tag += inner_shape.tag_count;
                children.insert(node.name.name.clone(), inner_shape);
            }
        }
    }

    Ok(Shape {
        def: protocol.clone(),
        mode,
        tag_offset,
        tag_count: next_tag - tag_offset,
        messages,
        children,
        param: args
    })
}

