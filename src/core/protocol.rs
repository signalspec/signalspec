use indexmap::IndexMap;

use crate::diagnostic::{ErrorReported, Span};
use crate::syntax::ast::{self, AstNode};
use crate::{Index, Dir, DiagnosticHandler, Diagnostic};
use super::resolve::expr::{ constant, type_tree };
use super::shape::ShapeMode;
use super::{ Scope, Shape, ShapeMsg, ShapeMsgParam };
use super::{ lexpr, rexpr, Item };

pub fn resolve(
    ctx: &dyn DiagnosticHandler,
    index: &Index,
    scope: &Scope,
    ast: &ast::ProtocolRef,
    tag_offset: usize,
) -> Result<Shape, ErrorReported> {
    let args = rexpr(ctx, scope, &ast.param);
    match instantiate(ctx, index, &ast.name.name, args, tag_offset) {
        Ok(shape) => Ok(shape),
        Err(InstantiateProtocolError::ProtocolNotFound) => {
            Err(ctx.report(Diagnostic::NoProtocolNamed {
                span: Span::new(&scope.file, ast.name.span),
                protocol_name: ast.name.name.clone(),
            }))
        },
        Err(InstantiateProtocolError::ArgsMismatch { found, protocol_def_span }) => {
            //TODO: should be chained to previous error
            Err(ctx.report(Diagnostic::ProtocolArgumentMismatch {
                span: Span::new(&scope.file, ast.param.span()),
                protocol_name: ast.name.name.clone(),
                found: format!("{found}"),
                def: protocol_def_span,
            }))
        }
        Err(InstantiateProtocolError::ErrorInProtocolBody(e)) => Err(e),
    }
}

#[derive(Debug)]
pub enum InstantiateProtocolError {
    ProtocolNotFound,
    ArgsMismatch {
        found: Item,
        protocol_def_span: Span,
    },
    ErrorInProtocolBody(ErrorReported),
}

impl From<ErrorReported> for InstantiateProtocolError {
    fn from(e: ErrorReported) -> Self {
        InstantiateProtocolError::ErrorInProtocolBody(e)
    }
}

/// Instantiates a protocol with passed arguments, creating a Shape.
pub fn instantiate(
    ctx: &dyn DiagnosticHandler,
    index: &Index,
    protocol_name: &str,
    args: Item,
    tag_offset: usize,
) -> Result<Shape, InstantiateProtocolError> {
    let protocol = index.find_protocol(protocol_name)
        .ok_or(InstantiateProtocolError::ProtocolNotFound)?;
    let protocol_ast = protocol.ast();
    let mut scope = protocol.scope().child();
    if let Err(_) = lexpr(ctx, &mut scope, &protocol_ast.param, &args) {
        return Err(InstantiateProtocolError::ArgsMismatch {
            found: args,
            protocol_def_span: Span::new(&scope.file, protocol_ast.param.span())
        });
    }

    let mode = constant::<ShapeMode>(ctx, &scope, &protocol_ast.dir)?;

    let mut messages = vec![];
    let mut children = IndexMap::new();
    let mut tag = tag_offset;

    for entry in &protocol_ast.entries {
        match *entry {
            ast::ProtocolEntry::Message(ast::ProtocolMessageDef { ref name, ref params, ref child, .. }) => {
                if child.is_some() && !params.is_empty() {
                    ctx.report(Diagnostic::ProtocolMessageWithArgsAndChild {
                        span: Span::new(&scope.file, entry.span())
                    });
                }

                let params = params.iter().map(|p| {
                    let (ty_expr, direction) = match &p {
                        ast::DefParam::Const(_) => {
                            panic!("Not allowed here")
                        }
                        ast::DefParam::Var(node) => {
                            let param_dir = constant::<Dir>(ctx, &scope, &node.direction)?;

                            if !mode.allows_data(param_dir) {
                                Err(ctx.report(Diagnostic::ProtocolDataModeMismatch {
                                    span: Span::new(&scope.file, p.span()),
                                    mode,
                                    param_dir
                                }))?;
                            }

                            (&node.expr, param_dir)
                        }
                    };
                    let ty = type_tree(ctx, &scope, ty_expr)?;
                    Ok(ShapeMsgParam { ty, direction })
                }).collect::<Result<_, InstantiateProtocolError>>()?;

                let child = if let Some(child) = child {
                    let shape = resolve(ctx, index, &scope, &child, tag)?;

                    if !mode.allows_child(shape.mode) {
                        Err(ctx.report(Diagnostic::ProtocolChildModeMismatch {
                            span: Span::new(&scope.file, child.span),
                            child_name: child.name.name.clone(),
                            mode,
                            child_mode: shape.mode,
                        }))?
                    }

                    tag += shape.tag_count;
                    Some(shape)
                } else { None };

                messages.push(ShapeMsg { name: name.name.clone(), params, tag, child });
                tag += 1;
            }
            ast::ProtocolEntry::Child(ref node) => {
                let inner_shape = resolve(ctx, index, &scope, &node.child_protocol, tag)?;

                if !mode.allows_child(inner_shape.mode) {
                    Err(ctx.report(Diagnostic::ProtocolChildModeMismatch {
                        span: Span::new(&scope.file, node.span),
                        child_name: node.name.name.clone(),
                        mode,
                        child_mode: inner_shape.mode,
                    }))?
                }

                tag += inner_shape.tag_count;
                children.insert(node.name.name.clone(), inner_shape);
            }
        }
    }

    Ok(Shape {
        def: protocol.clone(),
        mode,
        tag_offset,
        tag_count: tag - tag_offset,
        messages,
        children,
        param: args
    })
}

