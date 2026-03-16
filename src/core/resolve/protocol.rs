use indexmap::IndexMap;

use crate::core::resolve::parameters::{match_args, resolve_arg};
use crate::diagnostic::{ErrorReported, collect_or_err};
use crate::syntax::ast::{self, AstNode};
use crate::tree::TupleFields;
use crate::{Diagnostic, DiagnosticContext, Dir, Index};
use crate::core::resolve::{expr::constant, type_expr::type_tree};
use crate::core::shape::{MessageField, ShapeMode, ShapeMsgParam};
use crate::core::{ Scope, Shape, ShapeMsg };

pub fn resolve(
    dcx: &mut DiagnosticContext,
    index: &Index,
    caller_scope: &Scope,
    ast: &ast::ProtocolRef,
    tag_offset: usize,
) -> Result<Shape, ErrorReported> {
    let Some(protocol) = index.find_protocol(&ast.name.name) else {
        return Err(dcx.report(Diagnostic::NoProtocolNamed {
            span: caller_scope.span(ast.name.span),
            protocol_name: ast.name.name.clone(),
        }));
    };

    let protocol_ast = protocol.ast();
    let mut args = Vec::with_capacity(protocol_ast.params.list.len());
    let mut body_scope = protocol.file().scope();

    for (param, arg_ast) in match_args(dcx, caller_scope, &protocol_ast.params, &ast.args) {
        let arg = resolve_arg(dcx, caller_scope, &body_scope, param, arg_ast);
        body_scope.bind(&param.name.name, arg.clone());
        args.push(arg);
    }

    let mode = constant::<ShapeMode>(dcx, &body_scope, &protocol_ast.dir)?;

    let mut messages = vec![];
    let mut children = IndexMap::new();
    let mut next_tag = tag_offset;

    for entry in &protocol_ast.entries {
        match *entry {
            ast::ProtocolEntry::Message(ast::ProtocolMessageDef { ref name, ref params, ref child, .. }) => {
                if child.is_some() && !params.is_empty() {
                    dcx.report(Diagnostic::ProtocolMessageWithArgsAndChild {
                        span: body_scope.span(entry.span())
                    });
                }

                let params: TupleFields<ShapeMsgParam> = collect_or_err(params.iter().map(|p| {
                    let name = p.name.as_ref().map(|n| n.name.clone());
                    let direction = constant::<Dir>(dcx, &body_scope, &p.direction)?;

                    if !mode.allows_data(direction) {
                        Err(dcx.report(Diagnostic::ProtocolDataModeMismatch {
                            span: body_scope.span(p.span()),
                            mode,
                            direction,
                        }))?;
                    }

                    let ty = type_tree(dcx, &body_scope, &p.expr)?;
                    Ok((name, ShapeMsgParam { ty, direction }))
                }))?;

                let mut fields = vec![];
                for (_, param) in params.iter() {
                    param.ty.for_each(&mut |t| fields.push(MessageField { ty: t.clone(), direction: param.direction }));
                }

                let tag = next_tag;

                let child = if let Some(child) = child {
                    let shape = resolve(dcx, index, &body_scope, &child, tag + 1)?;

                    if !mode.allows_child(shape.mode) {
                        Err(dcx.report(Diagnostic::ProtocolChildModeMismatch {
                            span: body_scope.span(child.span),
                            child_name: child.name.name.clone(),
                            mode,
                            child_mode: shape.mode,
                        }))?
                    }

                    next_tag += shape.tag_count;
                    Some(shape)
                } else { None };

                debug!("Assigning tag {tag} to {name}", name = name.name);
                messages.push(ShapeMsg { name: name.name.clone(), params, fields, tag, child });
                next_tag += 1;
            }
            ast::ProtocolEntry::Child(ref node) => {
                let inner_shape = resolve(dcx, index, &body_scope, &node.child_protocol, next_tag)?;

                if !mode.allows_child(inner_shape.mode) {
                    Err(dcx.report(Diagnostic::ProtocolChildModeMismatch {
                        span: body_scope.span(node.span),
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
        args,
    })
}
