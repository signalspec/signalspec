use crate::diagnostic::{Collector, ErrorReported, Span};
use crate::syntax::ast::{self, AstNode};
use crate::{Index, Dir, DiagnosticHandler, Diagnostic};
use super::resolve::resolve_dir;
use super::{ Scope, Shape, ShapeMsg, ShapeMsgParam };
use super::{ lexpr, rexpr, Item };

pub fn resolve(ctx: &dyn DiagnosticHandler, index: &Index, scope: &Scope, ast: &ast::ProtocolRef) -> Result<Shape, ErrorReported> {
    let args = rexpr(ctx, scope, &ast.param);
    match instantiate(ctx, index, &ast.name.name, args) {
        Ok(shape) => Ok(shape),
        Err(InstantiateProtocolError::ProtocolNotFound) => {
            Err(ctx.report(Diagnostic::NoProtocolNamed {
                span: Span::new(&scope.file, ast.name.span),
                protocol_name: ast.name.name.clone(),
            }))
        },
        Err(InstantiateProtocolError::ArgsMismatch { msg, found, protocol_def_span }) => {
            Err(ctx.report(Diagnostic::ProtocolArgumentMismatch {
                span: Span::new(&scope.file, ast.param.span()),
                protocol_name: ast.name.name.clone(),
                msg: msg.to_string(),
                found: format!("{found}"),
                def: protocol_def_span,
            }))
        }
    }
}

#[derive(Debug)]
pub enum InstantiateProtocolError {
    ProtocolNotFound,
    ArgsMismatch {
        msg: &'static str,
        found: Item,
        protocol_def_span: Span,
    },
}

/// Instantiates a protocol with passed arguments, creating a Shape.
pub fn instantiate(ctx: &dyn DiagnosticHandler, index: &Index, protocol_name: &str, args: Item) -> Result<Shape, InstantiateProtocolError> {
    let protocol = index.find_protocol(protocol_name).ok_or(InstantiateProtocolError::ProtocolNotFound)?;
    let protocol_ast = protocol.ast();
    let mut protocol_def_scope = protocol.scope().child();
    if let Err(msg) = lexpr(ctx, &mut protocol_def_scope, &protocol_ast.param, &args) {
        return Err(InstantiateProtocolError::ArgsMismatch {
            msg,
            found: args,
            protocol_def_span: Span::new(&protocol_def_scope.file, protocol_ast.param.span())
        });
    }

    let dir = super::resolve::resolve_dir(ctx, &protocol_def_scope, &protocol_ast.dir).unwrap();

    let mut messages = vec![];

    for entry in &protocol_ast.entries {
        match *entry {
            ast::ProtocolEntry::Message(ast::ProtocolMessageDef { ref name, ref params, .. }) => {
                let params = params.iter().map(|p| {
                    match &p {
                        ast::DefParam::Const(node) => {
                            let ty = rexpr(ctx, &protocol_def_scope, &node.expr).as_type_tree().expect("todo: not a type");
                            ShapeMsgParam { ty, direction: Dir::Dn } // TODO: allow const here at all?
                        }
                        ast::DefParam::Var(node) => {
                            let ty = rexpr(ctx, &protocol_def_scope, &node.expr).as_type_tree().expect("todo: not a type");
                            let direction = resolve_dir(ctx, &protocol_def_scope, &node.direction).unwrap();
                            ShapeMsgParam { ty, direction }
                        }
                    }
                }).collect();
                messages.push(ShapeMsg::new(name.name.clone(), params));
            }
        }
    }

    Ok(Shape { def: protocol.clone(), dir, messages, param: args })
}

/// Match a Shape against the `with` part of a with-def block, binding variables
/// in scope. Returns false if the shape's arguments fail to match, but the
/// scope may have already been modified.
pub(crate) fn match_protocol(scope: &mut Scope, protocol: &ast::ProtocolRef, shape: &Shape) -> bool {
    // TODO: resolve protocol.name in its file and make sure they refer to the same protocol
    if protocol.name.name != shape.def.ast().name.name {
        return false;
    }

    let mut ctx = Collector::new();

    if let Err(e) = lexpr(&mut ctx, scope, &protocol.param, &shape.param) {
        debug!("Failed to match protocol argument for `{}`: {:?}", protocol.name.name, e);
        return false;
    }

    true
}

/// Match an argument item against the parameters of a def, binding variables
/// in scope. Returns false if an argument fails to match, but the scope
/// may have already been modified.
pub(crate) fn match_def_params(scope: &mut Scope, params: &[ast::DefParam], args: &[Item]) -> bool {
    if params.len() != args.len() {
        debug!("Wrong number of arguments");
        return false;
    }

    for (param, arg) in params.iter().zip(args.iter()) {
        let param_expr = match param {
            ast::DefParam::Const(node) => &node.expr,
            ast::DefParam::Var(node) => &node.expr,
        };

        let mut ctx = Collector::new();

        if let Err(err) = lexpr(&mut ctx, scope, param_expr, &arg) {
            debug!("Failed to match argument: {:?}", err);
            return false;
        }
    }

    true
}
