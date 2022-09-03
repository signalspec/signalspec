use crate::core::value;
use crate::syntax::ast;
use crate::{Value, Index};
use super::expr_resolve::PatternError;
use super::{ Scope, Shape, ShapeMsg, ShapeMsgParam, DataMode };
use super::{ lexpr, rexpr, Item };

pub fn resolve(index: &Index, scope: &Scope, ast: &ast::ProtocolRef) -> Shape {
    let args = rexpr(scope, &ast.param);
    instantiate(index, &ast.name.name, args).unwrap()
}

#[derive(Debug)]
pub enum InstantiateProtocolError {
    ProtocolNotFound,
    ArgsMismatch(PatternError),
}

/// Instantiates a protocol with passed arguments, creating a Shape.
pub fn instantiate(index: &Index, protocol_name: &str, args: Item) -> Result<Shape, InstantiateProtocolError> {
    let protocol = index.find_protocol(protocol_name).ok_or(InstantiateProtocolError::ProtocolNotFound)?;
    let mut protocol_def_scope = protocol.scope().child();
    lexpr(&mut protocol_def_scope, &protocol.ast().param, args.clone()).map_err(InstantiateProtocolError::ArgsMismatch)?;

    let dir = super::resolve::resolve_dir(value(&protocol_def_scope, &protocol.ast().dir));

    let mut messages = vec![];

    for entry in &protocol.ast().entries {
        match *entry {
            ast::ProtocolEntry::Message(ast::ProtocolMessageDef { ref name, ref params, .. }) => {
                let params = params.iter().map(|p| {
                    match &p {
                        ast::DefParam::Const(node) => {
                            let item = rexpr(&protocol_def_scope, &node.expr);
                            ShapeMsgParam { item, direction: DataMode { up: false, down: false} }
                        }
                        ast::DefParam::Var(node) => {
                            let item = rexpr(&protocol_def_scope, &node.expr);
                            let direction = match super::expr_resolve::value(&protocol_def_scope, &node.direction).eval_const() {
                                Value::Symbol(s) if s == "up" => DataMode { up: true, down: false },
                                Value::Symbol(s) if s == "dn" => DataMode { up: false, down: true },
                                other => panic!("Invalid direction {:?}, expected `#up` or `#dn`", other)
                            };
                            ShapeMsgParam { item, direction }
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

    if let Err(e) = lexpr(scope, &protocol.param, shape.param.clone()) {
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

        if let Err(err) = lexpr(scope, param_expr, arg.clone()) {
            debug!("Failed to match argument: {:?}", err);
            return false;
        }
    }

    true
}
