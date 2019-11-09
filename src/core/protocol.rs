use crate::syntax::ast;
use super::{ Scope, Shape, ShapeMsg, ShapeMsgParam, Ctxt };
use super::{ lexpr, rexpr, Item };

/// Instantiates a protocol with passed arguments, creating a Shape.
pub fn resolve_protocol_invoke(ctx: &Ctxt, scope: &Scope, ast: &ast::ProtocolRef) -> Shape {
    if let Some(protocol) = ctx.index.find_protocol(&ast.name[..]) {
        let param = rexpr(scope, &ast.param);

        let mut protocol_def_scope = protocol.scope().child();
        lexpr(&mut protocol_def_scope, &protocol.ast().param, param.clone())
            .unwrap_or_else(|e| panic!("failed to match parameters for protocol `{}`: {:?}", ast.name, e));

        let mut messages = vec![];

        for entry in &protocol.ast().entries {
            match *entry {
                ast::ProtocolEntry::Message(ref name, ref es) => {
                    let params = es.iter().map(|p| {
                        let item = match &p.node {
                            ast::DefParam::Const(e) | ast::DefParam::Var{ value: e, .. } => rexpr(&protocol_def_scope, e),
                        };

                        ShapeMsgParam { item }
                    }).collect();
                    messages.push(ShapeMsg::new(name.clone(), params));
                }
            }
        }

        Shape::Seq { def: protocol.clone(), messages: messages, param }
    } else {
        panic!("Protocol `{}` not found", ast.name);
    }
}

/// Match a Shape against the `with` part of a with-def block, binding variables
/// in scope. Returns false if the shape's arguments fail to match, but the
/// scope may have already been modified.
pub(crate) fn match_protocol(scope: &mut Scope, protocol: &ast::ProtocolRef, shape: &Shape) -> bool {
    debug!("Trying to match {:?} against {:?}", protocol, shape);
    match shape {
        Shape::None => panic!("looking for defs on Shape::None"),
        Shape::Seq { def: r_proto, param: ref r_param, ..} => {
            // TODO: resolve protocol.name in its file and make sure they refer to the same protocol
            if protocol.name != r_proto.ast().name {
                return false;
            }

            if let Err(e) = lexpr(scope, &protocol.param, r_param.clone()) {
                debug!("Failed to match protocol argument for `{}`: {:?}", protocol.name, e);
                return false;
            }
        }
    }

    return true;
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
            ast::DefParam::Const(e) => e,
            ast::DefParam::Var{value, ..} => value,
        };

        if let Err(err) = lexpr(scope, &param_expr, arg.clone()) {
            debug!("Failed to match argument: {:?}", err);
            return false;
        }
    }

    true
}
