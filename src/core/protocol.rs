use crate::syntax::ast;
use super::{ Scope, Shape, ShapeVariant, Ctxt };
use super::{ lexpr, rexpr };

pub fn resolve_protocol_invoke(ctx: &Ctxt, scope: &Scope, ast: &ast::ProtocolRef) -> Shape {
    if let Some(protocol) = ctx.index.find_protocol(&ast.name[..]) {
        let param = rexpr(scope, &ast.param);

        let mut protocol_def_scope = protocol.scope().child();
        lexpr(&mut protocol_def_scope, &protocol.ast().param, param.clone())
            .unwrap_or_else(|e| panic!("failed to match parameters for protocol `{}`: {:?}", ast.name, e));

        let mut messages = vec![];

        for entry in &protocol.ast().entries {
            match *entry {
                ast::ProtocolEntry::Message(ref name, ref e) => {
                    messages.push(ShapeVariant::new(name.clone(), rexpr(&protocol_def_scope, e)));
                }
            }
        }

        Shape::Seq { def: protocol.clone(), messages: messages, param }
    } else {
        panic!("Protocol `{}` not found", ast.name);
    }
}
