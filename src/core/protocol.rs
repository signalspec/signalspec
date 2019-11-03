use crate::syntax::ast;
use super::{ Item, Scope, Shape, ShapeVariant, Ctxt, PrimitiveDef };
use super::{ lexpr, rexpr };

pub fn resolve_protocol_invoke(ctx: &Ctxt, scope: &Scope, ast: &ast::ProtocolRef) -> Shape {
    if let Some(protocol) = ctx.protocols_by_name.borrow_mut().get(&ast.name[..]) {
        let param = rexpr(ctx, scope, &ast.param);

        let mut protocol_def_scope = protocol.scope().child();
        lexpr(ctx, &mut protocol_def_scope, &protocol.ast().param, param.clone())
            .unwrap_or_else(|e| panic!("failed to match parameters for protocol `{}`: {:?}", ast.name, e));

        let mut messages = vec![];

        for entry in &protocol.ast().entries {
            match *entry {
                ast::ProtocolEntry::Message(ref name, ref e) => {
                    messages.push(ShapeVariant::new(name.clone(), rexpr(ctx, &protocol_def_scope, e)));
                }
            }
        }

        Shape::Seq { def: protocol.clone(), messages: messages, param }
    } else {
        panic!("Protocol `{}` not found", ast.name);
    }
}

struct WithBlock {
    protocol: ast::ProtocolRef,
    name: String,
    param: ast::Expr,
    scope: Scope,
    implementation: DefImpl
}

pub enum DefImpl {
    Code(Vec<ast::Process>),
    Primitive(Vec<PrimitiveDef>, Option<ast::ProtocolRef>)
 }


pub struct ProtocolScope {
    entries: Vec<WithBlock>,
}

impl ProtocolScope {
    pub fn new() -> Self {
        ProtocolScope { entries: vec![] }
    }

    pub fn add_def(&mut self, scope: Scope, def: ast::Def) {
        self.entries.push(WithBlock {
            protocol: def.bottom,
            name: def.name.clone(),
            scope: scope,
            param: def.param.node,
            implementation: DefImpl::Code(def.processes)
        });
    }

    pub fn add_primitive(&mut self, scope: &Scope, header: ast::PrimitiveHeader, defs: Vec<PrimitiveDef>) {
        self.entries.push(WithBlock {
            protocol: header.bottom,
            name: header.name.clone(),
            scope: scope.child(),
            param: header.param.node,
            implementation: DefImpl::Primitive(defs, header.top),
        });
    }

    pub fn find(&self, ctx: &Ctxt, shape: &Shape, name: &str, param: Item) -> (Scope, &DefImpl) {
        let mut found = None;
        for entry in &self.entries {
            if entry.name != name { continue }

            let mut scope = entry.scope.child();

            let protocol = ctx.protocols_by_name.borrow_mut().get(&entry.protocol.name[..]).cloned().unwrap_or_else(|| {
                panic!("Failed to find protocol {:?}", entry.protocol.name);
            });
            
            debug!("Trying to match {:?} against {:?}", entry.protocol, shape);
            match shape {
                Shape::None => panic!("looking for methods of Shape::None"),
                Shape::Seq { def: r_proto, param: ref r_param, ..} => {
                    if !(&protocol == r_proto && lexpr(ctx, &mut scope, &entry.protocol.param, r_param.clone()).is_ok()) {
                        debug!("Failed to match protocol for `{}`", name);
                        continue;
                    }
                }
            }

            if let Err(err) = lexpr(ctx, &mut scope, &entry.param, param.clone()) {
                debug!("Failed to match argument for `{}`: {:?}", name, err);
                continue
            }

            if found.is_none() {
                found = Some((scope, &entry.implementation));
            } else {
                panic!("Multiple definition of `{}`", name);
            }
        }
        found.unwrap_or_else(|| panic!("No definition found for `{}`", name))
    }
}
