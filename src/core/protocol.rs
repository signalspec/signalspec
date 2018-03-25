use syntax::ast;
use super::{ Item, Scope, Shape, ShapeVariant, Ctxt, PrimitiveDef };
use super::{ lexpr, rexpr };

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ProtocolId(pub usize);

impl From<usize> for ProtocolId {
    fn from(i: usize) -> ProtocolId { ProtocolId(i) }
}

impl From<ProtocolId> for usize {
    fn from(i: ProtocolId) -> usize { i.0 }
}

/// A pattern to match a protocol. These are used in the predicate of `with` blocks.
#[derive(Debug, Clone)]
struct ProtocolMatch {
    id: ProtocolId,
    param: ast::Expr
}

impl ProtocolMatch {
    /// Attempt to match the protocol pattern against the supplied shape, returning whether it
    /// succeeded. Destructured variables are added to the scope. Even if it returns false,
    /// the scope may have already been modified.
    fn try_match(&self, ctx: &Ctxt, shape: &Shape, scope: &mut Scope) -> bool {
        debug!("Trying to match {:?} against {:?}", self, shape);
        match *shape {
            Shape::None => false,
            Shape::Seq { def: r_id, param: ref r_param, ..} => {
                self.id == r_id && lexpr(ctx, scope, &self.param, r_param.clone()).is_ok()
            }
        }
    }
}

pub fn resolve_protocol_invoke(ctx: &Ctxt, scope: &Scope, ast: &ast::ProtocolRef) -> Shape {
    if let Some(Item::Protocol(protocol_id)) = scope.get(&ast.name[..]) {
        let protocol = ctx.protocols.get(protocol_id);
        let param = rexpr(ctx, scope, &ast.param);

        let mut protocol_def_scope = protocol.scope.child();
        lexpr(ctx, &mut protocol_def_scope, &protocol.ast.param, param.clone())
            .unwrap_or_else(|e| panic!("failed to match parameters for protocol `{}`: {:?}", ast.name, e));

        let mut messages = vec![];

        for entry in &protocol.ast.entries {
            match *entry {
                ast::ProtocolEntry::Message(ref name, ref e) => {
                    messages.push(ShapeVariant::new(name.clone(), rexpr(ctx, &protocol_def_scope, e)));
                }
            }
        }

        Shape::Seq { def: protocol_id, messages: messages, param }
    } else {
        panic!("Protocol `{}` not found", ast.name);
    }
}

fn resolve_protocol_match(_ctx: &Ctxt, scope: &Scope, ast: ast::ProtocolRef) -> ProtocolMatch {
    if let Some(Item::Protocol(protocol_id)) = scope.get(&ast.name[..]) {
        ProtocolMatch { id: protocol_id, param: ast.param.node }
    } else {
        panic!("Protocol `{}` not found", ast.name);
    }
}

struct WithBlock {
    protocol: ProtocolMatch,
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

    pub fn add_def(&mut self, ctx: &Ctxt, scope: Scope, def: ast::Def) {
        self.entries.push(WithBlock {
            protocol: resolve_protocol_match(ctx, &scope, def.bottom),
            name: def.name.clone(),
            scope: scope,
            param: def.param.node,
            implementation: DefImpl::Code(def.processes)
        });
    }

    pub fn add_primitive(&mut self, ctx: &Ctxt, scope: &Scope, header: ast::PrimitiveHeader, defs: Vec<PrimitiveDef>) {
        self.entries.push(WithBlock {
            protocol: resolve_protocol_match(ctx, scope, header.bottom),
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

            if !entry.protocol.try_match(ctx, shape, &mut scope) {
                debug!("Failed to match protocol for `{}`", name);
                continue
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
