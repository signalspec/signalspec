use protocol::ProtocolId;
use data::{ Value, Type };
use protocol::Shape;
use super::scope::{Item, Scope};
use super::ast;
use super::expr;
use super::{ Ctxt, PrimitiveDef };
use super::eval::Expr;

/// A pattern to match a protocol. These are used in the predicate of `with` blocks.
#[derive(Debug, Clone)]
enum ProtocolMatch<'a> {
    Protocol { id: ProtocolId, param: &'a ast::Expr },
    Type(Type),
    Tup(Vec<ProtocolMatch<'a>>),
    Const(Value),
}

impl<'a> ProtocolMatch<'a> {
    /// Attempt to match the protocol pattern against the supplied shape, returning whether it
    /// succeeded. Destructured variables are added to the scope. Even if it returns false,
    /// the scope may have already been modified.
    fn try_match(&self, ctx: &'a Ctxt<'a>, shape: &Shape, scope: &mut Scope) -> bool {
        debug!("Trying to match {:?} against {:?}", self, shape);
        match (self, shape) {
            (&ProtocolMatch::Protocol { id: l_id, param: l_param }, &Shape::Protocol { def: r_id, param: ref r_param, ..}) => {
                l_id == r_id && expr::lexpr(ctx, scope, l_param, r_param.clone()).is_ok()
            }
            (&ProtocolMatch::Type(ref l_ty), &Shape::Val(ref r_ty)) => {
                l_ty == r_ty
            }
            (&ProtocolMatch::Tup(ref l_items), &Shape::Tup(ref r_items)) => {
                l_items.len() == r_items.len() && l_items.iter().zip(r_items.iter()).all(|(p, s)| p.try_match(ctx, s, scope))
            }
            (&ProtocolMatch::Const(ref l_val), &Shape::Const(ref r_val)) => {
                l_val == r_val
            }
            _ => false
        }
    }
}

fn item_to_shape(item: Item) -> Shape {
    match item {
        Item::Value(Expr::Const(c)) => Shape::Const(c),
        Item::Value(e) => Shape::Val(e.get_type()),
        Item::Tuple(t) => Shape::Tup(t.into_iter().map(item_to_shape).collect()),
        e => panic!("{:?} cannot be converted to Shape", e),
    }
}

pub fn resolve_protocol_invoke<'a>(ctx: &'a Ctxt<'a>, scope: &Scope, ast: &'a ast::ProtocolRef) -> Shape {
    match *ast {
        ast::ProtocolRef::Protocol{ ref name, param: ref param_ast } => {
            if let Some(Item::Protocol(protocol_id)) = scope.get(&name[..]) {
                let protocol = ctx.protocols.get(protocol_id);
                let param = expr::rexpr(ctx, scope, param_ast);

                let mut protocol_def_scope = protocol.scope.child();
                expr::lexpr(ctx, &mut protocol_def_scope, &protocol.ast.param, param.clone())
                    .unwrap_or_else(|e| panic!("failed to match parameters for protocol `{}`: {:?}", name, e));

                let mut messages = vec![];
                for entry in &protocol.ast.entries {
                    match *entry {
                        ast::ProtocolEntry::Message(ref e) => {
                            messages.push(resolve_protocol_invoke(ctx, &protocol_def_scope, e));
                        }
                    }
                }

                Shape::Protocol { def: protocol_id, messages: messages, param }
            } else {
                panic!("Protocol `{}` not found", name);
            }
        }
        ast::ProtocolRef::Type(ref expr) => {
            item_to_shape(expr::rexpr(ctx, scope, expr))
        }
        ast::ProtocolRef::Tup(ref items) => {
            let resolved_items = items.iter().map(|x| resolve_protocol_invoke(ctx, scope, x)).collect::<Vec<_>>();
            if resolved_items.len() == 1 {
                resolved_items.into_iter().next().unwrap()
            } else {
                Shape::Tup(resolved_items)
            }
        }
    }
}

fn resolve_protocol_match<'a>(ctx: &'a Ctxt<'a>, scope: &Scope, ast: &'a ast::ProtocolRef) -> ProtocolMatch<'a> {
    match *ast {
        ast::ProtocolRef::Protocol{ ref name, ref param } => {
            if let Some(Item::Protocol(protocol_id)) = scope.get(&name[..]) {
                ProtocolMatch::Protocol{ id: protocol_id, param: param }
            } else {
                panic!("Protocol `{}` not found", name);
            }
        }
        ast::ProtocolRef::Type(ref expr) => {
            ProtocolMatch::Type(expr::value(ctx, scope, expr).get_type())
        }
        ast::ProtocolRef::Tup(ref items) => {
            let resolved_items = items.iter().map(|x| resolve_protocol_match(ctx, scope, x)).collect::<Vec<_>>();
            if resolved_items.len() == 1 {
                resolved_items.into_iter().next().unwrap()
            } else {
                ProtocolMatch::Tup(resolved_items)
            }
        }
    }
}

struct WithBlock<'a> {
    protocol: ProtocolMatch<'a>,
    name: String,
    param: &'a ast::Expr,
    scope: Scope,
    shape_up: &'a Option<ast::ProtocolRef>,
    implementation: DefImpl<'a>
}

pub enum DefImpl <'a> {
    Code(&'a ast::Block),
    Primitive(Vec<PrimitiveDef>)
 }


pub struct ProtocolScope<'a> {
    entries: Vec<WithBlock<'a>>,
}

impl<'a> ProtocolScope<'a> {
    pub fn new() -> Self {
        ProtocolScope { entries: vec![] }
    }

    pub fn add_def(&mut self, ctx: &'a Ctxt<'a>, scope: Scope, def: &'a ast::Def) {
        self.entries.push(WithBlock {
            protocol: resolve_protocol_match(ctx, &scope, &def.bottom),
            name: def.name.clone(),
            scope: scope,
            param: &def.param,
            shape_up: &def.top,
            implementation: DefImpl::Code(&def.block)
        });
    }

    pub fn add_primitive(&mut self, ctx: &'a Ctxt<'a>, scope: &Scope, header: &'a ast::PrimitiveHeader, defs: Vec<PrimitiveDef>) {
        self.entries.push(WithBlock {
            protocol: resolve_protocol_match(ctx, scope, &header.bottom),
            name: header.name.clone(),
            scope: scope.child(),
            param: &header.param,
            shape_up: &header.top,
            implementation: DefImpl::Primitive(defs),
        });
    }

    pub fn find(&self, ctx: &'a Ctxt<'a>, shape: &Shape, name: &str, param: Item) -> (Scope, &DefImpl<'a>, Shape) {
        let mut found = None;
        for entry in &self.entries {
            if entry.name != name { continue }

            let mut scope = entry.scope.child();

            if !entry.protocol.try_match(ctx, shape, &mut scope) {
                debug!("Failed to match protocol for `{}`", name);
                continue
            }

            if let Err(err) = expr::lexpr(ctx, &mut scope, &entry.param, param.clone()) {
                debug!("Failed to match argument for `{}`: {:?}", name, err);
                continue
            }

            if found.is_none() {
                let shape_up = if let &Some(ref x) = entry.shape_up {
                    resolve_protocol_invoke(ctx, &scope, x)
                } else {
                    Shape::null()
                };

                found = Some((scope, &entry.implementation, shape_up));
            } else {
                panic!("Multiple definition of `{}`", name);
            }
        }
        found.unwrap_or_else(|| panic!("No definition found for `{}`", name))
    }
}
