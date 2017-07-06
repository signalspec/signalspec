use protocol::ProtocolId;
use data::Type;
use protocol::Shape;
use super::step::{ Step, resolve_seq };
use super::scope::{Item, Scope};
use super::ast;
use super::expr;
use super::{ Ctxt, PrimitiveDef };
use super::eval::Expr;
use session::Session;


/// A pattern to match a protocol. These are used in the predicate of `with` blocks.
enum ProtocolMatch<'a> {
    Protocol { id: ProtocolId, param: &'a ast::Expr },
    Type(Type),
    Tup(Vec<ProtocolMatch<'a>>)
}

pub fn resolve_protocol_invoke<'a>(ctx: &'a Ctxt<'a>, scope: &Scope, ast: &'a ast::ProtocolRef) -> Shape {
    match *ast {
        ast::ProtocolRef::Protocol{ ref name, ref param } => {
            if let Some(Item::Protocol(protocol_id)) = scope.get(&name[..]) {
                let protocol = ctx.protocols.get(protocol_id);
                let mut protocol_def_scope = protocol.scope.child();

                match (expr::rexpr(ctx, scope, param), protocol.ast.params.len())  {
                    (item, 1) => protocol_def_scope.bind(&protocol.ast.params[0], item),
                    (Item::Tuple(t), x) => {
                        if t.len() != x {
                            panic!("Wrong number of arguments for protocol `{}`, expected {}", protocol.ast.name, x);
                        }

                        for (name, item) in protocol.ast.params.iter().zip(t.into_iter()) {
                            protocol_def_scope.bind(name, item);
                        }
                    }
                    (x, n) => panic!("Can't destructure `{:?}` as protocol parameter (required {} items)", x, n)
                }

                let mut messages = vec![];
                for entry in &protocol.ast.entries {
                    match *entry {
                        ast::ProtocolEntry::Message(ref e) => {
                            messages.push(resolve_protocol_invoke(ctx, &protocol_def_scope, e));
                        }
                    }
                }

                Shape::Protocol { def: protocol_id, messages: messages }
            } else {
                panic!("Protocol `{}` not found", name);
            }
        }
        ast::ProtocolRef::Type(ref expr) => {
            match expr::value(ctx, scope, expr) {
                Expr::Const(c) => Shape::Const(c),
                e => Shape::Val(e.get_type())
            }
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

    fn find_by_name<'m>(&'m self, _shape: &Shape, name: &str) -> Option<&'m WithBlock<'a>> {
        let mut found = None;
        for entry in &self.entries {
            if entry.name != name { continue }

            if found.is_none() {
                found = Some(entry);
            } else {
                panic!("Multiple definition of `{}`", name);
            }
        }
        found
    }

    pub fn find(&self, ctx: &'a Ctxt<'a>, shape: &Shape, name: &str, param: Item) -> (Scope, &DefImpl<'a>, Shape) {
        let block = self.find_by_name(shape, name).unwrap_or_else(|| panic!("No definition found for `{}`", name));
        let mut scope = block.scope.child();
        expr::assign(ctx.session, &mut scope, &block.param, param);

        let shape_up = if let &Some(ref x) = block.shape_up {
            resolve_protocol_invoke(ctx, &scope, x)
        } else {
            Shape::null()
        };

        (scope, &block.implementation, shape_up)
    }
}
