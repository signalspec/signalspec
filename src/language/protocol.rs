use protocol::ProtocolId;
use data::{ Shape, ShapeData, ShapeVariant, Type, DataMode };
use super::step::{ Step, ResolveInfo, resolve_seq };
use super::scope::{Item, Scope};
use super::ast;
use super::expr;
use super::eval::Expr;
use session::Session;

/// A pattern to match a protocol. These are used in the predicate of `with` blocks.
enum ProtocolMatch<'a> {
    Protocol { id: ProtocolId, param: &'a ast::Expr },
    Type(Type),
    Tup(Vec<ProtocolMatch<'a>>)
}

pub fn resolve_protocol_invoke_inner<'a>(session: &Session, scope: &Scope<'a>, ast: &'a ast::ProtocolRef, dir: DataMode) -> ShapeData {
    match *ast {
        ast::ProtocolRef::Protocol{ ref name, ref param } => {
            if let Some(Item::Protocol(protocol_id)) = scope.get(&name[..]) {
                unimplemented!();
            } else {
                panic!("Protocol `{}` not found", name);
            }
        }
        ast::ProtocolRef::Type(ref expr) => {
            match expr::value(session, scope, expr) {
                Expr::Const(c) => ShapeData::Const(c),
                e => ShapeData::Val(e.get_type(), dir)
            }
        }
        ast::ProtocolRef::Tup(ref items) => {
            let resolved_items = items.iter().map(|x| resolve_protocol_invoke_inner(session, scope, x, dir)).collect::<Vec<_>>();
            if resolved_items.len() == 1 {
                resolved_items.into_iter().next().unwrap()
            } else {
                ShapeData::Tup(resolved_items)
            }
        }
    }
}

pub fn resolve_protocol_invoke<'a>(session: &Session, scope: &Scope<'a>, ast: &'a ast::ProtocolRef, dir: DataMode) -> Shape {
    let data = resolve_protocol_invoke_inner(session, scope, ast, dir);
    Shape { variants: vec![ ShapeVariant { data: data } ] }
}

fn resolve_protocol_match<'a>(session: &Session, scope: &Scope<'a>, ast: &'a ast::ProtocolRef) -> ProtocolMatch<'a> {
    match *ast {
        ast::ProtocolRef::Protocol{ ref name, ref param } => {
            if let Some(Item::Protocol(protocol_id)) = scope.get(&name[..]) {
                ProtocolMatch::Protocol{ id: protocol_id, param: param }
            } else {
                panic!("Protocol `{}` not found", name);
            }
        }
        ast::ProtocolRef::Type(ref expr) => {
            ProtocolMatch::Type(expr::value(session, scope, expr).get_type())
        }
        ast::ProtocolRef::Tup(ref items) => {
            let resolved_items = items.iter().map(|x| resolve_protocol_match(session, scope, x)).collect::<Vec<_>>();
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
    def: &'a ast::Def,
    scope: Scope<'a>,
}

pub struct ProtocolScope<'a> {
    entries: Vec<WithBlock<'a>>,
}

impl<'a> ProtocolScope<'a> {
    pub fn new() -> Self {
        ProtocolScope { entries: vec![] }
    }

    pub fn add_def(&mut self, session: &Session, scope: Scope<'a>, def: &'a ast::Def) {
        self.entries.push(WithBlock {
            protocol: resolve_protocol_match(session, &scope, &def.bottom),
            name: def.name.clone(),
            def: def,
            scope: scope,
        });
    }

    fn find<'m>(&'m self, _shape: &Shape, name: &str) -> Option<&'m WithBlock<'a>> {
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

    pub fn call(&self, session: &Session, shape: &Shape, name: &str, param: Item<'a>) -> (Shape, Step, ResolveInfo) {
        let matched = self.find(shape, name).unwrap_or_else(|| panic!("No definition found for `{}`", name));

        let mut scope = matched.scope.child();
        let mut shape_up = if let Some(ref x) = matched.def.top {
            resolve_protocol_invoke(session, &scope, x, DataMode { down: false, up: true })
        } else {
            Shape::null()
        };

        expr::assign(session, &mut scope, &matched.def.param, param);
        let (step, ri) = resolve_seq(session, &scope, self, shape, &mut shape_up, &matched.def.block);

        (shape_up, step, ri)
    }
}
