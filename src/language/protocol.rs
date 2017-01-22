use std::cell::RefCell;
use protocol::{ProtocolDef, ProtocolMessageDef, ProtocolId};
use data::{ Shape, Type };
use super::program::ProcessDef;
use process::PrimitiveDef;
use super::scope::{Item, Scope};
use super::ast;
use super::eval::Expr;
use super::expr;
use session::Session;


fn into_protocol_message_def(i: Item) -> ProtocolMessageDef {
    match i {
        Item::Value(Expr::Const(c)) => ProtocolMessageDef::Const(c),
        Item::Value(ref e) => ProtocolMessageDef::Val(e.get_type()),
        Item::Tuple(items) => ProtocolMessageDef::Tup(items.into_iter().map(into_protocol_message_def).collect()),
        other => panic!("{:?} isn't a valid message component", other),
    }
}

pub fn resolve_protocol<'a>(session: &Session, scope: &Scope<'a>, ast: &'a ast::Protocol) -> ProtocolDef {
    let messages = ast.entries.iter().map(|entry| {
        match entry {
            &ast::ProtocolEntry::Message(ref expr) => {
                into_protocol_message_def(super::expr::rexpr(session, scope, expr))
            },
        }
    }).collect();

    ProtocolDef {
        name: ast.name.clone(),
        params: ast.params.clone(),
        messages: messages,
    }
}

enum ProtocolMatch<'a> {
    Protocol { id: ProtocolId, param: &'a ast::Expr },
    Type(Type),
    Tup(Vec<ProtocolMatch<'a>>)
}

fn resolve_protocol_match<'a>(session: &Session, scope: &Scope<'a>, ast: &'a ast::With) -> ProtocolMatch<'a> {
    match *ast {
        ast::With::Protocol{ ref name, ref param } => {
            if let Some(Item::Protocol(protocol_id)) = scope.get(&name[..]) {
                ProtocolMatch::Protocol{ id: protocol_id, param: param }
            } else {
                panic!("Protocol `{}` not found", name);
            }
        }
        ast::With::Type(ref expr) => {
            ProtocolMatch::Type(expr::value(session, scope, expr).get_type())
        }
        ast::With::Tup(ref items) => {
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
    process: ProcessEntry<'a>,
}

enum ProcessEntry<'a> {
    Code {
        def: &'a ast::Def,
        scope: &'a RefCell<Scope<'a>>,
    },
    Primitive(&'a PrimitiveDef)
}

pub struct ProtocolScope<'a> {
    entries: Vec<WithBlock<'a>>,
}

impl<'a> ProtocolScope<'a> {
    pub fn new() -> Self {
        ProtocolScope { entries: vec![] }
    }

    pub fn add_def(&mut self, session: &Session, scope: &'a RefCell<Scope<'a>>, with: &'a ast::With, def: &'a ast::Def) {
        self.entries.push(WithBlock {
            protocol: resolve_protocol_match(session, &*scope.borrow(), with),
            name: def.name.clone(),
            process: ProcessEntry::Code{ def: def, scope: &scope },
        });
    }

    pub fn add_primitive(&mut self, session: &Session, name: &str, primitive: &'a PrimitiveDef) {
        self.entries.push(WithBlock {
            protocol: ProtocolMatch::Tup(vec![]), //TODO: allow this to be specified
            name: name.to_owned(),
            process: ProcessEntry::Primitive(primitive),
        });
    }

    pub fn find(&self, _shape: &Shape, name: &str) -> Option<ProcessDef<'a>> {
        let mut found = None;
        for entry in &self.entries {
            if entry.name != name { continue }

            if found.is_none() {
                found = Some(match entry.process {
                    ProcessEntry::Code{ def, scope } => ProcessDef::Code(def, scope),
                    ProcessEntry::Primitive(p) => ProcessDef::Primitive(p),
                });
            } else {
                panic!("Multiple definition of `{}``", name);
            }
        }
        found
    }
}
