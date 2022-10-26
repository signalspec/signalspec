use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use crate::syntax::{ ast, SourceFile };
use super::{ ProtocolRef, Item, PrimitiveDef, Scope, FunctionDef, PrimitiveFn, FileScope, Shape, scope::LeafItem };

pub struct Index {
    prelude: HashMap<String, Item>,
    protocols_by_name: BTreeMap<String, ProtocolRef>,
    defs: Vec<Def>,
}

struct Def {
    protocol: ast::ProtocolRef,
    name: ast::Identifier,
    params: Vec<ast::DefParam>,
    scope: Scope,
    implementation: DefImpl
}

pub (crate) enum DefImpl {
    Code(ast::Process),
    Primitive(PrimitiveDef, Option<ast::ProtocolRef>)
 }

impl Index {
    pub fn new() -> Index {
        Index {
            prelude: HashMap::new(),
            protocols_by_name: BTreeMap::new(),
            defs: Vec::new(),
        }
    }

    pub fn add_primitive_fn(&mut self, name: &str, prim: PrimitiveFn) {
        self.prelude.insert(name.to_owned(), Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Primitive(prim)))));
    }

    pub fn define_prelude(&mut self, source: &str) {
        let file = Arc::new(SourceFile::new("<prelude>".into(), source.into()));
        let module = self.parse_module(file);
        self.prelude = module.scope.names.clone();
    }

    pub fn define_primitive(&mut self, header_src: &str, implementation: PrimitiveDef) {
        let file = Arc::new(SourceFile::new("<primitive>".into(), header_src.into()));
        let header = crate::syntax::parse_primitive_header(file.source()).expect("failed to parse primitive header");
        self.defs.push(Def {
            protocol: header.bottom,
            name: header.name.clone(),
            scope: Scope { file, names: self.prelude.clone() },
            params: header.params.iter().map(|x| x.clone()).collect(),
            implementation: DefImpl::Primitive(implementation, header.top),
        });
    }

    pub fn find_protocol(&self, name: &str) -> Option<&ProtocolRef> {
        self.protocols_by_name.get(name)
    }

    pub(crate) fn find_def(&self, shape: &Shape, name: &str, args: Vec<Item>) -> Result<(Scope, &DefImpl), FindDefError> {
        let mut found = None;
        for entry in &self.defs {
            if let Some(scope) = match_def(entry, shape, name, &args) {
                if found.is_none() {
                    found = Some((scope, &entry.implementation));
                } else {
                    panic!("Multiple definition of `{}`", name);
                }
            }
        }
        found.ok_or(FindDefError::NoDefinitionWithName)
    }

    pub fn add_file(&mut self, file: Arc<FileScope>) {
        for (protocol_ref, protocol_ast) in file.protocols() {
            self.protocols_by_name.insert(protocol_ast.name.name.clone(), protocol_ref);
        }

        for def in file.defs() {
            self.defs.push(Def {
                protocol: def.bottom.clone(),
                name: def.name.clone(),
                scope: file.scope.clone(),
                params: def.params.iter().map(|x| x.clone()).collect(),
                implementation: DefImpl::Code(def.process.clone())
            });
        }
    }

    pub fn remove_file(&mut self, file: &Arc<FileScope>) {
        for (_, protocol_ast) in file.protocols() {
            self.protocols_by_name.remove(&protocol_ast.name.name);
        }

        self.defs.retain(|d| {
            !Arc::ptr_eq(&d.scope.file, &file.scope.file)
        });
    }

    pub fn parse_module(&mut self, file: Arc<SourceFile>) -> Arc<FileScope> {
        let file = Arc::new(FileScope::new(file, &self.prelude));
        self.add_file(file.clone());
        file
    }

    /// Get a reference to the index's prelude.
    pub fn prelude(&self) -> &HashMap<String, Item> {
        &self.prelude
    }
}

pub enum FindDefError {
    NoDefinitionWithName,
}

/// Match a shape, name, and argument against a candidate with-def block. If
/// matched, returns a scope for the inside of the block.
fn match_def(def: &Def, shape: &Shape, name: &str, args: &[Item]) -> Option<Scope> {
    if def.name.name != name {
        return None;
    }

    let mut scope = def.scope.child();

    if !crate::core::protocol::match_protocol(&mut scope, &def.protocol, shape) {
        return None;
    }

    if !crate::core::protocol::match_def_params(&mut scope, &def.params, args) {
        return None;
    }

    Some(scope)
}
