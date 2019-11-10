use std::collections::BTreeMap;
use std::sync::Arc;
use crate::syntax::{ ast, ParseError, SourceFile };
use super::{ ProtocolRef, Item, PrimitiveDef, Scope, FunctionDef, PrimitiveFn, FileScope, Shape };

pub struct Index {
    pub prelude: Scope,
    protocols_by_name: BTreeMap<String, ProtocolRef>,
    defs: Vec<Def>,
}

struct Def {
    protocol: ast::ProtocolRef,
    name: String,
    params: Vec<ast::DefParam>,
    scope: Scope,
    implementation: DefImpl
}

pub (crate) enum DefImpl {
    Code(Vec<ast::Process>),
    Primitive(PrimitiveDef, Option<ast::ProtocolRef>)
 }

impl Index {
    pub fn new() -> Index {
        Index {
            prelude: Scope::new(),
            protocols_by_name: BTreeMap::new(),
            defs: Vec::new(),
        }
    }

    pub fn add_primitive_fn(&mut self, name: &str, prim: PrimitiveFn) {
        self.prelude.bind(name, Item::Func(Arc::new(FunctionDef::Primitive(prim))));
    }

    pub fn define_prelude(&mut self, source: &str) {
        let file = SourceFile { name: "<prelude>".into(), source: source.into() };
        let module = self.parse_module(file).expect("failed to parse prelude module");
        self.prelude = module.scope.clone();
    }

    pub fn define_primitive(&mut self, header_src: &str, implementation: PrimitiveDef) {
        let file = SourceFile { name: "<primitive>".into(), source: header_src.into() };
        let header = crate::syntax::parse_primitive_header(&file.source).expect("failed to parse primitive header");
        self.defs.push(Def {
            protocol: header.bottom,
            name: header.name.clone(),
            scope: self.prelude.child(),
            params: header.params.iter().map(|x| x.node.clone()).collect(),
            implementation: DefImpl::Primitive(implementation, header.top),
        });
    }

    pub fn find_protocol(&self, name: &str) -> Option<&ProtocolRef> {
        self.protocols_by_name.get(name)
    }

    pub(crate) fn find_def(&self, shape: &Shape, name: &str, args: Vec<Item>) -> (Scope, &DefImpl) {
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
        found.unwrap_or_else(|| panic!("No definition found for `{}`", name))
    }

    pub fn add_file(&mut self, file: Arc<FileScope>) {
        for (index, protocol_ast) in file.protocols.iter().enumerate() {
            self.protocols_by_name.insert(protocol_ast.name.clone(), ProtocolRef { file: file.clone(), index });
        }

        for def in &file.defs {
            self.defs.push(Def {
                protocol: def.bottom.clone(),
                name: def.name.clone(),
                scope: file.scope.clone(),
                params: def.params.iter().map(|x| x.node.clone()).collect(),
                implementation: DefImpl::Code(def.processes.clone())
            });
        }
    }

    pub fn parse_module(&mut self, file: SourceFile) -> Result<Arc<FileScope>, ParseError> {
        let file = Arc::new(FileScope::new(file, &self.prelude)?);
        self.add_file(file.clone());
        Ok(file)
    }
}

/// Match a shape, name, and argument against a candidate with-def block. If
/// matched, returns a scope for the inside of the block.
fn match_def(def: &Def, shape: &Shape, name: &str, args: &[Item]) -> Option<Scope> {
    if def.name != name {
        return None;
    }

    let mut scope = def.scope.child();

    if !crate::core::protocol::match_protocol(&mut scope, &def.protocol, &shape) {
        return None;
    }

    if !crate::core::protocol::match_def_params(&mut scope, &def.params, args) {
        return None;
    }

    Some(scope)
}
