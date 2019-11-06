use std::collections::BTreeMap;
use std::sync::Arc;
use crate::syntax::{ ast, ParseError, SourceFile };
use super::{ ProtocolRef, Item, PrimitiveDef, Scope, FunctionDef, PrimitiveFn, FileScope, Shape, lexpr };

pub struct Index {
    pub prelude: Scope,
    protocols_by_name: BTreeMap<String, ProtocolRef>,
    defs: Vec<Def>,
}

struct Def {
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

    pub fn define_primitive(&mut self, header_src: &str, implementations: Vec<PrimitiveDef>) {
        let file = SourceFile { name: "<primitive>".into(), source: header_src.into() };
        let header = crate::syntax::parse_primitive_header(&file.source).expect("failed to parse primitive header");
        self.defs.push(Def {
            protocol: header.bottom,
            name: header.name.clone(),
            scope: self.prelude.child(),
            param: header.param.node,
            implementation: DefImpl::Primitive(implementations, header.top),
        });
    }

    pub fn find_protocol(&self, name: &str) -> Option<&ProtocolRef> {
        self.protocols_by_name.get(name)
    }

    pub fn find_def(&self, shape: &Shape, name: &str, param: Item) -> (Scope, &DefImpl) {
        let mut found = None;
        for entry in &self.defs {
            if entry.name != name { continue }

            let mut scope = entry.scope.child();

            let protocol = self.protocols_by_name.get(&entry.protocol.name[..]).cloned().unwrap_or_else(|| {
                panic!("Failed to find protocol {:?}", entry.protocol.name);
            });
            
            debug!("Trying to match {:?} against {:?}", entry.protocol, shape);
            match shape {
                Shape::None => panic!("looking for methods of Shape::None"),
                Shape::Seq { def: r_proto, param: ref r_param, ..} => {
                    if !(&protocol == r_proto && lexpr(&mut scope, &entry.protocol.param, r_param.clone()).is_ok()) {
                        debug!("Failed to match protocol for `{}`", name);
                        continue;
                    }
                }
            }

            if let Err(err) = lexpr(&mut scope, &entry.param, param.clone()) {
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

    pub fn add_file(&mut self, file: Arc<FileScope>) {
        for (index, protocol_ast) in file.protocols.iter().enumerate() {
            self.protocols_by_name.insert(protocol_ast.name.clone(), ProtocolRef { file: file.clone(), index });
        }

        for def in &file.defs {
            self.defs.push(Def {
                protocol: def.bottom.clone(),
                name: def.name.clone(),
                scope: file.scope.clone(),
                param: def.param.node.clone(),
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
