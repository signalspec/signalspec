use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::path::PathBuf;
use std::fs;
use std::fmt::Debug;

use crate::syntax::{ ast, parse_module, parse_process_chain, ParseError, SourceFile };
use super::{ Item, PrimitiveDef, Scope, FunctionDef, PrimitiveFn, ProcessChain, ProtocolScope, Shape, Fields };
use super::{ resolve_process };


#[derive(Clone, Default, Debug)]
pub struct Config {
    pub debug_dir: Option<PathBuf>
}

pub struct FileScope {
    file: SourceFile,
    scope: Scope,
    protocols: Vec<ast::Protocol>,
    tests: Vec<ast::Test>,
}

impl FileScope {
    pub fn tests<'m>(&'m self) -> Vec<Test<'m>> {
        self.tests.iter().map(|test| {
            Test { ast: test, scope: &self.scope }
        }).collect()
    }
}

pub struct Test<'m> {
    pub scope: &'m Scope,
    pub ast: &'m ast::Test,
}

impl<'m> Test<'m> {
    pub fn should_fail(&self) -> bool { self.ast.should_fail }
}

#[derive(Clone)]
pub struct ProtocolRef {
    file: Arc<FileScope>,
    index: usize,
}

impl ProtocolRef {
    pub fn ast(&self) -> &ast::Protocol {
        &self.file.protocols[self.index]
    }

    pub fn scope(&self) -> &Scope {
        &self.file.scope
    }
}

impl PartialEq<ProtocolRef> for ProtocolRef {
    fn eq(&self, other: &ProtocolRef) -> bool {
        Arc::ptr_eq(&self.file, &other.file) && self.index == other.index
    }
}

impl Debug for ProtocolRef {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(fmt, "<protocol {}>", self.ast().name)
    }
}

pub struct Index {
    pub prelude: Scope,
    pub protocol_scope: ProtocolScope, // TODO: should be scoped
    pub protocols_by_name: BTreeMap<String, ProtocolRef>,
}

impl Index {
    pub fn new() -> Index {
        Index {
            prelude: Scope::new(),
            protocol_scope: ProtocolScope::new(),
            protocols_by_name: BTreeMap::new(),
        }
    }

    pub fn add_primitive_fn(&mut self, name: &str, prim: PrimitiveFn) {
        self.prelude.bind(name, Item::Func(Arc::new(FunctionDef::Primitive(prim))));
    }

    pub fn define_primitive(&mut self, header_src: &str, implementations: Vec<PrimitiveDef>) {
        let file = SourceFile { name: "<primitive>".into(), source: header_src.into() };
        let header = crate::syntax::parse_primitive_header(&file.source).expect("failed to parse primitive header");
        self.protocol_scope.add_primitive(&self.prelude, header, implementations);
    }

    pub fn define_prelude(&mut self, source: &str) {
        let file = SourceFile { name: "<prelude>".into(), source: source.into() };
        let module = self.parse_module(file).expect("failed to parse prelude module");
        self.prelude = module.scope.clone();
    }

    pub fn parse_module(&mut self, file: SourceFile) -> Result<Arc<FileScope>, ParseError> {
        let ast = parse_module(&file.source)?;

        let mut scope = self.prelude.child();
        let mut with_blocks = vec![];
        let mut protocols = vec![];
        let mut tests = vec![];

        for entry in ast.entries {
            match entry.node {
                ast::ModuleEntry::Let(letdef) => {
                    super::step::resolve_letdef(&mut scope, &letdef);
                }
                ast::ModuleEntry::Use(_) => {
                    panic!("`use` unimplemented");
                }
                ast::ModuleEntry::WithDef(def) => {
                    with_blocks.push(def);
                }
                ast::ModuleEntry::Protocol(d) => {
                    protocols.push(d);
                }
                ast::ModuleEntry::Test(t) => {
                    tests.push(t)
                }
            }
        }

        let file = Arc::new(FileScope { file, scope, protocols, tests });

        for (index, protocol_ast) in file.protocols.iter().enumerate() {
            self.protocols_by_name.insert(protocol_ast.name.clone(), ProtocolRef { file: file.clone(), index });
        }

        for def in with_blocks {
            self.protocol_scope.add_def(file.scope.clone(), def);
        }

        Ok(file)
    }

}

pub struct Ctxt<'a> {
    pub id_counter: AtomicUsize,
    pub debug_dir: Option<PathBuf>,
    pub index: &'a Index,
}

impl Ctxt<'_> {
    pub fn new<'a>(config: Config, index: &'a Index) -> Ctxt<'a> {
        if let Some(ref p) = config.debug_dir {
            fs::create_dir_all(p)
                .unwrap_or_else(|e| error!("Failed to create debug directory `{}`: {}", p.display(), e));
        }

        Ctxt {
            id_counter: AtomicUsize::new(1),
            debug_dir: config.debug_dir,
            index: index,
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }

    pub fn debug_file<T: FnOnce() -> String>(&self, name: T) -> Option<fs::File> {
        self.debug_dir.as_ref().and_then(|path| {
            let mut p = path.to_owned();
            p.push(name());
            fs::File::create(&p)
                .map_err(|e| error!("Failed to open debug file `{}`: {}", p.display(), e))
                .ok()
        })
    }

    pub fn parse_process(&self, source: &str, shape_below: &Shape, fields_below: &Fields) -> Result<ProcessChain, ParseError> {
        let file = SourceFile { name: "<process>".into(), source: source.into() };
        let ast = parse_process_chain(&file.source)?;
        Ok(resolve_process(self, &self.index.prelude, &self.index.protocol_scope, shape_below, fields_below, &ast))
    }
}
