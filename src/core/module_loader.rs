use std::cell::RefCell;
use codemap::{ CodeMap, File };
use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::path::PathBuf;
use std::fs;

use crate::util::Index;
use crate::syntax::{ ast, parse_module, parse_process_chain, ParseError};
use super::{ Item, PrimitiveDef, Scope, FunctionDef, PrimitiveFn, ProcessChain, ProtocolScope, ProtocolId, Shape, Fields };
use super::{ resolve_process };


#[derive(Clone, Default, Debug)]
pub struct Config {
    pub debug_dir: Option<PathBuf>
}

pub struct Ctxt {
    pub id_counter: AtomicUsize,
    pub debug_dir: Option<PathBuf>,
    pub prelude: RefCell<Scope>,
    pub protocol_scope: RefCell<ProtocolScope>, // TODO: should be scoped
    pub protocols: Index<ProtocolDef, ProtocolId>,
    pub protocols_by_name: RefCell<BTreeMap<String, ProtocolId>>,
    pub codemap: RefCell<CodeMap>,
}

pub struct Module {
    scope: Scope,
    tests: Vec<ast::Test>,
}

pub struct ProtocolDef {
    pub ast: ast::Protocol,
    pub scope: Scope,
}

impl Ctxt {
    pub fn new(config: Config) -> Ctxt {
        if let Some(ref p) = config.debug_dir {
            fs::create_dir_all(p)
                .unwrap_or_else(|e| error!("Failed to create debug directory `{}`: {}", p.display(), e));
        }

        Ctxt {
            id_counter: AtomicUsize::new(1),
            debug_dir: config.debug_dir,
            prelude: RefCell::new(Scope::new()),
            protocol_scope: RefCell::new(ProtocolScope::new()),
            protocols: Index::new(),
            protocols_by_name: RefCell::new(BTreeMap::new()),
            codemap: RefCell::new(CodeMap::new()),
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

    pub fn add_primitive_fn(&self, name: &str, prim: PrimitiveFn) {
        self.prelude.borrow_mut().bind(name, Item::Func(Arc::new(FunctionDef::Primitive(prim))));
    }

    pub fn define_primitive(&self, header_src: &str, implementations: Vec<PrimitiveDef>) {
        let file = self.codemap.borrow_mut().add_file("<primitive>".into(), header_src.into());
        let header = crate::syntax::parse_primitive_header(&file.source(), file.span).expect("failed to parse primitive header");
        self.protocol_scope.borrow_mut().add_primitive(&*self.prelude.borrow(), header, implementations);
    }

    pub fn define_prelude(&self, source: &str) {
        let file = self.codemap.borrow_mut().add_file("<prelude>".into(), source.into());
        let module = self.parse_module(&file).expect("failed to parse prelude module");
        *self.prelude.borrow_mut() = module.scope;
    }

    pub fn parse_process(&self, source: &str, shape_below: &Shape, fields_below: &Fields) -> Result<ProcessChain, ParseError> {
        let file = self.codemap.borrow_mut().add_file("<process>".into(), source.into());
        let ast = parse_process_chain(&file.source(), file.span)?;
        Ok(resolve_process(self, &*self.prelude.borrow(), &*self.protocol_scope.borrow(), shape_below, fields_below, &ast))
    }

    pub fn parse_module(&self, file: &File) -> Result<Module, ParseError> {
        let ast = parse_module(&file.source(), file.span)?;

        let mut scope = self.prelude.borrow().child();
        let mut with_blocks = vec![];
        let mut protocols = vec![];
        let mut tests = vec![];

        for entry in ast.entries {
            match entry.node {
                ast::ModuleEntry::Let(letdef) => {
                    super::step::resolve_letdef(self, &mut scope, &letdef);
                }
                ast::ModuleEntry::Use(_) => {
                    panic!("`use` unimplemented");
                }
                ast::ModuleEntry::WithDef(def) => {
                    with_blocks.push(def);
                }
                ast::ModuleEntry::Protocol(d) => {
                    let protocol_id = self.protocols.create();
                    self.protocols_by_name.borrow_mut().insert(d.name.clone(), protocol_id);
                    protocols.push((protocol_id, d));
                }
                ast::ModuleEntry::Test(t) => {
                    tests.push(t)
                }
            }
        }

        let scope = scope; // No longer mutable

        for (id, protocol_ast) in protocols {
            self.protocols.define(id, ProtocolDef{ ast: protocol_ast, scope: scope.clone() });
        }

        let mut protocol_scope = self.protocol_scope.borrow_mut();
        for def in with_blocks {
            protocol_scope.add_def(scope.clone(), def);
        }

        Ok(Module { scope: scope, tests: tests  })
    }
}

impl Module {
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
