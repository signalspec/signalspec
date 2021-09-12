use std::sync::Arc;

use crate::syntax::{ ast, parse_module, ParseError, SourceFile };
use crate::core::Scope;
use std::fmt::Debug;

pub struct FileScope {
    pub(crate) file: SourceFile,
    pub(crate) scope: Scope,
    pub(crate) protocols: Vec<ast::Protocol>,
    pub(crate) defs: Vec<ast::Def>,
    pub(crate) tests: Vec<ast::Test>,
}

impl FileScope {
    pub fn new(file: SourceFile, prelude: &Scope) -> Result<FileScope, ParseError> {
        let ast = parse_module(&file.source)?;

        let mut scope = prelude.child();
        let mut defs = vec![];
        let mut protocols = vec![];
        let mut tests = vec![];

        for entry in ast.entries {
            match entry.node {
                ast::ModuleEntry::Let(letdef) => {
                    super::resolve::resolve_letdef(&mut scope, &letdef);
                }
                ast::ModuleEntry::Use(_) => {
                    panic!("`use` unimplemented");
                }
                ast::ModuleEntry::WithDef(def) => {
                    defs.push(def);
                }
                ast::ModuleEntry::Protocol(d) => {
                    protocols.push(d);
                }
                ast::ModuleEntry::Test(t) => {
                    tests.push(t)
                }
            }
        }

        Ok(FileScope { file, scope, defs, protocols, tests })
    }
}

#[derive(Clone)]
pub struct ProtocolRef {
    pub(crate) file: Arc<FileScope>,
    pub(crate) index: usize,
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