use std::sync::Arc;

use crate::syntax::{ ast, parse_module, ParseError, SourceFile };
use crate::core::Scope;
use std::fmt::Debug;

use super::scope::ScopeNames;

pub struct FileScope {
    pub(crate) scope: Scope,
    pub(crate) protocols: Vec<ast::Protocol>,
    pub(crate) defs: Vec<ast::Def>,
}

impl FileScope {
    pub fn new(file: Arc<SourceFile>, prelude: &ScopeNames) -> Result<FileScope, ParseError> {
        let ast = parse_module(file.source())?;

        let mut scope = Scope { file, names: prelude.clone() };
        let mut defs = vec![];
        let mut protocols = vec![];

        for entry in ast.entries {
            match entry {
                ast::ModuleEntry::Let(letdef) => {
                    super::resolve::resolve_letdef(&mut scope, &letdef);
                }
                ast::ModuleEntry::WithDef(def) => {
                    defs.push(def);
                }
                ast::ModuleEntry::Protocol(d) => {
                    protocols.push(d);
                }
            }
        }

        Ok(FileScope { scope, defs, protocols })
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
        write!(fmt, "<protocol {}>", self.ast().name.name)
    }
}