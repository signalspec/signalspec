use std::sync::Arc;

use crate::diagnostic::{Diagnostic, Collector};
use crate::syntax::{ ast, parse_module, SourceFile };
use crate::core::Scope;
use std::fmt::Debug;

use super::ScopeNames;

pub struct FileScope {
    pub(crate) file: Arc<SourceFile>,
    pub(crate) names: ScopeNames,
    pub(crate) ast: ast::Module,
    pub errors: Vec<Diagnostic>,
}

impl FileScope {
    pub fn new(file: Arc<SourceFile>, prelude: &ScopeNames) -> FileScope {
        let ast = parse_module(file.source()).expect("parser failed");
        let mut scope = Scope { file, names: prelude.clone() };

        let ctx = Collector::new();
        crate::diagnostic::report_parse_errors(&ctx, &scope.file, &ast);

        for entry in &ast.entries {
            match entry {
                ast::ModuleEntry::Let(letdef) => {
                    super::resolve::action::resolve_letdef(&ctx, &mut scope, &letdef);
                }
                _ => {}
            }
        }

        FileScope { file: scope.file, names: scope.names, ast, errors: ctx.diagnostics() }
    }

    pub fn protocols<'a>(self: &'a Arc<Self>) -> impl Iterator<Item=(ProtocolRef, &'a ast::Protocol)> + 'a {
        self.ast.entries.iter().enumerate().filter_map(|(ix, node)| match (ix, node) {
            (index, ast::ModuleEntry::Protocol(p)) => Some((ProtocolRef { file: self.clone(), index }, p)),
            _ => None
        })
    }

    pub fn defs<'a>(&'a self) -> impl Iterator<Item = &'a ast::Def> {
        self.ast.entries.iter().filter_map(|node| match node {
            ast::ModuleEntry::WithDef(d) => Some(d),
            _ => None
        })
    }

    pub fn source(&self) -> &Arc<SourceFile> { &self.file }

    pub fn scope(&self) -> Scope {
        Scope {
            file: self.file.clone(),
            names: self.names.clone(),
        }
    }
}

#[derive(Clone)]
pub struct ProtocolRef {
    file: Arc<FileScope>,
    index: usize,
}

impl ProtocolRef {
    pub fn ast(&self) -> &ast::Protocol {
        match &self.file.ast.entries[self.index] {
            ast::ModuleEntry::Protocol(p) => p,
            _ => panic!("ProtocolRef points to non-protocol")
        }
    }

    pub fn file(&self) -> &Arc<FileScope> {
        &self.file
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