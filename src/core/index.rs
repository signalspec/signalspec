use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use crate::DiagnosticHandler;
use crate::syntax::{ ast, SourceFile };
use super::{ ProtocolRef, Item, PrimitiveDef, Scope, FunctionDef, PrimitiveFn, FileScope, Shape, scope::LeafItem };

fn default_library_dir() -> Option<PathBuf> {
    Some(std::env::current_exe().ok()?.parent()?.parent()?.join("lib/signalspec"))
}

#[derive(Clone)]
pub struct Index {
    prelude: HashMap<String, Item>,
    protocols_by_name: BTreeMap<String, ProtocolRef>,
    defs: Vec<Def>,
}

#[derive(Clone)]
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

 impl Clone for DefImpl {
    fn clone(&self) -> Self {
        match self {
            Self::Code(arg0) => Self::Code(arg0.clone()),
            Self::Primitive(..) => unreachable!(),
        }
    }
}

impl Index {
    pub fn new() -> Index {
        Index {
            prelude: super::expr::expr_prelude(),
            protocols_by_name: BTreeMap::new(),
            defs: Vec::new(),
        }
    }

    pub fn add_primitive_fn(&mut self, name: &str, prim: PrimitiveFn) {
        self.prelude.insert(name.to_owned(), Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Primitive(prim)))));
    }

    /// Create an index and populate it with the libraries from the colon-separated
    /// environment variable SIGNALSPEC_PATH. If not set or contains empty components,
    /// the path <signalspec executable>/../../lib/signalspec is used.
    pub fn from_env(ui: &dyn DiagnosticHandler) -> Result<Index, std::io::Error> {
        let mut index = Self::new();
        for path in std::env::var("SIGNALSPEC_PATH").unwrap_or_default().split(":") {
            if path.is_empty() {
                if let Some(dir) = default_library_dir() {
                    index.load(ui, &dir)?;
                }
            } else {
                index.load(ui, Path::new(path))?;
            }
        }
        Ok(index)
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

    pub fn load(&mut self, ui: &dyn DiagnosticHandler, p: &Path) -> Result<(), std::io::Error> {
        info!("Loading {}", p.display());
        match std::fs::metadata(p) {
            Ok(ref meta) if meta.is_dir() => {
                for entry in std::fs::read_dir(p)? {
                    let entry = entry?;
                    let path = entry.path();
                    let m = entry.metadata()?;
                    if path.extension() == Some("signalspec".as_ref()) || m.is_dir() {
                        self.load(ui, &path)?;
                    }
                }
                Ok(())
            }
            Ok(_) => {
                let module = self.parse_module(Arc::new(SourceFile::load(p)?));
                ui.report_all(module.errors.clone());
                Ok(())
            }
            Err(e) => Err(e)
        }
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
