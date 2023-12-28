use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use crate::DiagnosticHandler;
use crate::syntax::{ ast, SourceFile };
use super::{ ProtocolRef, Item, Scope, FileScope, Shape };

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
pub(crate) struct Def {
    pub(crate) protocol: ast::ProtocolRef,
    pub(crate) name: ast::Identifier,
    pub(crate) params: Vec<ast::DefParam>,
    pub(crate) scope: Scope,
    pub(crate) implementation: ast::Process,
}

impl Index {
    pub fn new() -> Index {
        Index {
            prelude: super::expr::expr_prelude(),
            protocols_by_name: BTreeMap::new(),
            defs: Vec::new(),
        }
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

    pub(crate) fn find_def(&self, shape: &Shape, name: &str) -> Result<&Def, FindDefError> {
        let mut found = None;
        for entry in &self.defs {
            // TODO: resolve protocol.name in its file and make sure they refer to the same protocol
            if entry.protocol.name.name != shape.def.ast().name.name {
                continue;
            }

            if entry.name.name != name {
                continue;
            }

            if found.is_none() {
                found = Some(entry);
            } else {
                panic!("Multiple definition of `{}`", name);
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
                implementation: def.process.clone()
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
