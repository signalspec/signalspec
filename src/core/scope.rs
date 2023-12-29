use std::sync::Arc;

use crate::{SourceFile, FileSpan, diagnostic::Span};
use super::{ Item, ScopeNames };

/// A collection of named Items.
#[derive(Clone)]
pub struct Scope {
    pub file: Arc<SourceFile>,
    pub names: ScopeNames,
}

impl Scope {
    /// Create an empty `Scope`
    pub fn new(file: Arc<SourceFile>) -> Scope {
      Scope {
        file,
        names: ScopeNames::new(),
      }
    }

    /// Bind a name to a value
    pub fn bind(&mut self, name: &str, value: Item) {
        self.names.insert(name.to_string(), value);
    }

    /// Get the item associated with the name
    pub fn get(&self, name: &str) -> Option<Item> {
        self.names.get(name).cloned()
    }

    /// Create a child scope for a lexically nested block
    pub fn child(&self) -> Scope {
        Scope {
            file: self.file.clone(),
            names: self.names.clone(),
        }
    }

    pub fn span(&self, s: FileSpan) -> Span {
        Span::new(&self.file, s)
    }
}

