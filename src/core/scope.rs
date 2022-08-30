use core::slice;
use std::fmt;
use std::collections::HashMap;
use std::default::Default;
use std::sync::Arc;

use crate::{syntax::Value, tree::Tree, SourceFile};
use super::{ Expr, FunctionDef };

pub type ScopeNames = HashMap<String, Item>;

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
        names: HashMap::new(),
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

    pub fn get_as<'a, T: FromItem<'a>>(&'a self, name: &str) -> Result<T, ()> {
        match self.names.get(name) {
            Some(i) => T::try_from_item(i).ok_or(()),
            None => Err(())
        }
    }

    /// Create a child scope for a lexically nested block
    pub fn child(&self) -> Scope {
        Scope {
            file: self.file.clone(),
            names: self.names.clone(),
        }
    }
}

/// A thing associated with a name in a Scope
#[derive(Clone)]
pub enum LeafItem {
    /// Expression for a (possibly runtime-variable) value
    Value(Expr),

    // Function closure
    Func(Arc<FunctionDef>),

    /// Sequence of Unicode code points. Not a Value because it is not of constant size.
    String(String),
}

pub type Item = Tree<LeafItem>;

pub trait FromItem<'a>: Sized { // TODO: use TryFrom once stable
    fn try_from_item(_: &'a Item) -> Option<Self>;
}

impl<'a> FromItem<'a> for &'a str {
    fn try_from_item(i: &'a Item) -> Option<Self> {
        if let &Item::Leaf(LeafItem::String(ref s)) = i { Some(s) } else { None }
    }
}

impl<'a> FromItem<'a> for &'a Item {
    fn try_from_item(i: &'a Item) -> Option<Self> { Some(i) }
}

impl Item {
    pub fn value(v: Value) -> Item {
        Item::Leaf(LeafItem::Value(Expr::Const(v)))
    }

    pub fn symbol(s: &str) -> Item {
        Item::value(Value::Symbol(s.into()))
    }

    pub fn as_tuple(&self) -> &[Item] {
        match self {
            Item::Leaf(_) => slice::from_ref(self),
            Item::Tuple(v) => &v[..],
        }
    }

    pub fn as_constant(&self) -> Option<&Value> {
        match *self {
            Item::Leaf(LeafItem::Value(Expr::Const(ref c))) => Some(c),
            _ => None
        }
    }

    pub fn as_symbol(&self) -> Option<&str> {
        match self.as_constant() {
            Some(Value::Symbol(s)) => Some(s),
            _ => None,
        }
    }
}

impl Default for Item {
    fn default() -> Item {
        Item::Tuple(Vec::new())
    }
}

impl fmt::Debug for LeafItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            LeafItem::Value(ref v) => write!(f, "{:?}", v),
            LeafItem::Func(..) => write!(f, "<function>"),
            LeafItem::String(ref s) => write!(f, "{:?}", s),
        }
    }
}
