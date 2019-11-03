use std::fmt;
use std::collections::HashMap;
use std::default::Default;
use std::sync::Arc;

use crate::syntax::Value;
use super::{ Type, Ctxt, ValueId, Expr, FunctionDef };

/// A collection of named Items.
#[derive(Clone)]
pub struct Scope{
    pub names: HashMap<String, Item>
}

impl Scope {
    /// Create an empty `Scope`
    pub fn new() -> Scope {
      Scope {
        names: HashMap::new(),
      }
    }

    /// Bind a name to a value
    pub fn bind(&mut self, name: &str, value: Item) {
        self.names.insert(name.to_string(), value);
    }

    /// Create a new runtime variable, bind it to a name in this scope, and return its ID
    pub fn new_variable(&mut self, ctxt: &Ctxt, name: &str, ty: Type) -> ValueId {
        let id = ctxt.make_id();
        debug!("Variable {} {}", id, name);
        self.bind(name, Item::Value(Expr::Variable(id, ty)));
        id
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
            names: self.names.clone(),
        }
    }
}

/// A thing associated with a name in a Scope
#[derive(Clone)]
pub enum Item {
    /// Expression for a (possibly runtime-variable) value
    Value(Expr),

    // Function closure
    Func(Arc<FunctionDef>),

    /// Collection of `Item`s
    Tuple(Vec<Item>), // TODO: named components

    /// Sequence of Unicode code points. Not a Value because it is not of constant size.
    String(String),
}

pub trait FromItem<'a>: Sized { // TODO: use TryFrom once stable
    fn try_from_item(_: &'a Item) -> Option<Self>;
}

impl<'a> FromItem<'a> for &'a str {
    fn try_from_item(i: &'a Item) -> Option<Self> {
        if let &Item::String(ref s) = i { Some(s) } else { None }
    }
}

impl<'a> FromItem<'a> for &'a Item {
    fn try_from_item(i: &'a Item) -> Option<Self> { Some(i) }
}

impl Item {
    pub fn as_constant(&self) -> Option<&Value> {
        match *self {
            Item::Value(Expr::Const(ref c)) => Some(c),
            _ => None
        }
    }
}

impl Default for Item {
    fn default() -> Item {
        Item::Tuple(Vec::new())
    }
}

impl fmt::Debug for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Item::Value(ref v) => write!(f, "{:?}", v),
            Item::Func(..) => write!(f, "<function>"),
            Item::Tuple(ref v) => {
                write!(f, "(")?;
                for i in v.iter() {
                    i.fmt(f)?;
                    write!(f, ", ")?;
                }
                write!(f, ")")
            }
            Item::String(ref s) => write!(f, "{:?}", s),
        }
    }
}
