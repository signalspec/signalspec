use core::slice;
use std::fmt;
use std::default::Default;
use std::sync::Arc;

use crate::{Value, tree::{Tree, TupleFields}, TypeTree, diagnostic::ErrorReported, Type};
use super::{ Expr, FunctionDef };

/// Non-tuple Item
#[derive(Clone)]
pub enum LeafItem {
    /// Expression for a (possibly runtime-variable) value
    Value(Expr),

    /// Function closure
    Func(Arc<FunctionDef>),

    /// Sequence of Unicode code points. Not a Value because it is not of constant size.
    String(String),

    /// An error was previously reported
    Invalid(ErrorReported),
}

/// A compile-time compound value.
pub type Item = Tree<LeafItem>;

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
            Item::Tuple(TupleFields { positional, .. }) => &positional[..],
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

    pub fn as_type_tree(&self) -> Option<TypeTree> {
        self.try_map_leaf(&mut |t| { match t {
            LeafItem::Value(v) => Ok(v.get_type()),
            LeafItem::Invalid(_) => Ok(Type::Ignored),
            _ => Err(())
        }}).ok()
    }
}

impl Default for Item {
    fn default() -> Item {
        Item::Tuple(TupleFields::default())
    }
}

impl fmt::Display for LeafItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            LeafItem::Value(ref v) => write!(f, "{}", v),
            LeafItem::Func(..) => write!(f, "<function>"),
            LeafItem::String(ref s) => write!(f, "\"{}\"", s),
            LeafItem::Invalid(_) => write!(f, "<error>"),
        }
    }
}

impl fmt::Debug for LeafItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<ErrorReported> for LeafItem {
    fn from(value: ErrorReported) -> Self {
        LeafItem::Invalid(value)
    }
}

impl From<ErrorReported> for Item {
    fn from(value: ErrorReported) -> Self {
        Item::Leaf(LeafItem::Invalid(value))
    }
}

impl From<Expr> for Item {
    fn from(value: Expr) -> Self {
        Item::Leaf(LeafItem::Value(value))
    }
}

impl<T: Into<Item>> From<Result<T, ErrorReported>> for Item {
    fn from(value: Result<T, ErrorReported>) -> Self {
        match value {
            Ok(v) => v.into(),
            Err(r) => r.into(),
        }
    }
}

impl<'a> TryFrom<Item> for Value {
    type Error = &'static str;

    fn try_from(i: Item) -> Result<Self, Self::Error> {
        if let Item::Leaf(LeafItem::Value(Expr::Const(v))) = i { Ok(v) } else { Err("expected constant value") }
    }
}

impl<'a> TryFrom<Item> for String {
    type Error = &'static str;
    
    fn try_from(i: Item) -> Result<Self, Self::Error> {
        if let Item::Leaf(LeafItem::String(s)) = i { Ok(s) } else { Err("expected string") }
    }
}

impl<'a> TryFrom<Item> for (Item, Item) {
    type Error = &'static str;
    
    fn try_from(i: Item) -> Result<Self, Self::Error> {
        match i {
            Item::Tuple(TupleFields { positional, named }) if named.is_empty()  => {
                let [i1, i2] = <[Item; 2]>::try_from(positional).map_err(|_| "expected 2-tuple")?;
                Ok((i1, i2))
            }
            _ => Err("expected 2-tuple"),
        }
    }
}