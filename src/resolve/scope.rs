use std::fmt;
use std::collections::HashMap;
use std::default::Default;
use std::cell::RefCell;

use ast;
use resolve;
use data::{ DataMode, Shape, ShapeVariant, ShapeData, Type };
use session::{ Session, ValueID };
use process::PrimitiveDef;
use eval::Expr;

/// A collection of named Items.
#[derive(Clone)]
pub struct Scope<'s>{
    pub names: HashMap<String, Item<'s>>,
}

impl<'s> Scope<'s> {
    /// Create an empty `Scope`
    pub fn new() -> Scope<'s> {
      Scope {
        names: HashMap::new(),
      }
    }

    /// Bind a name to a value
    pub fn bind(&mut self, name: &str, value: Item<'s>) {
        self.names.insert(name.to_string(), value);
    }

    /// Create a new runtime variable, bind it to a name in this scope, and return its ID
    pub fn new_variable(&mut self, session: &Session, name: &str, ty: Type) -> ValueID {
        let id = session.make_id();
        debug!("Variable {} {}", id, name);
        self.bind(name, Item::Value(Expr::Variable(id, ty)));
        id
    }

    /// Get the item associated with the name
    pub fn get(&self, name: &str) -> Option<Item<'s>> {
        self.names.get(name).cloned()
    }

    /// Create a child scope for a lexically nested block
    pub fn child(&self) -> Scope<'s> {
        Scope {
            names: self.names.clone(),
        }
    }
}

/// A thing associated with a name in a Scope
#[derive(Clone)]
pub enum Item<'s> {
    /// Expression for a (possibly runtime-variable) value
    Value(Expr),

    /// Signal definition - `def` block AST and enclosing scope
    Def(&'s ast::Def, &'s RefCell<Scope<'s>>),

    /// Reference to a primitive
    PrimitiveDef(&'s PrimitiveDef),

    /// Interface definition - `interface` block AST and enclosing scope
    Interface(&'s ast::Interface, &'s RefCell<Scope<'s>>),

    /// Collection of `Item`s
    Tuple(Vec<Item<'s>>), // TODO: named components

    /// Sequence of Unicode code points. Not a Value because it is not of constant size.
    String(String),
}

impl<'s> Item<'s> {
    /// Get a `ShapeData` corresponding to a tree of `Tuple` and `Value` `Item`s
    pub fn into_data_shape(self, dir: DataMode) -> ShapeData {
        match self {
            Item::Value(Expr::Const(c)) => ShapeData::Const(c),
            Item::Value(ref e) => ShapeData::Val(e.get_type(), dir),
            Item::Tuple(items) => ShapeData::Tup(items.into_iter().map(|x| x.into_data_shape(dir)).collect()),
            other => panic!("{:?} isn't a valid shape", other),
        }
    }

    /// Get a `Shape` corresponding to an `Interface` or data example
    pub fn into_shape(self, sess: &Session, dir: DataMode) -> Shape {
        match self {
            Item::Interface(ast, scope) =>  resolve::interface(sess, ast, &*scope.borrow(), dir),
            i => Shape { variants: vec![ ShapeVariant { data: i.into_data_shape(dir) } ] }
        }
    }
}

impl<'s> PartialEq for Item<'s> {
    fn eq(&self, other: &Item<'s>) -> bool {
        match (self, other) {
            (&Item::Value(ref va), &Item::Value(ref vb)) => va == vb,
            (&Item::Tuple(ref va), &Item::Tuple(ref vb)) => va == vb,
            (&Item::String(ref a), &Item::String(ref b)) => a == b,
            _ => false
        }
    }
}

impl <'s> Default for Item<'s> {
    fn default() -> Item<'s> {
        Item::Tuple(Vec::new())
    }
}

impl<'s> fmt::Debug for Item<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Item::Value(ref v) => write!(f, "{:?}", v),
            Item::Def(..) => write!(f, "<def>"),
            Item::PrimitiveDef(..) => write!(f, "<primitive def>"),
            Item::Interface(..) => write!(f, "<interface>"),
            Item::Tuple(ref v) => {
                try!(write!(f, "("));
                for i in v.iter() {
                    try!(i.fmt(f));
                    try!(write!(f, ", "));
                }
                write!(f, ")")
            }
            Item::String(ref s) => write!(f, "{:?}", s),
        }
    }
}
