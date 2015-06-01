use std::fmt;
use std::collections::HashMap;
use std::default::Default;
use std::cell::RefCell;

use ast;
use resolve::types::{ShapeData, Type};
use session::{ Session, ValueID };
use eval::{ Expr, DataMode };

/// A collection of named Items.
#[derive(Clone)]
pub struct Scope<'s>{
    pub names: HashMap<String, Item<'s>>,
}

impl<'s> Scope<'s> {
    pub fn new() -> Scope<'s> {
      Scope {
        names: HashMap::new(),
      }
    }

    pub fn bind(&mut self, name: &str, value: Item<'s>) {
        self.names.insert(name.to_string(), value);
    }

    pub fn new_variable(&mut self, session: &'s Session<'s>, name: &str, ty: Type) -> ValueID {
        let id = session.make_id();
        debug!("Variable {} {}", id, name);
        self.bind(name, Item::Value(Expr::Variable(id, ty)));
        id
    }

    pub fn get(&self, name: &str) -> Option<Item<'s>> {
        self.names.get(name).cloned()
    }

    pub fn child(&self) -> Scope<'s> {
        Scope {
            names: self.names.clone(),
        }
    }
}

/// A thing associated with a name in a Scope
#[derive(Clone)]
pub enum Item<'s> {
    Value(Expr),
    Def(&'s ast::Def, &'s RefCell<Scope<'s>>),
    Tuple(Vec<Item<'s>>), // TODO: named components
    String(String), // Not a Value because it is not of constant size
}

impl<'s> Item<'s> {
    pub fn into_data_shape(self, dir: DataMode) -> ShapeData {
        match self {
            Item::Value(ref e) => ShapeData::Val(e.get_type(), dir),
            Item::Tuple(items) => ShapeData::Tup(items.into_iter().map(|x| x.into_data_shape(dir)).collect()),
            other => panic!("{:?} isn't a valid shape", other),
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
