use std::fmt;
use std::collections::HashMap;
use std::default::Default;

use resolve::block::{EventClosure};
use session::ValueID;
use resolve::types::Type;
use ast::Value;
use eval::Expr;
use exec::Message;

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
    Def(&'s EventClosure<'s>),
    Tuple(Vec<Item<'s>>), // TODO: named components
    String(String), // Not a Value because it is not of constant size
}

impl<'s> Item<'s> {
    pub fn into_message(self) -> Message {
        let mut components = Vec::new();

        fn inner<'s>(i: Item<'s>, components: &mut Vec<Expr>) {
            match i {
                Item::Value(v) => components.push(v),
                Item::Tuple(t) => for i in t { inner(i, components) },
                other => panic!("Can't send {:?}", other),
            }
        }

        inner(self, &mut components);

        Message { components: components }
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
