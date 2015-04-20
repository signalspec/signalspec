use std::fmt;
use std::collections::HashMap;
use std::default::Default;

use resolve::block::{EventClosure};
use resolve::types::Shape;
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
    pub fn into_message(self, shape: &Shape) -> Message {
        let mut components = Vec::new();

        fn inner<'s>(i: Item<'s>, shape: &Shape, components: &mut Vec<Expr>) {
            match shape {
                &Shape::Val(ref _t, dir) => {
                    if let Item::Value(v) = i {
                        components.push(v.limit_direction(dir))
                    } else {
                        panic!("Expected value but found {:?}", i);
                    }
                }
                &Shape::Tup(ref m) => {
                    if let Item::Tuple(t) = i {
                        if t.len() == m.len() {
                            for (mi, i) in m.iter().zip(t.into_iter()) {
                                inner(i, mi, components)
                            }
                        } else {
                            panic!("Expected tuple length {}, found {}", m.len(), t.len());
                        }
                    } else {
                        panic!("Expected tuple of length {}, found {:?}", m.len(), i);
                    }
                }
            }
        }

        inner(self, shape, &mut components);

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
