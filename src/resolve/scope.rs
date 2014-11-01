use std::fmt;
use std::collections::hashmap::HashMap;
use std::default::Default;

use resolve::block::{EventClosure};
use resolve::context::ValueID;
use resolve::types::Type;
use ast;
use ast::Value;

/// A collection of named Items.
#[deriving(Clone)]
pub struct Scope<'s>{
    pub names: HashMap<String, Item<'s>>,
}

impl<'s> Scope<'s> {
    pub fn new() -> Scope<'s> {
      Scope {
        names: HashMap::new(),
      }
    }

    pub fn add_lets(&mut self, lets: &[ast::LetDef]) {
        for _letdef in lets.iter() {
            panic!("Let unimplemented");
        }
    }

    pub fn bind(&mut self, name: &str, value: Item<'s>) {
        self.names.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Option<Item<'s>> {
        self.names.find_equiv(name).map(|x| x.clone())
    }

    pub fn child(&self) -> Scope<'s> {
        Scope {
            names: self.names.clone(),
        }
    }
}

/// A thing associated with a name in a Scope
#[deriving(Clone)]
pub enum Item<'s> {
    ConstantItem(Value),
    ValueItem(Type, ValueRef /*Down*/, ValueRef /*Up*/),
    DefItem(&'s EventClosure<'s>),
    TupleItem(Vec<Item<'s>>) // TODO: named components
}

impl<'s> PartialEq for Item<'s> {
    fn eq(&self, other: &Item<'s>) -> bool {
        match (self, other) {
            (&ValueItem(ref ta, ref da, ref ua), &ValueItem(ref tb, ref db, ref ub))
                if ta==tb && da==db && ua==ub => true,
            (&ConstantItem(ref a), &ConstantItem(ref b)) if a == b => true,
            _ => false
        }
    }
}

impl <'s> Default for Item<'s> {
    fn default() -> Item<'s> {
        TupleItem(Vec::new())
    }
}

impl<'s> fmt::Show for Item<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ConstantItem(ref v) => write!(f, "{}", v),
            ValueItem(t, d, u) => write!(f, "[{}: {} {}]", t, d, u),
            DefItem(..) => write!(f, "<def>"),
            TupleItem(ref v) => {
                try!(write!(f, "("));
                for i in v.iter() {
                    try!(i.fmt(f));
                    try!(write!(f, ", "));
                }
                write!(f, ")")
            }
        }
    }
}

/// A constant value or ID for obtaining a value at runtime
#[deriving(PartialEq, Clone)]
pub enum ValueRef {
    Ignored,

    /// down: register to load the value from
    /// up: the register to store the value into
    Dynamic(ValueID),

    /// The use of the value is illegal. Contains the error message to show if the value is used.
    Poison(&'static str),
}

impl ValueRef {
    pub fn propagate(self, f: |ValueID| -> ValueRef) -> ValueRef {
        match self {
            Dynamic(id) => f(id),
            _ => self,
        }
    }

    pub fn value_id(self) -> Option<ValueID> {
        match self {
            Dynamic(id) => Some(id),
            Ignored => None,
            Poison(s) => panic!("Use of an invalid value: {}", s),
        }
    }
}

pub fn propagate_pair(a: ValueRef, b: ValueRef, f: |ValueID, ValueID| -> ValueRef) -> ValueRef {
    match (a, b) {
        (Dynamic(id_a), Dynamic(id_b)) => f(id_a, id_b),
        (p @ Poison(..), _) | (_, p @ Poison(..)) => p,
        (Ignored, _) | (_, Ignored) => Ignored,
    }
}

impl fmt::Show for ValueRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
        match *self {
            Ignored => write!(f, "ignore"),
            Dynamic(c) => write!(f, "%{}", c),
            Poison(s) => write!(f, "poison: {}", s),
        }
    }
}
