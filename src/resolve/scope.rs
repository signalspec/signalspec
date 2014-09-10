use std::fmt;
use std::collections::hashmap::HashMap;

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
      fail!("Let unimplemented");
    }
  }

  pub fn bind(&mut self, name: &str, value: Item<'s>) {
    self.names.insert(name.to_string(), value);
  }

  pub fn get(&self, name: &str) -> Option<Item<'s>> {
    self.names.find_equiv(&name).map(|x| x.clone())
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
  EmptyItem, // TODO: 0-item tuple
  DefItem(&'s EventClosure<'s>),
  ValueItem(Type, ValueRef /*Down*/, ValueRef /*Up*/),
  TupleItem(Vec<Item<'s>>) // TODO: named components
}

impl<'s> PartialEq for Item<'s> {
  fn eq(&self, other: &Item<'s>) -> bool {
    match (self, other) {
      (&ValueItem(ref ta, ref da, ref ua), &ValueItem(ref tb, ref db, ref ub))
        if ta==tb && da==db && ua==ub => true,
      _ => false
    }
  }
}

impl<'s> fmt::Show for Item<'s> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?}", self)
  }
}

impl<'s> Item<'s> {
  fn flatten_into(&self, down: &mut Vec<ValueRef>, up: &mut Vec<ValueRef>) {
    // TODO: check shape
    match *self {
      EmptyItem => (),
      ValueItem(_, ref d, ref u) => {
        down.push(d.clone());
        up.push(u.clone());
      },
      TupleItem(ref t) => for i in t.iter() { i.flatten_into(down, up) },
      DefItem(..) => fail!("Cannot flatten non-sendable expression")
    }
  }

  pub fn flatten(&self) -> (Vec<ValueRef>, Vec<ValueRef>) {
    let mut down = Vec::new();
    let mut up = Vec::new();
    self.flatten_into(&mut down, &mut up);
    (down, up)
  }
}


/// A constant value or ID for obtaining a value at runtime
#[deriving(PartialEq, Clone)]
pub enum ValueRef {
  Ignored,
  Constant(Value),
  Dynamic(ValueID),
  Poison(&'static str),
}

impl fmt::Show for ValueRef {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
    match *self {
      Ignored => write!(f, "ignore"),
      Constant(ref v) => write!(f, "{}", v),
      Dynamic(c) => write!(f, "%{}", c),
      Poison(s) => write!(f, "poison: {}", s),
    }
  }
}

impl ValueRef {
  pub fn const_down(&self) -> Value {
    match *self {
      Ignored => ast::NumberValue(0.), // should fail?
      Constant(ref v) => v.clone(),
      Dynamic(..) => fail!("const_down of a dynamic!"),
      Poison(s) => fail!("const_down of a poison: {}", s),
    }
  }

  pub fn const_up(&self, v: &Value) -> bool {
    match *self {
      Ignored => true,
      Constant(ref p) => v == p,
      Dynamic(..) => fail!("const_up of a dynamic!"),
      Poison(s) => fail!("const_up of a poison: {}", s),
    }
  }
}
