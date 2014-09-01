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
  pub names: HashMap<String, &'s Item<'s>>,
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

  pub fn add_param(&mut self, param_def: &ast::Expr, param_value: &'s Item<'s>) {
    match *param_def {
      ast::VarExpr(ref name) => {
        self.names.insert(name.to_string(), param_value);
      }
      _ => fail!("Unimplemented expression form in parameter")
    }
  }

  pub fn get(&self, name: &str) -> Option<&'s Item<'s>> {
    self.names.find_equiv(&name).map(|x| x.clone())
  }

  pub fn child(&self) -> Scope<'s> {
    Scope {
      names: self.names.clone(),
    }
  }
}

/// A thing associated with a name in a Scope
pub enum Item<'s> {
  EmptyItem, // TODO: 0-item tuple
  DefItem(EventClosure<'s>),
  ValueItem(Type, ValueRef /*Down*/, ValueRef /*Up*/)
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
  pub fn const_down(&self) -> Option<Value> {
    match *self {
      Ignored => None,
      Constant(ref v) => Some(v.clone()),
      Dynamic(..) => fail!("const_down of a dynamic!"),
      Poison(s) => fail!("const_down of a poison: {}", s),
    }
  }

  pub fn const_up(&self, vo: Option<&Value>) -> bool {
    match vo {
        Some(v) => {
          match *self {
            Ignored => true,
            Constant(ref p) => v == p,
            Dynamic(..) => fail!("const_up of a dynamic!"),
            Poison(s) => fail!("const_up of a poison: {}", s),
          }
        }
        None => true
    }
  }
}
