use std::fmt;
use std::collections::HashMap;
use std::default::Default;
use std::cell::RefCell;

use super::ast;
use data::{ Value, DataMode, Shape, ShapeVariant, ShapeData, Type };
use session::{ Session, ValueID };
use process::PrimitiveDef;
use super::eval::{ Expr };
use super::expr;

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

    /// Function literal
    Func(Func<'s>),

    /// Reference to a primitive function
    PrimitiveFn(super::module_loader::PrimitiveFn<'s>),

    /// Interface definition - `interface` block AST and enclosing scope
    Protocol(&'s ast::Protocol, &'s RefCell<Scope<'s>>),

    /// Collection of `Item`s
    Tuple(Vec<Item<'s>>), // TODO: named components

    /// Sequence of Unicode code points. Not a Value because it is not of constant size.
    String(String),
}

#[derive(Clone)]
pub struct Func<'s> {
    pub args: &'s ast::Expr,
    pub body: &'s ast::Expr,
    pub scope: Box<Scope<'s>>,
}

impl<'s> Func<'s> {
    pub fn apply(&self, session: &Session, arg: Item<'s>) -> Item<'s> {
        let mut scope = self.scope.child();
        expr::assign(session, &mut scope, self.args, arg);
        expr::rexpr(session, &mut scope, self.body)
    }
}

impl<'s> Item<'s> {
    pub fn as_constant(&self) -> Option<&Value> {
        match *self {
            Item::Value(Expr::Const(ref c)) => Some(c),
            _ => None
        }
    }

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
        fn collect_variants<'s>(session: &Session, scope: &Scope<'s>, entries: &'s [ast::ProtocolEntry], dir: DataMode) -> Shape {
            Shape {
                variants: entries.iter().map(|entry| {
                    match entry {
                        &ast::ProtocolEntry::Message(ref expr) => {
                            ShapeVariant {
                                data: super::expr::rexpr(session, scope, expr).into_data_shape(dir),
                            }
                        },
                    }
                }).collect()
            }
        }

        match self {
            Item::Protocol(ast, scope) =>  collect_variants(sess, &*scope.borrow(), &ast.entries[..], dir),
            i => Shape { variants: vec![ ShapeVariant { data: i.into_data_shape(dir) } ] }
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
            Item::Func(ref func) => write!(f, "|{:?}| {:?}", func.args, func.body),
            Item::PrimitiveFn(..) => write!(f, "<primitive fn>"),
            Item::Protocol(..) => write!(f, "<protocol>"),
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
