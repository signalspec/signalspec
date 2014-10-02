use ast;
pub use ast::{TopType, NumberType, SymbolType, IntegerType};

/// For now, types have nothing to resolve.
/// Eventually, some type parameters will be expressions.
pub type Type = ast::TypeExpr;

/// Resolve a type AST
fn resolve_type(t: ast::TypeExpr) -> Type { t }

/// Return the intersection of the two types, or None if the types don't intersect
pub fn common_type(a: Type, b: Type) -> Option<Type>{
  match (a, b) {
    (TopType, x) | (x, TopType) => Some(x),
    (a, b) if a == b => Some(a),
    _ => None
  }
}

/// Return the intersection of a vector of types, or None if the types don't intersect
pub fn common_type_all<T:Iterator<Type>>(mut l: T) -> Option<Type> {
  l.fold(Some(TopType), |opt_a, b|{opt_a.and_then(|a| common_type(a, b))})
}

#[deriving(Clone, Show, PartialEq)]
pub enum Shape {
  ShapeUnknown,
  ShapeTup(Vec<Shape>),
  ShapeVal(Type),
}

impl Shape {
  pub fn count(&self) -> Option<uint> {
    match *self {
      ShapeUnknown => None,
      ShapeTup(ref v) => v.iter().fold(Some(0), |c, s| c.and_then(|c| s.count().map(|s| s+c))),
      ShapeVal(..) => Some(1),
    }
  }

  pub fn unify_with(&mut self, other: &Shape) -> bool {
    match (self, other) {
      (s @ &ShapeUnknown, _) => {
        *s = other.clone();
        true
      }
      (&ShapeTup(ref mut sv), &ShapeTup(ref ov)) => {
        if sv.len() != ov.len() { return false; }
        for (s, o) in sv.iter_mut().zip(ov.iter()) {
          if !s.unify_with(o) { return false;}
        }
        true
      }
      (&ShapeVal(ref mut st), &ShapeVal(ot)) => {
        match common_type(*st, ot) {
          Some(t) => {
            *st = t;
            true
          }
          None => false
        }
      }
      _ => false
    }
  }
}
