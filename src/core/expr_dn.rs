use super::ValueSrc;
use crate::Value;
use super::op::UnaryOp;

/// An expression representing a runtime computation
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum ExprDn {
    Const(Value),
    Variable(ValueSrc),
    Concat(Vec<ConcatElem<ExprDn>>),
    Index(Box<ExprDn>, u32),
    Slice(Box<ExprDn>, u32, u32),
    Unary(Box<ExprDn>, UnaryOp),
}

impl ExprDn {
    pub fn invalid() -> ExprDn {
        ExprDn::Variable(ValueSrc(u32::MAX.into(), u32::MAX))
    }

    /// Down-evaluate the expression with variables from the given value function.
    pub fn eval(&self, state: &dyn Fn(ValueSrc)->Value) -> Value {
        match *self {
            ExprDn::Variable(id) => state(id),
            ExprDn::Const(ref v) => v.clone(),
            ExprDn::Concat(ref components) => {
                let mut result = vec![];
                for component in components {
                    match component {
                        ConcatElem::Elem(e) => result.push(e.eval(state)),
                        ConcatElem::Slice(e, w) => {
                            match e.eval(state) {
                                Value::Vector(v) => result.extend(v.into_iter()),
                                other => panic!("Slice splat expected vector of length {}, found {}", w, other)
                            }
                        }
                    }
                }
                Value::Vector(result)
            },

            ExprDn::Index(ref e, i) => {
                match e.eval(state) {
                    Value::Vector(v) if v.len() >= i as usize => v[i as usize].clone(),
                    other => panic!("Slice expected vector of at least length {i}, found {}", other)
                }
            }

            ExprDn::Slice(ref e, a, b) => {
                match e.eval(state) {
                    Value::Vector(v) if v.len() >= b as usize => Value::Vector(v[a as usize..b as usize].to_vec()),
                    other => panic!("Slice expected vector of at least length {b}, found {}", other)
                }
            }

            ExprDn::Unary(ref e, ref op) => {
                op.eval(e.eval(state))
            }
        }
    }
}

/// Element of Expr::Concat
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum ConcatElem<E> {
    /// An `Expr` included directly in a concatenation (`[a]`)
    Elem(E),

    /// An `Expr` for a `Vector` value whose elements are included in a concatenation (`[8:a]`)
    Slice(E, u32),
}

impl<E> ConcatElem<E> {
    pub fn map_elem<T>(&self, f: impl FnOnce(&E) -> Option<T>) -> Option<ConcatElem<T>> {
        match *self {
            ConcatElem::Elem(ref e) => Some(ConcatElem::Elem(f(e)?)),
            ConcatElem::Slice(ref e, w) => Some(ConcatElem::Slice(f(e)?, w)),
        }
    }

    pub fn map_elem_owned<T>(self, f: impl FnOnce(E) -> T) -> ConcatElem<T> {
        match self {
            ConcatElem::Elem(e) => ConcatElem::Elem(f(e)),
            ConcatElem::Slice(e, w) => ConcatElem::Slice(f(e), w),
        }
    }

    pub fn elem_count(&self) -> u32 {
        match *self {
            ConcatElem::Elem(..) => 1,
            ConcatElem::Slice(_, s) => s,
        }
    }
}
