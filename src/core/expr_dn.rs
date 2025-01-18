use crate::entitymap::{entity_key, EntityIntern};
use super::ValueSrc;
use crate::Value;
use super::op::UnaryOp;

entity_key!(pub ExprDnId);

impl ExprDnId {
    pub const INVALID: ExprDnId = ExprDnId(0);
}

pub struct ExprCtx {
    exprs: EntityIntern<ExprDnId, ExprDn>,
}

impl ExprCtx {
    pub fn new() -> ExprCtx {
        let mut exprs = EntityIntern::new();
        assert_eq!(exprs.insert(ExprDn::Variable(ValueSrc(u32::MAX.into(), u32::MAX))), ExprDnId::INVALID);
        ExprCtx { exprs }
    }

    pub(crate) fn get(&self, count: ExprDnId) -> &ExprDn {
        &self.exprs[count]
    }

    pub fn invalid(&self) -> ExprDnId {
        ExprDnId::INVALID
    }

    pub fn constant(&mut self, v: Value) -> ExprDnId {
        self.exprs.insert(ExprDn::Const(v))
    }

    pub(crate) fn concat(&mut self, parts: Box<[ConcatElem<ExprDnId>]>) -> ExprDnId {
        self.exprs.insert(ExprDn::Concat(parts))
    }

    pub(crate) fn unary(&mut self, e: ExprDnId, op: UnaryOp) -> ExprDnId {
        self.exprs.insert(ExprDn::Unary(e, op))
    }

    pub(crate) fn index(&mut self, v: ExprDnId, offset: u32) -> ExprDnId {
        self.exprs.insert(ExprDn::Index(v, offset))
    }

    pub(crate) fn slice(&mut self, v: ExprDnId, offset: u32, width: u32) -> ExprDnId {
        self.exprs.insert(ExprDn::Slice(v, offset, width))
    }

    pub(crate) fn variable(&mut self, id: ValueSrc) -> ExprDnId {
        self.exprs.insert(ExprDn::Variable(id))
    }

    /// Down-evaluate the expression with variables from the given value function.
    pub fn eval(&self, e: ExprDnId, state: &dyn Fn(ValueSrc)->Value) -> Value {
        match self.exprs[e] {
            ExprDn::Variable(id) => state(id),
            ExprDn::Const(ref v) => v.clone(),
            ExprDn::Concat(ref components) => {
                let mut result = vec![];
                for component in components {
                    match *component {
                        ConcatElem::Elem(e) => result.push(self.eval(e, state)),
                        ConcatElem::Slice(e, w) => {
                            match self.eval(e, state) {
                                Value::Vector(v) => result.extend(v.into_iter()),
                                other => panic!("Slice splat expected vector of length {}, found {}", w, other)
                            }
                        }
                    }
                }
                Value::Vector(result)
            },

            ExprDn::Index(e, i) => {
                match self.eval(e, state) {
                    Value::Vector(v) if v.len() >= i as usize => v[i as usize].clone(),
                    other => panic!("Slice expected vector of at least length {i}, found {}", other)
                }
            }

            ExprDn::Slice(e, a, b) => {
                match self.eval(e, state) {
                    Value::Vector(v) if v.len() >= b as usize => Value::Vector(v[a as usize..b as usize].to_vec()),
                    other => panic!("Slice expected vector of at least length {b}, found {}", other)
                }
            }

            ExprDn::Unary(e, ref op) => {
                op.eval(self.eval(e, state))
            }
        }
    }
}

/// An expression representing a runtime computation
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum ExprDn {
    Const(Value),
    Variable(ValueSrc),
    Concat(Box<[ConcatElem<ExprDnId>]>),
    Index(ExprDnId, u32),
    Slice(ExprDnId, u32, u32),
    Unary(ExprDnId, UnaryOp),
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
