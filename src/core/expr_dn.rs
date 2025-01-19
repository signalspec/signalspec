use std::fmt::Display;

use crate::entitymap::{entity_key, EntityIntern, EntityMap};
use super::{ValueSrc, ValueSrcId};
use crate::Value;
use super::op::UnaryOp;

entity_key!(pub ExprDnId);

impl ExprDnId {
    pub const INVALID: ExprDnId = ExprDnId(0);
}

pub struct ExprCtx {
    vars: EntityMap<ValueSrcId, ()>,
    exprs: EntityIntern<ExprDnId, ExprDn>,
}

impl ExprCtx {
    pub fn new() -> ExprCtx {
        let vars = EntityMap::new();
        let mut exprs = EntityIntern::new();
        assert_eq!(exprs.insert(ExprDn::Variable(ValueSrc(u32::MAX.into(), u32::MAX))), ExprDnId::INVALID);
        ExprCtx { vars, exprs }
    }

    pub fn fresh_var(&mut self) -> ValueSrcId {
        self.vars.push(())
    }

    pub fn vars(&self) -> &EntityMap<ValueSrcId, ()> {
        &self.vars
    }

    pub fn get(&self, id: ExprDnId) -> &ExprDn {
        &self.exprs[id]
    }

    pub fn get_const(&self, id: ExprDnId) -> Option<&Value> {
        match &self.exprs[id] {
            ExprDn::Const(value) => Some(value),
            _ => None,
        }
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
        if let Some(v) = self.get_const(e) {
            self.constant(op.eval(v.clone()))
        } else {
            self.exprs.insert(ExprDn::Unary(e, op))
        }
    }

    pub(crate) fn index(&mut self, v: ExprDnId, i: u32) -> ExprDnId {
        if let Some(v) = self.get_const(v) {
            self.constant(v.index(i))
        } else {
            self.exprs.insert(ExprDn::Index(v, i))
        }
    }

    pub(crate) fn slice(&mut self, v: ExprDnId, i1: u32, i2: u32) -> ExprDnId {
        if let Some(v) = self.get_const(v) {
            self.constant(v.slice(i1, i2))
        } else {
            self.exprs.insert(ExprDn::Slice(v, i1, i2))
        }
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
                self.eval(e, state).index(i)
            }

            ExprDn::Slice(e, a, b) => {
                self.eval(e, state).slice(a, b)
            }

            ExprDn::Unary(e, ref op) => {
                op.eval(self.eval(e, state))
            }
        }
    }

    pub fn substitute(&mut self, e: ExprDnId, var: ValueSrcId, v: &mut impl FnMut(&mut ExprCtx, u32) -> ExprDnId) -> ExprDnId {
        match self.exprs[e] {
            ExprDn::Const(_) => e,
            ExprDn::Variable(ValueSrc(var2, i)) if var == var2 => v(self, i),
            ExprDn::Variable(_) => e,
            ExprDn::Concat(ref parts) => {
                let mut parts = parts.clone();
                for part in &mut parts {
                    *part = part.map_elem(|e2| self.substitute(*e2, var, v));
                }
                self.concat(parts)
            },
            ExprDn::Index(e2, i) => {
                let e2s = self.substitute(e2, var, v);
                if e2 != e2s { self.index(e2s, i) } else { e }
            }
            ExprDn::Slice(e2, i1, i2) => {
                let e2s = self.substitute(e2, var, v);
                if e2 != e2s { self.slice(e2s, i1, i2) } else { e }
            }
            ExprDn::Unary(e2, ref op) => {
                let op = op.clone();
                let e2s = self.substitute(e2, var, v);
                if e2 != e2s { self.unary(e2s, op) } else { e }
            }
        }
    }

    pub fn substitute_var(&mut self, e: ExprDnId, var: ValueSrcId, new_var: ValueSrcId) -> ExprDnId {
        match self.exprs[e] {
            ExprDn::Const(_) => e,
            ExprDn::Variable(ValueSrc(var2, i)) if var == var2 => self.variable(ValueSrc(new_var, i)),
            ExprDn::Variable(_) => e,
            ExprDn::Concat(ref parts) => {
                let mut parts = parts.clone();
                for part in &mut parts {
                    *part = part.map_elem(|e2| self.substitute_var(*e2, var, new_var));
                }
                self.concat(parts)
            },
            ExprDn::Index(e2, i) => {
                let e2s = self.substitute_var(e2, var, new_var);
                if e2 != e2s { self.index(e2s, i) } else { e }
            }
            ExprDn::Slice(e2, i1, i2) => {
                let e2s = self.substitute_var(e2, var, new_var);
                if e2 != e2s { self.slice(e2s, i1, i2) } else { e }
            }
            ExprDn::Unary(e2, ref op) => {
                let op = op.clone();
                let e2s = self.substitute_var(e2, var, new_var);
                if e2 != e2s { self.unary(e2s, op) } else { e }
            }
        }
    }

    pub fn format(&self, id: ExprDnId) -> ExprDnFormat<'_> {
        ExprDnFormat { ecx: self, id }
    }
}

pub struct ExprDnFormat<'a> {
    id: ExprDnId,
    ecx: &'a ExprCtx,
}

impl Display for ExprDnFormat<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self.ecx.get(self.id) {
            ExprDn::Const(ref value) => write!(f, "{}", value),
            ExprDn::Variable(id) => write!(f, "${}${}", id.0.0, id.1),
            ExprDn::Concat(ref parts) => {
                write!(f, "[")?;
                for p in parts {
                    match *p {
                        ConcatElem::Elem(e) => write!(f, "{}", self.ecx.format(e))?,
                        ConcatElem::Slice(e, w) => write!(f, "{w}:{}", self.ecx.format(e))?,
                    }
                    write!(f, ", ")?;
                }
                write!(f, "]")
            },
            ExprDn::Index(e, i) => {
                write!(f, "{}[{i}]", self.ecx.format(e))
            },
            ExprDn::Slice(e, i1, i2) => {
                write!(f, "{}[{i1}..{i2}]", self.ecx.format(e))
            }
            ExprDn::Unary(e, ref op) => {
                write!(f, "{} {op:?}", self.ecx.format(e))
            },
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
    pub fn map_elem<T>(&self, f: impl FnOnce(&E) -> T) -> ConcatElem<T> {
        match *self {
            ConcatElem::Elem(ref e) => ConcatElem::Elem(f(e)),
            ConcatElem::Slice(ref e, w) => ConcatElem::Slice(f(e), w),
        }
    }

    pub fn map_elem_opt<T>(&self, f: impl FnOnce(&E) -> Option<T>) -> Option<ConcatElem<T>> {
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
