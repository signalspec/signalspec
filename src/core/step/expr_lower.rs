use crate::{core::{op::ConcatElem, resolve::{expr::{Pattern, VarId}, ExprKind}, step::{ExprCtx, ExprDnId, Predicate}}, entitymap::EntityMap};

pub struct ExprLower {
    vars: EntityMap<VarId, VarState>,
}

struct VarState {
    dn: Option<ExprDnId>,
    up: Option<ExprDnId>,
    predicate: Option<Predicate>,
}

impl ExprLower {
    pub fn new(vars: &EntityMap<VarId, ()>) -> Self {
        ExprLower {
            vars: EntityMap::from_iter(vars.iter().map(|_| VarState { up: None, dn: None, predicate: None }))
        }
    }

    pub fn set(&mut self, var: VarId, dn: Option<ExprDnId>, predicate: Option<Predicate>) {
        self.vars[var] = VarState { dn, up: None, predicate };
    }

    pub fn take_up(&mut self, var: VarId) -> Option<ExprDnId> {
        self.vars[var].up.take()
    }

    /// Define `expr` to equal `v` when entering its scope.
    ///
    /// The returned predicate must be tested against `v` before entering the scope.
    pub fn define_dn(&mut self, ecx: &mut ExprCtx, pat: &Pattern, v: ExprDnId) -> Predicate {
        match *pat {
            Pattern::Ignored => Predicate::Any,
            Pattern::Const(ref value) => Predicate::from_value(value),
            Pattern::Var(var_id) => {
                self.vars[var_id] = VarState { dn: Some(v), up: None, predicate: None };
                Predicate::Any
            }
            Pattern::Concat(ref concat_elems) => {
                Predicate::vector(ConcatElem::enumerate(concat_elems.iter()).map(|(offset, elem)| {
                    match *elem {
                        ConcatElem::Elem(ref e) => {
                            let v2 = ecx.index(v, offset);
                            ConcatElem::Elem(self.define_dn(ecx, e, v2))
                        }
                        ConcatElem::Slice(ref e, width) => {
                            let v2 = ecx.slice(v, offset, offset + width);
                            ConcatElem::Slice(self.define_dn(ecx, e, v2), width)
                        }
                    }
                }))
            }
        }
    }

    pub fn define_up(&mut self, ecx: &mut ExprCtx, pat: &Pattern, predicate: Predicate) {
        match *pat {
            Pattern::Ignored | Pattern::Const(..) => {},
            Pattern::Var(var_id) => {
                self.vars[var_id] = VarState { dn: None, up: None, predicate: Some(predicate) };
            }
            Pattern::Concat(ref concat_elems) => {
                for (offset, elem) in ConcatElem::enumerate(concat_elems.iter()) {
                    match *elem {
                        ConcatElem::Elem(ref e) => {
                            self.define_up(ecx, e, predicate.index(offset));
                        }
                        ConcatElem::Slice(ref e, width) => {
                            self.define_up(ecx, e, predicate.slice(offset, width));
                        }
                    }
                }
            }
        }
    }

    /// Get the up-evaluated value of `pat` when exiting its scope.
    pub fn consume_up(&mut self, ecx: &mut ExprCtx, pat: &Pattern) -> Option<ExprDnId> {
        match *pat {
            Pattern::Ignored => None,
            Pattern::Const(ref v) => Some(ecx.constant(v.clone())),
            Pattern::Var(var_id) => {
                self.vars[var_id].up.take()
            }
            Pattern::Concat(ref concat_elems) => {
                let parts = concat_elems.iter()
                    .map(|l| l.map_elem_opt(|e| self.consume_up(ecx, e)))
                    .collect::<Option<Vec<_>>>()?;
                Some(ecx.concat(parts.into_boxed_slice()))
            }
        }
    }

    pub fn use_dn(&mut self, ecx: &mut ExprCtx, expr: &ExprKind) -> Option<ExprDnId> {
        match *expr {
            ExprKind::Ignored | ExprKind::Range(..) | ExprKind::Enum(..) => None,
            ExprKind::Var(var) => self.vars[var].dn,
            ExprKind::Const(ref v) => Some(ecx.constant(v.clone())),
            ExprKind::Flip(ref d, _) => self.use_dn(ecx, d),
            ExprKind::Concat(ref c) => {
                let parts = c.iter()
                    .map(|l| l.map_elem_opt(|e| self.use_dn(ecx, e)))
                    .collect::<Option<Vec<_>>>()?;
                Some(ecx.concat(parts.into_boxed_slice()))
            },
            ExprKind::Unary(ref e, ref op) => {
                let e = self.use_dn(ecx, e)?;
                Some(ecx.unary(e, op.clone()))
            }
        }
    }

    pub fn use_up(&mut self, ecx: &mut ExprCtx, expr: &ExprKind, v: ExprDnId) -> bool {
        match *expr {
            ExprKind::Ignored | ExprKind::Const(_)
             | ExprKind::Range(_, _) | ExprKind::Enum(..) => false,
            ExprKind::Var(id) => {
                self.vars[id].up = Some(v);
                true
            }
            ExprKind::Flip(_, ref up) => self.use_up(ecx, up, v),
            ExprKind::Concat(ref concat) => {
                let mut used = false;
                for (offset, c) in ConcatElem::enumerate(concat.iter()) {
                    match c {
                        ConcatElem::Elem(e) => {
                            let v2 = ecx.index(v, offset);
                            used |= self.use_up(ecx, e, v2);
                        },
                        ConcatElem::Slice(e, width) => {
                            let v2 = ecx.slice(v, offset, offset + width);
                            used |= self.use_up(ecx, e, v2);
                        },
                    };
                }
                used
            }
            ExprKind::Unary(ref e, ref op) => {
                let v2 = ecx.unary(v, op.invert());
                self.use_up(ecx, e, v2)
            }
        }
    }

    pub fn predicate(&self, expr: &ExprKind) -> Option<Predicate> {
        match *expr {
            ExprKind::Ignored => Some(Predicate::Any),
            ExprKind::Var(var_id) => self.vars[var_id].predicate.clone(),
            ExprKind::Flip(_, ref up) => self.predicate(up),
            ExprKind::Const(ref v) => Some(Predicate::from_value(v)),
            ExprKind::Range(lo, Some(hi)) => Some(Predicate::Range(lo, hi)),
            ExprKind::Range(lo, None) => Some(Predicate::AtLeast(lo)),
            ExprKind::Enum(ref set) => {
                Some(Predicate::AnyOf(set.iter().cloned().collect()))
            },
            ExprKind::Concat(ref parts) => {
                Some(Predicate::vector(parts.iter().map(|c| c.map_elem(|e| self.predicate(e).unwrap()))))
            }
            ExprKind::Unary(ref e, _) => {
                match self.predicate(e) {
                    Some(Predicate::Any) => Some(Predicate::Any),
                    _ => None
                }
            },
        }
    }
}
