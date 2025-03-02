use std::collections::{BTreeMap, BTreeSet};

use indexmap::indexset;

use super::{expr_dn::ExprDnId, step::{ProcId, StepBuilder}, ChannelId, Dir, Predicate, Step, StepId, ValueSrcId};

#[derive(Debug)]
pub(crate) enum Derivatives {
    End,

    Send {
        chan: ChannelId,
        variant: usize,
        dn: Vec<ExprDnId>,
        next: StepId,
    },

    Receive {
        chan: ChannelId,
        arms: Vec<ReceiveArm>,
        other: StepId,
    },

    Process {
        id: ProcId,
        next: StepId,
        err: StepId,
    },

    Assign {
        var: ValueSrcId,
        val: ExprDnId,
        next: StepId,
    },

    Switch {
        src: Vec<ExprDnId>,
        arms: Vec<SwitchArm>,
        other: StepId,
    },

    Select {
        branches: Vec<Derivatives>,
        end: Vec<ChannelId>,
    },
}

#[derive(Debug)]
pub(crate) struct ReceiveArm {
    pub variant: usize,
    pub var: ValueSrcId,
    pub predicates: Vec<Predicate>,
    pub next: StepId,
}

#[derive(Debug)]
pub(crate) struct SwitchArm {
    pub var: Option<ValueSrcId>,
    pub predicates: Vec<Predicate>,
    pub next: StepId,
}

impl Derivatives {
    fn follow(&self, f: &mut impl FnMut(StepId)) {
        match *self {
            Derivatives::End => {},
            Derivatives::Process { next, err, ..} => { f(next); f(err) },
            Derivatives::Send { next, .. } => f(next),
            Derivatives::Assign { next , .. } => f(next),
            Derivatives::Receive { ref arms, other, .. } => {
                for arm in arms {
                    f(arm.next);
                }
                f(other)
            },
            Derivatives::Switch { ref arms, other, .. } => {
                for arm in arms {
                    f(arm.next);
                }
                f(other)
            },
            Derivatives::Select { ref branches, .. } => {
                for b in branches {
                    b.follow(f)
                }
            },
        }
    }

    fn follow_mut(&mut self, f: &mut impl FnMut(&mut StepId)) {
        match self {
            Derivatives::End => {},
            Derivatives::Process { next, err, ..} => { f(next); f(err) },
            Derivatives::Send { next, .. } => f(next),
            Derivatives::Assign { next , .. } => f(next),
            Derivatives::Receive { arms, other, .. } => {
                for arm in arms {
                    f(&mut arm.next);
                }
                f(other)
            },
            Derivatives::Switch { arms, other, .. } => {
                for arm in arms {
                    f(&mut arm.next);
                }
                f(other)
            },
            Derivatives::Select { branches, .. } => {
                for b in branches {
                    b.follow_mut(f)
                }
            },
        }
    }

    fn map_follow(mut self, mut f: impl FnMut(StepId) -> StepId) -> Derivatives {
        self.follow_mut(&mut |m| *m = f(*m));
        self
    }
}

impl StepBuilder {
    fn nullable(&self, step: StepId) -> bool {
        match self.steps[step] {
            Step::Fail => false,
            Step::Accept => true,
            Step::Pass => false,
            Step::Stack { lo, hi, .. } => self.nullable(lo) && self.nullable(hi),
            Step::Send { .. } => false,
            Step::Receive { .. } => false,
            Step::Process(_) => false,
            Step::Seq(a, b) => self.nullable(a) && self.nullable(b),
            Step::Assign(_, _) => false,
            Step::Guard(_, _) => false,
            Step::Repeat(_, accepting) => accepting,
            Step::Alt(ref arms) => arms.iter().any(|&s| self.nullable(s)),
        }
    }

    fn derivative(&mut self, step: StepId) -> Derivatives {
        match self.steps[step] {
            Step::Fail | Step::Accept => Derivatives::End,
            Step::Pass => todo!(),
            Step::Stack { lo, conn, hi } => {
                let control_dir = self.connections[conn].mode.control_dir();
                let dlo = self.derivative(lo);
                let dhi = self.derivative(hi);

                match (dlo, dhi) {
                    // paired: transfer
                    (
                        Derivatives::Send { chan: chan1, variant, dn, next },
                        Derivatives::Receive { chan: chan2, arms, other }
                    ) if chan1 == conn.up() && chan2 == conn.up() => {
                        let arms: Vec<SwitchArm> = arms.into_iter()
                            .filter(|arm| arm.variant == variant)
                            .map(|arm| {
                                SwitchArm {
                                    var: Some(arm.var),
                                    predicates: arm.predicates,
                                    next: self.stack(next, conn, arm.next),
                                }
                            })
                            .collect();

                        let other = self.stack(lo, conn, other);

                        if arms.is_empty() {
                            self.derivative(other)
                        } else {
                            Derivatives::Switch { src: dn, arms, other }
                        }
                    },

                    (
                        Derivatives::Receive { chan: chan1, arms, other },
                        Derivatives::Send { chan: chan2, variant, dn, next }
                    ) if chan1 == conn.dn() && chan2 == conn.dn() => {
                        let arms: Vec<SwitchArm> = arms.into_iter()
                            .filter(|arm| arm.variant == variant)
                            .map(|arm| {
                                SwitchArm {
                                    var: Some(arm.var),
                                    predicates: arm.predicates,
                                    next: self.stack(arm.next, conn, next),
                                }
                            })
                            .collect();

                        let other = self.stack(other, conn, hi);

                        if arms.is_empty() {
                            self.derivative(other)
                        } else {
                            Derivatives::Switch { src: dn, arms, other }
                        }
                    },

                    (dlo @ (Derivatives::Process { .. } | Derivatives::Select { .. }), Derivatives::End) => {
                        if control_dir == Some(Dir::Dn) {
                            let a = dlo.map_follow(|nlo| self.stack(nlo, conn, hi));
                            self.select_end(a, conn.dn())
                        } else {
                            Derivatives::End
                        }
                    }

                    (Derivatives::End, dhi @ (Derivatives::Process { .. } | Derivatives::Select { .. })) => {
                        if control_dir == Some(Dir::Up)  {
                            let b = dhi.map_follow(|nhi| self.stack(lo, conn, nhi));
                            self.select_end(b, conn.up())
                        } else {
                            Derivatives::End
                        }
                    }

                    (Derivatives::End, _) if self.nullable(hi) => Derivatives::End,
                    (_, Derivatives::End) if self.nullable(lo) => Derivatives::End,

                    (dlo @ (Derivatives::Process { .. } | Derivatives::Select { .. }), dhi) |
                    (dlo, dhi @ (Derivatives::Process { .. } | Derivatives::Select { .. })) => {
                        let a = dlo.map_follow(|nlo| self.stack(nlo, conn, hi));
                        let b = dhi.map_follow(|nhi| self.stack(lo, conn, nhi));
                        self.select(a, b)
                    }

                    // take immediate transitions from hi
                    (_, dhi @ (Derivatives::Assign { .. } | Derivatives::Switch { .. })) => {
                        dhi.map_follow(|hi| self.stack(lo, conn, hi))
                    }

                    // lo is blocked, advance hi
                    (Derivatives::Send { chan, .. }, dhi) if chan == conn.up() => dhi.map_follow(|hi| self.stack(lo, conn, hi)),
                    (Derivatives::Receive { chan, .. }, dhi) if chan == conn.dn() => dhi.map_follow(|hi| self.stack(lo, conn, hi)),

                    // otherwise, advance lo
                    (dlo, _) => dlo.map_follow(|lo| self.stack(lo, conn, hi))
                }
            }

            Step::Send { chan, variant, ref msg } => {
                Derivatives::Send { chan, variant, dn: msg.clone(), next: StepId::ACCEPT }
            }
            Step::Receive { chan, variant, var, ref msg } => {
                let arms = vec![ReceiveArm { variant, predicates: msg.clone(), var, next: StepId::ACCEPT }];
                Derivatives::Receive { chan, arms, other: StepId::FAIL }
            }
            Step::Process(id) => {
                Derivatives::Process { id, next: StepId::ACCEPT, err: StepId::FAIL }
            }
            Step::Seq(a, b) => {
                let da = self.derivative(a);
                let d = self.then(da, b);
                if !self.nullable(a) {
                    d
                } else {
                    self.or(d, b)
                }
            }
            Step::Assign(var, ref val) => {
                Derivatives::Assign { var, val: val.clone(), next: StepId::ACCEPT }
            }
            Step::Guard(ref val, ref predicate) => {
                let arms = vec![SwitchArm { predicates: vec![predicate.clone()], var: None, next: StepId::ACCEPT }];
                Derivatives::Switch { src: vec![val.clone()], arms, other: StepId::FAIL }
            }
            Step::Repeat(inner, _) => {
                let d = self.derivative(inner);
                let next = self.repeat(inner, true);
                self.then(d, next)
            }
            Step::Alt(ref arms) => {
                let mut d = Derivatives::End;
                for &s in arms.clone().iter() {
                    d = self.or(d, s);
                }
                d
            }
        }
    }

    fn then(&mut self, a: Derivatives, b: StepId) -> Derivatives {
        a.map_follow(|s| self.seq(s, b))
    }

    fn or(&mut self, da: Derivatives, b: StepId) -> Derivatives {
        let db = self.derivative(b);

        match (da, db) {
            (Derivatives::End, x) | (x, Derivatives::End) => x,

            (
                Derivatives::Send { chan: chan1, variant: variant1, dn: dn1, next: next1 },
                Derivatives::Send { chan: chan2, variant: variant2, dn: dn2, next: next2 }
            ) => {
                if chan1 == chan2 && variant1 == variant2 && dn1 == dn2 {
                    Derivatives::Send { chan: chan1, variant: variant1, dn: dn1, next: self.alt(vec![next1, next2]) }
                } else {
                    todo!("conflict")
                }
            }

            (
                Derivatives::Assign { var: var1, val: val1, next: next1 },
                Derivatives::Assign { var: var2, val: val2, next: next2 }
            ) => {
                if var1 == var2 && val1 == val2 {
                    Derivatives::Assign { var: var1, val: val1, next: self.alt(vec![next1, next2]) }
                } else {
                    todo!("ambiguous")
                }
            }

            (
                Derivatives::Receive { chan: chan1, arms: mut arms1, other: other1 },
                Derivatives::Receive { chan: chan2, arms: arms2, other: other2 }
            ) if chan1 == chan2 => {
                // TODO: Merge overlapping arms
                arms1.extend(arms2.into_iter());
                Derivatives::Receive { chan: chan1, arms: arms1, other: self.alt(vec![other1, other2]) }
            }

            // TODO: disallow for symmetric alt?
            (Derivatives::Receive { chan, arms, other }, _) => {
                Derivatives::Receive { chan, arms, other: self.alt(vec![other, b]) }
            }

            (
                Derivatives::Switch { src: s1, arms: mut arm1, other: o1 },
                Derivatives::Switch { src: s2, arms: arm2, other: o2 }
            ) if s1 == s2 => {
                arm1.extend(arm2.into_iter());
                Derivatives::Switch { src: s1, arms: arm1, other: self.alt(vec![o1, o2]) }
            }

            (a, b) => todo!("Conflict: {a:?}, {b:?}")
        }
    }

    fn select(&mut self, a: Derivatives, b: Derivatives) -> Derivatives {
        match (a, b) {
            (Derivatives::End, x) | (x, Derivatives::End) => x,

            // Assign and Switch complete immediately, so they can be chosen
            // arbitrarily and the alternatives ignored.
            (i @ (Derivatives::Assign {..} | Derivatives::Switch {..}), _) |
            (_, i @ (Derivatives::Assign {..} | Derivatives::Switch {..})) => i,

            // Merge Selects
            (
                Derivatives::Select { branches: mut b1, end: mut e1 },
                Derivatives::Select { branches: b2, end: e2 }
            ) => {
                b1.extend(b2.into_iter());
                e1.extend(e2.into_iter());
                Derivatives::Select { branches: b1, end: e1 }
            }

            // If one side is already a Select, flatten into it
            (Derivatives::Select { mut branches, end }, other) |
            (other, Derivatives::Select { mut branches, end }) => {
                branches.push(other);
                Derivatives::Select { branches, end }
            }

            (a, b) => {
                Derivatives::Select { branches: vec![a, b], end: vec![] }
            }
        }
    }

    pub(crate) fn select_end(&self, inner: Derivatives, chan: ChannelId) -> super::Derivatives {
        if let Derivatives::Select { branches, mut end } = inner {
            end.push(chan);
            Derivatives::Select { branches, end }
        } else {
            Derivatives::Select { branches: vec![inner], end: vec![chan] }
        }
    }

    pub(crate) fn fsm(&mut self, start: StepId) -> (BTreeMap<StepId, Derivatives>, BTreeSet<StepId>) {
        let mut queue = indexset![start];
        let mut known = BTreeMap::new();
        let mut accepting = BTreeSet::new();

        while let Some(s) = queue.pop() {
            assert!(!known.contains_key(&s));
            let d = self.derivative(s);

            if log_enabled!(log::Level::Info) {
                let mut buf = String::new();
                self.write_tree(&mut buf, 4, s).unwrap();
                log::info!("Derivatives of {s:?}: {d:?}\n{buf}");
            }

            d.follow(&mut |f| {
                if f != s && !known.contains_key(&f) {
                    queue.insert(f);
                }
            });

            known.insert(s, d);

            if self.nullable(s) {
                accepting.insert(s);
            }
        }

        log::info!("Accepting states: {accepting:?}");

        (known, accepting)
    }
}