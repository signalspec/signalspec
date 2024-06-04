use std::sync::Arc;

use crate::diagnostic::ErrorReported;
use crate::entitymap::{entity_key, EntityMap};
use crate::runtime::PrimitiveProcess;
use crate::Dir;
use super::{ExprDn, MatchSet, Shape, Predicate, ValueSrcId, ShapeMode};

entity_key!(pub StepId);

#[derive(Debug)]
pub struct AltDnArm {
    pub vals: Vec<Predicate>,
    pub body: StepId,
}

#[derive(Debug)]
pub struct AltUpArm {
    pub vals: Vec<ExprDn>,
    pub body: StepId,
}

#[derive(Debug)]

pub enum Step {
    Fail,
    Accept,
    Pass,
    Stack { lo: StepId, shape: Shape, hi: StepId },
    Send { dir: Dir, variant: usize, msg: Vec<ExprDn> },
    Receive { dir: Dir, variant: usize, msg: Vec<(Predicate, ValueSrcId)> },
    Primitive(Arc<dyn PrimitiveProcess + 'static>),
    Seq(StepId, StepId),
    RepeatDn {
        count: ExprDn,
        inner: StepId,
    },
    RepeatUp {
        min: i64,
        max: Option<i64>,
        inner: StepId,
        count: ValueSrcId,
    },
    AltDn(Vec<ExprDn>, Vec<AltDnArm>),
    AltUp(Vec<AltUpArm>, Vec<ValueSrcId>),
}

pub(crate) struct StepBuilder {
    pub steps: EntityMap<StepId, Step>
}

impl StepBuilder {
    pub(crate) const FAIL: StepId = StepId(0);
    pub(crate) const ACCEPT: StepId = StepId(1);

    pub(crate) fn new() -> Self {
        let mut steps = EntityMap::new();
        assert_eq!(steps.push(Step::Fail), Self::FAIL);
        assert_eq!(steps.push(Step::Accept), Self::ACCEPT);
        Self { steps }
    }
    
    pub(crate) fn invalid(&self, _r: ErrorReported) -> StepId {
        Self::FAIL
    }
    
    pub(crate) fn accepting(&self) -> StepId {
        Self::ACCEPT
    }
    
    pub(crate) fn seq(&mut self, first: StepId, second: StepId) -> StepId {
        match (first, second) {
            (Self::ACCEPT, _) => return second,
            (_, Self::ACCEPT) => return first,
            (Self::FAIL, _) => return Self::FAIL,
            _ => {}
        }

        if let Step::Seq(a, b) = self.steps[first] {
            let s1 = self.seq(b, second);
            return self.seq(a, s1)
        }

        self.steps.push(Step::Seq(first, second))
    }

    pub(crate) fn seq_from<I>(&mut self, i: I) -> StepId where I: IntoIterator<Item = StepId>, I::IntoIter: DoubleEndedIterator {
        i.into_iter().rev()
            .reduce(|s2, s1| self.seq(s1, s2))
            .unwrap_or(self.accepting())
    }
    
    pub(crate) fn receive(&mut self, dir: Dir, variant: usize, msg: Vec<(Predicate, ValueSrcId)>) -> StepId {
        self.steps.push(Step::Receive { dir, variant, msg })
    }
    
    pub(crate) fn send(&mut self, dir: Dir, variant: usize, msg: Vec<ExprDn>) -> StepId {
        self.steps.push(Step::Send { dir, variant, msg })
    }
    
    pub(crate) fn pass(&mut self) -> StepId {
        self.steps.push(Step::Pass)
    }
    
    pub(crate) fn stack(&mut self, lo: StepId, shape: Shape, hi: StepId) -> StepId {
        match (&self.steps[lo], &self.steps[hi]) {
            (Step::Fail, _) | (_, Step::Fail) => Self::FAIL,
            (Step::Pass, _) => hi,
            (_, Step::Pass) => lo,
            (Step::Seq(a, b), _) if matches!(self.steps[*a], Step::Pass) => {
                self.seq(hi, *b)
            }
            _ => self.steps.push(Step::Stack { lo, shape, hi })
        }
    }
}

pub fn write_tree(f: &mut dyn std::fmt::Write, indent: u32, steps: &EntityMap<StepId, Step>, step: StepId) -> Result<(), std::fmt::Error> {
    let i: String = " ".repeat(indent as usize);
    match steps[step] {
        Step::Fail => writeln!(f, "{}Fail", i)?,
        Step::Accept => writeln!(f, "{}Accept", i)?,
        Step::Pass => writeln!(f, "{}Pass", i)?,
        Step::Stack{ lo, hi, ..} => {
            writeln!(f, "{}Stack:", i)?;
            write_tree(f, indent+2, steps, lo)?;
            write_tree(f, indent+2, steps, hi)?;
        }
        Step::Primitive(_) => {
            writeln!(f, "{}Primitive", i)?
        }
        Step::Send { dir, variant, ref msg } => {
            writeln!(f, "{}Send: {:?} {:?} {:?}", i, dir, variant, msg)?;
        }
        Step::Receive { dir, variant, ref msg } => {
            writeln!(f, "{}Receive: {:?} {:?} {:?}", i, dir, variant, msg)?;
        }
        Step::Seq(s1, s2) => {
            writeln!(f, "{}Seq", i)?;
            write_tree(f, indent+1, steps, s1)?;
            write_tree(f, indent+1, steps, s2)?;
        }
        Step::RepeatDn { ref count, inner } => {
            writeln!(f, "{}Repeat[Dn]: {:?}", i, count)?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::RepeatUp { min, max, inner, count} => {
            writeln!(f, "{i}Repeat[Up]: {min}..={max:?} => {count:?}")?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::AltDn(ref scrutinee, ref arms) => {
            writeln!(f, "{}Alt #dn ({scrutinee:?}):", i)?;
            for arm in arms {
                writeln!(f, "{} {:?} =>", i, arm.vals)?;
                write_tree(f, indent + 2, steps, arm.body)?;
            }
        }
        Step::AltUp(ref arms, ref scrutinee) => {
            writeln!(f, "{}Alt #up ({scrutinee:?}):", i)?;
            for arm in arms {
                writeln!(f, "{} {:?} =>", i, arm.vals)?;
                write_tree(f, indent + 2, steps, arm.body)?;
            }
        }
    }
    Ok(())
}

#[derive(Debug)]
pub struct StepInfo {
    pub(crate) nullable: bool,
    pub(crate) first: MatchSet,
    pub(crate) followlast: Option<MatchSet>,
}

pub fn analyze_unambiguous(steps: &EntityMap<StepId, Step>) -> EntityMap<StepId, StepInfo> {
    let mut res = EntityMap::with_capacity(steps.len());

    for (_, step) in steps {
        let get = |id: StepId| -> &StepInfo {&res[id]};

        let info = match *step {
            Step::Fail => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: false,
                }
            }
            Step::Accept => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: true,
                }
            }
            Step::Pass => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: false,
                }
            }
            Step::Stack { .. } => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: false,
                }
            }

            Step::Send { dir, variant, ref msg } => {
                StepInfo {
                    first: MatchSet::send(dir, variant, msg.clone()),
                    followlast: None,
                    nullable: false,
                }
            }

            Step::Receive { dir, variant, ref msg } => {
                StepInfo {
                    first: MatchSet::receive(dir, variant, msg.iter().map(|(p, _)| p.clone()).collect()),
                    followlast: None,
                    nullable: false,
                }
            }

            Step::Primitive(_) => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: false,
                }
            },
            Step::Seq(s1, s2) => {
                let i1 = get(s1);
                let i2 = get(s2);

                StepInfo {
                    first: if i1.nullable {
                        MatchSet::merge_first([&i1.first, &i2.first].into_iter())
                    } else {
                        i1.first.clone()
                    },
                    followlast: i2.followlast.clone(),
                    nullable: i1.nullable && i2.nullable,
                }
            },
            Step::RepeatDn { inner, .. } => {
                let inner = get(inner);

                MatchSet::check_compatible(&inner.followlast, &inner.first);

                StepInfo {
                    first: MatchSet::proc(),
                    followlast: inner.followlast.clone(),
                    nullable: inner.nullable,
                }
            },
            Step::RepeatUp { min, inner, .. } => {
                let inner = get(inner);

                MatchSet::check_compatible(&inner.followlast, &inner.first);

                StepInfo {
                    first: inner.first.clone(),
                    followlast: Some(inner.first.clone()),
                    nullable: inner.nullable || min == 0,
                }
            },
            Step::AltDn(_, ref opts) => {
                let nullable = opts.iter().any(|arm| get(arm.body).nullable);
                let followlast = MatchSet::merge_followlast(opts.iter().map(|x| &get(x.body).followlast));

                StepInfo {
                    first: MatchSet::proc(),
                    followlast,
                    nullable,
                }
            },
            Step::AltUp(ref opts, _) => {
                let first = MatchSet::merge_first(opts.iter().map(|x| &get(x.body).first));
                let followlast = MatchSet::merge_followlast(opts.iter().map(|x| &get(x.body).followlast));
                let nullable = opts.iter().all(|x| get(x.body).nullable);

                StepInfo { first, nullable, followlast }
            },
        };
        res.push(info);
    }

    res
}
