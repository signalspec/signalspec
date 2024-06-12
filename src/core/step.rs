use std::sync::Arc;

use crate::diagnostic::ErrorReported;
use crate::entitymap::{entity_key, EntityMap};
use crate::runtime::PrimitiveProcess;
use crate::Dir;
use super::{ExprDn, MatchSet, Shape, Predicate, ValueSrcId, ShapeMode};

entity_key!(pub StepId);

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
    Assign(ValueSrcId, ExprDn),
    Guard(ExprDn, Predicate),
    Repeat(StepId, bool),
    Alt(Vec<StepId>),
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

    pub(crate) fn assign(&mut self, dest: ValueSrcId, src: ExprDn) -> StepId {
        self.steps.push(Step::Assign(dest, src))
    }

    pub(crate) fn guard(&mut self, src: ExprDn, pred: Predicate) -> StepId {
        match pred {
            Predicate::Any => self.accepting(),
            pred => self.steps.push(Step::Guard(src, pred))
        }
    }

    pub(crate) fn alt(&mut self, arms: Vec<StepId>) -> StepId {
        self.steps.push(Step::Alt(arms))
    }

    pub(crate) fn repeat(&mut self, inner: StepId, nullable: bool) -> StepId {
        self.steps.push(Step::Repeat(inner, nullable))
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
        Step::Assign(dst, ref src) => {
            writeln!(f, "{}Assign {dst:?} = {src:?}", i)?;
        },
        Step::Guard(ref src, ref pred) => {
            writeln!(f, "{}Guard {src:?} is {pred:?}", i)?;
        }
        Step::Seq(s1, s2) => {
            writeln!(f, "{}Seq", i)?;
            write_tree(f, indent+1, steps, s1)?;
            write_tree(f, indent+1, steps, s2)?;
        }
        Step::Repeat(inner, nullable) => {
            writeln!(f, "{}Repeat accepting={nullable}", i)?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::Alt(ref arms) => {
            writeln!(f, "{}Alt", i)?;
            for &arm in arms {
                write_tree(f, indent + 1, steps, arm)?;
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

            Step::Assign(..) => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: false,
                }
            }

            Step::Guard(ref expr, ref predicate) => {
                StepInfo {
                    first: MatchSet::guard(expr.clone(), predicate.clone()),
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
            Step::Repeat(inner, nullable) => {
                let inner = get(inner);

                MatchSet::check_compatible(&inner.followlast, &inner.first);

                StepInfo {
                    first: inner.first.clone(),
                    followlast: Some(inner.first.clone()),
                    nullable: inner.nullable || nullable,
                }
            },
            Step::Alt(ref opts) => {
                let first = MatchSet::merge_first(opts.iter().map(|&x| &get(x).first));
                let followlast = MatchSet::merge_followlast(opts.iter().map(|&x| &get(x).followlast));
                let nullable = opts.iter().all(|&x| get(x).nullable);

                StepInfo { first, nullable, followlast }
            },
        };
        res.push(info);
    }

    res
}
