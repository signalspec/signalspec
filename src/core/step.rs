use std::io::{Write, Result as IoResult};
use std::sync::Arc;

use crate::diagnostic::ErrorReported;
use crate::entitymap::{entity_key, EntityMap};
use crate::{PrimitiveProcess};
use super::{ExprDn, MatchSet, Shape, Dir, Predicate, ValueSrcId};

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
    Invalid(ErrorReported),
    Stack { lo: StepId, shape: Shape, hi: StepId },
    Token { variant: usize, dn: Vec<ExprDn>, up: Vec<(Predicate, ValueSrcId)> },
    TokenTop { top_dir: Dir, variant: usize, dn: Vec<(Predicate, ValueSrcId)>, up: Vec<ExprDn>, inner: StepId },
    Primitive(Arc<dyn PrimitiveProcess + 'static>),
    Seq(Vec<StepId>),
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
    Foreach {
        /// Number of iterations = length of input and output vectors
        iters: u32,

        /// Evaluated at entry to produce vectors that are iterated in the loop
        dn: Vec<(ExprDn, ValueSrcId)>,

        /// Evaluated after each iteration to produce an element for each output vector
        up: Vec<(ExprDn, ValueSrcId)>,

        inner: StepId
    },
    AltDn(Vec<ExprDn>, Vec<AltDnArm>),
    AltUp(Vec<AltUpArm>, Vec<ValueSrcId>),
}

pub fn write_tree(f: &mut dyn Write, indent: u32, steps: &[Step], step: StepId) -> IoResult<()> {
    let i: String = " ".repeat(indent as usize);
    match steps[step.0 as usize] {
        Step::Invalid(_) => writeln!(f, "{}Invalid", i)?,
        Step::Stack{ lo, hi, ..} => {
            writeln!(f, "{}Stack:", i)?;
            write_tree(f, indent+2, steps, lo)?;
            write_tree(f, indent+2, steps, hi)?;
        }
        Step::Primitive(_) => {
            writeln!(f, "{}Primitive", i)?
        }
        Step::Token { variant, ref dn, ref up } => {
            writeln!(f, "{}Token: {:?} {:?} {:?}", i, variant, dn, up)?
        }
        Step::TokenTop { variant, ref dn, ref up, inner, .. } => {
            writeln!(f, "{}Up: {:?} {:?} {:?}", i, variant, dn, up)?;
            write_tree(f, indent+1, steps, inner)?;
        }
        Step::Seq(ref inner) => {
            writeln!(f, "{}Seq", i)?;
            for &c in inner.iter() {
                write_tree(f, indent+1, steps, c)?;
            }
        }
        Step::RepeatDn { ref count, inner } => {
            writeln!(f, "{}Repeat[Dn]: {:?}", i, count)?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::RepeatUp { min, max, inner, count} => {
            writeln!(f, "{i}Repeat[Up]: {min}..={max:?} => {count:?}")?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::Foreach { iters, dn: ref dn_vecs, up: ref up_elems, inner } => {
            write!(f, "{}For: {} ", i, iters)?;
            for expr in dn_vecs { write!(f, "<={:?}, ", expr)?; }
            for expr in up_elems { write!(f, "=>{:?}, ", expr)?; }
            writeln!(f, "")?;
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
            Step::Invalid(_) => {
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
            },
            Step::Token { variant, ref dn, ref up } => {
                StepInfo {
                    first: MatchSet::lower(variant, dn.clone(), up.iter().map(|(p, _)| p.clone()).collect()),
                    followlast: None,
                    nullable: false,
                }
            },
            Step::TokenTop { top_dir, variant, ref dn, inner, .. } => {
                let inner = get(inner);
                match top_dir {
                    Dir::Up => {
                        StepInfo {
                            first: inner.first.clone(),
                            followlast: inner.followlast.clone(),
                            nullable: inner.nullable,
                        }
                    },
                    Dir::Dn => {
                        StepInfo {
                            first: MatchSet::upper(variant, dn.iter().map(|(p, _)| p.clone()).collect()),
                            followlast: inner.followlast.clone(),
                            nullable: false,
                        }
                    },
                }
            },
            Step::Primitive(_) => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: false,
                }
            },
            Step::Seq(ref steps) if steps.is_empty() => {
                StepInfo {
                    first: MatchSet::proc(),
                    followlast: None,
                    nullable: false,
                }
            }
            Step::Seq(ref steps) => {
                //TODO: take_until https://github.com/rust-itertools/itertools/issues/597
                let nullable_prefix = &steps[0..=steps.iter().position(|&s| !get(s).nullable).unwrap_or(steps.len() - 1)];
                let first = MatchSet::merge_first(nullable_prefix.iter().map(|&s| &get(s).first));

                let nullable = steps.iter().all(|&s| get(s).nullable);
                let followlast = steps.last().and_then(|&l| get(l).followlast.clone());

                let mut prev = &None;
                for &i in steps {
                    let i = get(i);
                    MatchSet::check_compatible(prev, &i.first);
                    prev = &i.followlast;
                }

                StepInfo {
                    first,
                    followlast,
                    nullable,
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
            Step::Foreach { inner, dn: ref dn_vecs, .. } => {
                let inner = get(inner);

                MatchSet::check_compatible(&inner.followlast, &inner.first);

                // If the `for` block is introducing variables, the send from first
                // cannot be lifted out of the block because it relies on those
                // variables being defined. It would conflict with any alternatives anyway
                // since those couldn't use the variable.
                // TODO: is testing vars_dn the right condition or should we more specifically
                // look at whether `first` contains a send with inner variables.
                if dn_vecs.is_empty() {
                    StepInfo {
                        first: inner.first.clone(),
                        followlast: inner.followlast.clone(),
                        nullable: inner.nullable,
                    }
                } else {
                    StepInfo {
                        first: MatchSet::proc(),
                        followlast: inner.followlast.clone(),
                        nullable: inner.nullable,
                    }
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
