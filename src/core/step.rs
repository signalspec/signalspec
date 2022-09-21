use std::io::{Write, Result as IoResult};
use std::sync::Arc;

use crate::entitymap::{entity_key, EntityMap};
use crate::{PrimitiveProcess};
use super::{Expr, ExprDn, MatchSet, Shape, Type, VarId, Dir};

entity_key!(pub StepId);

#[derive(Debug)]
pub enum Step {
    Invalid,
    Stack { lo: StepId, shape: Shape, hi: StepId },
    Token { variant: usize, send: Vec<ExprDn>, receive: Vec<Expr> },
    TokenTop { top_dir: Dir, variant: usize, send: Vec<Expr>, receive: Vec<ExprDn>, inner: StepId },
    Primitive(Arc<dyn PrimitiveProcess + 'static>),
    Seq(Vec<StepId>),
    RepeatDn(ExprDn, StepId),
    RepeatUp(Expr, StepId),
    Foreach { iters: u32, vars_dn: Vec<(VarId, ExprDn)>, vars_up: Vec<(VarId, Expr)>, inner: StepId },
    AltDn(Vec<(Vec<(Expr, ExprDn)>, StepId)>),
    AltUp(Vec<(Vec<(ExprDn, Expr)>, StepId)>),
}

pub fn write_tree(f: &mut dyn Write, indent: u32, steps: &[Step], step: StepId) -> IoResult<()> {
    let i: String = " ".repeat(indent as usize);
    match steps[step.0 as usize] {
        Step::Invalid => writeln!(f, "{}Invalid", i)?,
        Step::Stack{ lo, hi, ..} => {
            writeln!(f, "{}Stack:", i)?;
            write_tree(f, indent+2, steps, lo)?;
            write_tree(f, indent+2, steps, hi)?;
        }
        Step::Primitive(_) => {
            writeln!(f, "{}Primitive", i)?
        }
        Step::Token { variant, ref send, ref receive } => {
            writeln!(f, "{}Token: {:?} {:?} {:?}", i, variant, send, receive)?
        }
        Step::TokenTop { variant, ref send, ref receive, inner, .. } => {
            writeln!(f, "{}Up: {:?} {:?} {:?}", i, variant, send, receive)?;
            write_tree(f, indent+1, steps, inner)?;
        }
        Step::Seq(ref inner) => {
            writeln!(f, "{}Seq", i)?;
            for &c in inner.iter() {
                write_tree(f, indent+1, steps, c)?;
            }
        }
        Step::RepeatDn(ref count, inner) => {
            writeln!(f, "{}Repeat[Dn]: {:?}", i, count)?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::RepeatUp(ref count, inner) => {
            writeln!(f, "{}Repeat[Up]: {:?}", i, count)?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::Foreach { iters, ref vars_dn, ref vars_up, inner } => {
            write!(f, "{}For: {} ", i, iters)?;
            for &(id, ref expr) in vars_dn { write!(f, "{}<={:?}, ", u32::from(id), expr)?; }
            for &(id, ref expr) in vars_up { write!(f, "{}=>{:?}, ", u32::from(id), expr)?; }
            writeln!(f, "")?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::AltDn(ref arms) => {
            writeln!(f, "{}Alt[{:?}]:", i, Dir::Dn)?;
            for &(ref cond, inner) in arms {
                writeln!(f, "{} {:?} =>", i, cond)?;
                write_tree(f, indent + 2, steps, inner)?;
            }
        }
        Step::AltUp(ref arms) => {
            writeln!(f, "{}Alt[{:?}]:", i, Dir::Up)?;
            for &(ref cond, inner) in arms {
                writeln!(f, "{} {:?} =>", i, cond)?;
                write_tree(f, indent + 2, steps, inner)?;
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
            Step::Invalid => {
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
            Step::Token { variant, ref send, ref receive } => {
                StepInfo {
                    first: MatchSet::lower(variant, send.clone(), receive.clone()),
                    followlast: None,
                    nullable: false,
                }
            },
            Step::TokenTop { top_dir, variant, ref send, inner, .. } => {
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
                            first: MatchSet::upper(variant, send.clone()),
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
            Step::RepeatDn(ref _count, inner) => {
                let inner = get(inner);

                MatchSet::check_compatible(&inner.followlast, &inner.first);

                StepInfo {
                    first: MatchSet::proc(),
                    followlast: inner.followlast.clone(),
                    nullable: inner.nullable,
                }
            },
            Step::RepeatUp(ref count, inner) => {
                let inner = get(inner);

                let count_includes_zero = match count.get_type() {
                    Type::Integer(lo, hi) => lo <= 0 && hi >= 0,
                    count_type => {
                        warn!("Loop count type is {:?} not int", count_type);
                        false
                    }
                };

                MatchSet::check_compatible(&inner.followlast, &inner.first);

                StepInfo {
                    first: inner.first.clone(),
                    followlast: Some(inner.first.clone()),
                    nullable: inner.nullable || count_includes_zero,
                }
            },
            Step::Foreach { inner, ref vars_dn, .. } => {
                let inner = get(inner);

                MatchSet::check_compatible(&inner.followlast, &inner.first);

                // If the `for` block is introducing variables, the send from first
                // cannot be lifted out of the block because it relies on those
                // variables being defined. It would conflict with any alternatives anyway
                // since those couldn't use the variable.
                // TODO: is testing vars_dn the right condition or should we more specifically
                // look at whether `first` contains a send with inner variables.
                if vars_dn.is_empty() {
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
            Step::AltDn(ref opts) => {
                let nullable = opts.iter().any(|&(_, s)| get(s).nullable);
                let followlast = MatchSet::merge_followlast(opts.iter().map(|x| &get(x.1).followlast));

                StepInfo {
                    first: MatchSet::proc(),
                    followlast,
                    nullable,
                }
            },
            Step::AltUp(ref opts) => {
                let first = MatchSet::merge_first(opts.iter().map(|x| &get(x.1).first));
                let followlast = MatchSet::merge_followlast(opts.iter().map(|x| &get(x.1).followlast));
                let nullable = opts.iter().all(|x| get(x.1).nullable);

                StepInfo { first, nullable, followlast }
            },
        };
        res.push(info);
    }

    res
}
