use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use crate::{PrimitiveProcess};
use super::{Expr, ExprDn, MatchSet, Shape, Type, ValueId, Dir};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct StepId(pub(crate) u32);

#[derive(Debug)]
pub enum Step {
    Stack { lo: StepId, shape: Shape, hi: StepId },
    Token { variant: usize, send: Vec<ExprDn>, receive: Vec<Expr> },
    TokenTop { top_dir: Dir, variant: usize, send: Vec<Expr>, receive: Vec<ExprDn>, inner: StepId },
    Primitive(Box<dyn PrimitiveProcess + 'static>),
    Seq(Vec<StepId>),
    RepeatDn(ExprDn, StepId),
    RepeatUp(Expr, StepId),
    Foreach { iters: u32, vars_dn: Vec<(ValueId, ExprDn)>, vars_up: Vec<(ValueId, Expr)>, inner: StepId },
    AltDn(Vec<(Vec<(Expr, ExprDn)>, StepId)>),
    AltUp(Vec<(Vec<(ExprDn, Expr)>, StepId)>),
}

pub fn write_tree(f: &mut dyn Write, indent: u32, steps: &[Step], step: StepId) -> IoResult<()> {
    let i: String = repeat(" ").take(indent as usize).collect();
    match steps[step.0 as usize] {
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
            for &(id, ref expr) in vars_dn { write!(f, "{}<={:?}, ", id, expr)?; }
            for &(id, ref expr) in vars_up { write!(f, "{}=>{:?}, ", id, expr)?; }
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
}

pub fn analyze_unambiguous(steps: &[Step]) -> Vec<StepInfo> {
    let mut res = vec![];

    for step in steps {
        let get = |id: StepId| -> &StepInfo {&res[id.0 as usize]};

        let info = match *step {
            Step::Stack { .. } => {
                StepInfo {
                    first: MatchSet::proc(),
                    nullable: false,
                }
            },
            Step::Token { variant, ref send, ref receive } => {
                StepInfo {
                    first: MatchSet::lower(variant, send.clone(), receive.clone()),
                    nullable: false,
                }
            },
            Step::TokenTop { top_dir, variant, ref send, inner, .. } => {
                let inner = get(inner);
                match top_dir {
                    Dir::Up => {
                        StepInfo {
                            first: inner.first.clone(),
                            nullable: inner.nullable,
                        }
                    },
                    Dir::Dn => {
                        StepInfo {
                            first: MatchSet::upper(variant, send.clone()),
                            nullable: false,
                        }
                    },
                }
            },
            Step::Primitive(_) => {
                StepInfo {
                    first: MatchSet::proc(),
                    nullable: false,
                }
            },
            Step::Seq(ref steps) => {
                let mut nullable = true;
                let mut first = MatchSet::null();

                for &s in steps {
                    let s = get(s);
                    if nullable {
                        first = first.merge(&s.first);
                        nullable &= s.nullable;
                    }
                }

                //TODO: check that each adjacent followlast and first are non-overlapping
                StepInfo {
                    first,
                    nullable,
                }
            },
            Step::RepeatDn(ref _count, inner) => {
                let inner = get(inner);

                StepInfo {
                    first: inner.first.clone(),
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

                StepInfo {
                    first: inner.first.clone(),
                    nullable: inner.nullable || count_includes_zero,
                }
            },
            Step::Foreach { inner, .. } => {
                let inner = get(inner);

                //TODO: check that inner followlast and first are non-overlapping
                StepInfo {
                    first: inner.first.clone(),
                    nullable: inner.nullable,
                }
            },
            Step::AltDn(ref opts) => {
                let nullable = opts.iter().any(|&(_, s)| get(s).nullable);

                StepInfo {
                    first: MatchSet::proc(),
                    nullable,
                }
            },
            Step::AltUp(ref opts) => {
                let mut nullable = false;
                let mut first = MatchSet::null();

                // TODO: check that first is nonoverlapping
                for &(_, s) in opts {
                    let s = get(s);
                    first = first.merge(&s.first);
                    nullable |= s.nullable;
                }

                StepInfo { first,  nullable }
            },
        };
        res.push(info);
    }

    res
}
