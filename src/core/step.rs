use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use crate::{PrimitiveProcess};
use super::{Expr, ExprDn, MatchSet, Shape, Type, ValueId, resolve::Builder, Dir};

#[derive(Debug)]
pub struct StepInfo {
    pub(crate) step: Step,
    pub(crate) nullable: bool,
    pub(crate) first: MatchSet,
}

#[derive(Debug)]
pub enum Step {
    Chain(Vec<StepInfo>, Vec<Shape>),
    Token { variant: usize, send: Vec<ExprDn>, receive: Vec<Expr> },
    TokenTop { variant: usize, send: Vec<Expr>, receive: Vec<ExprDn>, inner: Box<StepInfo> },
    Primitive(Box<dyn PrimitiveProcess + 'static>),
    Seq(Vec<StepInfo>),
    RepeatDn(ExprDn, Box<StepInfo>),
    RepeatUp(Expr, Box<StepInfo>),
    Foreach { iters: u32, vars_dn: Vec<(ValueId, ExprDn)>, vars_up: Vec<(ValueId, Expr)>, inner: Box<StepInfo> },
    AltDn(Vec<(Vec<(Expr, ExprDn)>, StepInfo)>),
    AltUp(Vec<(Vec<(ExprDn, Expr)>, StepInfo)>),
}

impl StepInfo {
    pub fn write_tree(&self, f: &mut dyn Write, indent: u32) -> IoResult<()> {
        let i: String = repeat(" ").take(indent as usize).collect();
        match &self.step {
            Step::Chain(ref c, _) => {
                writeln!(f, "{}Chain:", i)?;
                for step in c {
                    step.write_tree(f, indent+2)?;
                }
            }
            Step::Primitive(_) => {
                writeln!(f, "{}Primitive", i)?
            }
            Step::Token { variant, send: dn, receive: up } => {
                writeln!(f, "{}Token: {:?} {:?} {:?}", i, variant, dn, up)?
            }
            Step::TokenTop { variant, send: dn, receive: up, inner } => {
                writeln!(f, "{}Up: {:?} {:?} {:?}", i, variant, dn, up)?;
                inner.write_tree(f, indent+1)?;
            }
            Step::Seq(ref steps) => {
                writeln!(f, "{}Seq", i)?;
                for c in steps.iter() {
                    c.write_tree(f, indent+1)?;
                }
            }
            Step::RepeatDn(ref count, ref inner) => {
                writeln!(f, "{}Repeat[Dn]: {:?}", i, count)?;
                inner.write_tree(f, indent + 1)?;
            }
            Step::RepeatUp(ref count, ref inner) => {
                writeln!(f, "{}Repeat[Up]: {:?}", i, count)?;
                inner.write_tree(f, indent + 1)?;
            }
            Step::Foreach { iters, ref vars_dn, ref vars_up, ref inner } => {
                write!(f, "{}For: {} ", i, iters)?;
                for &(id, ref expr) in vars_dn { write!(f, "{}<={:?}, ", id, expr)?; }
                for &(id, ref expr) in vars_up { write!(f, "{}=>{:?}, ", id, expr)?; }
                writeln!(f, "")?;
                inner.write_tree(f, indent + 1)?;
            }
            Step::AltDn(ref arms) => {
                writeln!(f, "{}Alt[{:?}]:", i, Dir::Dn)?;
                for &(ref cond, ref inner) in arms {
                    writeln!(f, "{} {:?} =>", i, cond)?;
                    inner.write_tree(f, indent + 2)?;
                }
            }
            Step::AltUp(ref arms) => {
                writeln!(f, "{}Alt[{:?}]:", i, Dir::Up)?;
                for &(ref cond, ref inner) in arms {
                    writeln!(f, "{} {:?} =>", i, cond)?;
                    inner.write_tree(f, indent + 2)?;
                }
            }
        }
        Ok(())
    }
}

pub(crate) struct StepBuilder;

impl Builder for StepBuilder {
    type Res = StepInfo;

    fn chain(&self, steps: Vec<StepInfo>, shapes: Vec<Shape>) -> StepInfo {
        assert_eq!(shapes.len() + 1, steps.len());

        StepInfo {
            first: MatchSet::proc(),
            nullable: false,
            step: Step::Chain(steps, shapes)
        }
    }

    fn primitive(&self, prim: Box<dyn PrimitiveProcess + 'static>) -> StepInfo {
        StepInfo {
            first: MatchSet::proc(),
            nullable: false,
            step: Step::Primitive(prim),
        }
    }

    fn token(&mut self, variant: usize, dn: Vec<ExprDn>, up: Vec<Expr>) -> StepInfo {
        StepInfo {
            first: MatchSet::lower(variant, dn.clone(), up.clone()),
            nullable: false,
            step: Step::Token { variant, send: dn, receive: up }
        }
    }

    fn token_top(&mut self, top_dir: Dir, variant: usize, dn: Vec<Expr>, up: Vec<ExprDn>, inner: StepInfo) -> StepInfo {
        match top_dir {
            Dir::Up => {
                StepInfo {
                    first: inner.first.clone(),
                    nullable: inner.nullable,
                    step: Step::TokenTop { variant, send: dn, receive: up, inner: Box::new(inner) }
                }
            },
            Dir::Dn => {
                StepInfo {
                    first: MatchSet::upper(variant, dn.clone()),
                    nullable: false,
                    step: Step::TokenTop { variant, send: dn, receive: up, inner: Box::new(inner) }
                }
            },
        }
    }

    fn seq(&mut self, steps: Vec<StepInfo>) -> StepInfo {
        let mut nullable = true;
        let mut first = MatchSet::null();

        for s in &steps {
            if nullable {
                first = first.merge(&s.first);
                nullable &= s.nullable;
            }
        }

        //TODO: check that each adjacent followlast and first are non-overlapping
        StepInfo {
            first,
            nullable,
            step: Step::Seq(steps)
        }
    }

    fn repeat(&mut self, dir: Dir, count: Expr, inner: StepInfo) -> StepInfo {
        let count_includes_zero = match count.get_type() {
            Type::Integer(lo, hi) => lo <= 0 && hi >= 0,
            count_type => {
                warn!("Loop count type is {:?} not int", count_type);
                false
            }
        };

        match dir {
            Dir::Up => {
                StepInfo {
                    first: inner.first.clone(),
                    nullable: count_includes_zero || inner.nullable,
                    step: Step::RepeatUp(count, Box::new(inner))
                }
            }
            Dir::Dn => {
                StepInfo {
                    first: MatchSet::proc(),
                    nullable: count_includes_zero,
                    step: Step::RepeatDn(count.down(), Box::new(inner))
                }
            },
        }
    }

    fn foreach(&mut self, iters: u32, vars: Vec<(ValueId, Expr)>, inner: StepInfo) -> StepInfo {
        //TODO: check that inner followlast and first are non-overlapping

        let mut vars_dn = Vec::new();
        let mut vars_up = Vec::new();

        for (id, e) in vars {
            let dir = e.dir();
            if dir.down {
                vars_dn.push((id, e.down()));
            }
            if dir.up {
                vars_up.push((id, e));
            }
        }

        StepInfo {
            first: inner.first.clone(),
            nullable: inner.nullable,
            step: Step::Foreach { iters, vars_dn, vars_up, inner: Box::new(inner) }
        }
    }

    fn alt(&mut self, dir: Dir, opts: Vec<(Vec<(Expr, Expr)>, StepInfo)>) -> StepInfo {
        match dir {
            Dir::Up => {
                let mut nullable = false;
                    let mut first = MatchSet::null();

                // TODO: check that first is nonoverlapping
                for (_, s) in &opts {
                    first = first.merge(&s.first);
                    nullable |= s.nullable;
                }

                let opts = opts.into_iter().map(|(e, b)|
                    (e.into_iter().map(|(l, r)| (l.down(), r)).collect(), b)
                ).collect();

                StepInfo {
                    first,
                    nullable,
                    step: Step::AltUp(opts)
                }
            },
            Dir::Dn => {
                let nullable = opts.iter().any(|(_, s)| s.nullable);
                let opts = opts.into_iter().map(|(e, b)|
                    (e.into_iter().map(|(l, r)| (l, r.down())).collect(), b)
                ).collect();

                StepInfo {
                    first: MatchSet::proc(),
                    nullable,
                    step: Step::AltDn(opts)
                }
            },
        }
    }
}
