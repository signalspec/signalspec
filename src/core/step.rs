use std::io::{Write, Result as IoResult};
use std::iter::repeat;

use crate::{PrimitiveProcess};
use super::{Expr, MatchSet, Shape, Type, ValueId, resolve::Builder};

pub type Message = Vec<Option<Expr>>;

#[derive(Debug)]
pub struct StepInfo {
    pub(crate) step: Step,
    pub(crate) first: MatchSet,
}

impl StepInfo {
    pub fn fake(step: Step) -> StepInfo {
        StepInfo {
            step,
            first: MatchSet::null()
        }
    }
}

#[derive(Debug)]
pub enum Step {
    Nop,
    Chain(Vec<StepInfo>, Vec<Shape>),
    Token(Message),
    TokenTop(Message, Box<StepInfo>),
    Primitive(Box<dyn PrimitiveProcess + 'static>),
    Seq(Vec<StepInfo>),
    Repeat(Expr, Box<StepInfo>),
    Foreach(u32, Vec<(ValueId, Expr)>, Box<StepInfo>),
    Alt(Vec<(Vec<(Expr, Expr)>, StepInfo)>),
}

impl StepInfo {
    pub fn write_tree(&self, f: &mut dyn Write, indent: u32) -> IoResult<()> {
        let i: String = repeat(" ").take(indent as usize).collect();
        match self.step {
            Step::Nop => {},
            Step::Chain(ref c, _) => {
                writeln!(f, "{}Chain:", i)?;
                for step in c {
                    step.write_tree(f, indent+2)?;
                }
            }
            Step::Primitive(_) => {
                writeln!(f, "{} Primitive", i)?
            }
            Step::Token(ref message) => {
                writeln!(f, "{} Token: {:?}", i, message)?
            }
            Step::TokenTop(ref message, ref body) => {
                writeln!(f, "{}Up: {:?}", i, message)?;
                body.write_tree(f, indent+1)?;
            }
            Step::Seq(ref steps) => {
                writeln!(f, "{}Seq", i)?;
                for c in steps.iter() {
                    c.write_tree(f, indent+1)?;
                }
            }
            Step::Repeat(ref count, ref inner) => {
                writeln!(f, "{}Repeat: {:?}", i, count)?;
                inner.write_tree(f, indent + 1)?;
            }
            Step::Foreach(width, ref vars, ref inner) => {
                write!(f, "{}For: {} ", i, width)?;
                for &(id, ref expr) in vars { write!(f, "{}={:?}, ", id, expr)?; }
                writeln!(f, "")?;
                inner.write_tree(f, indent + 1)?;
            }
            Step::Alt(ref arms) => {
                writeln!(f, "{}Alt:", i)?;
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

    fn nop(&mut self) -> StepInfo {
        StepInfo {
            first: MatchSet::epsilon(),
            step: Step::Nop
        }
    }

    fn chain(&self, steps: Vec<StepInfo>, shapes: Vec<Shape>) -> StepInfo {
        assert_eq!(shapes.len() + 1, steps.len());
        
        let bottom_first = &steps.first().unwrap().first;
        let top_first = &steps.last().unwrap().first;

        StepInfo {
            first: MatchSet::join(&bottom_first, &top_first),
            step: Step::Chain(steps, shapes)
        }
    }

    fn primitive(&self, prim: Box<dyn PrimitiveProcess + 'static>) -> StepInfo {
        StepInfo {
            first: MatchSet::null(),
            step: Step::Primitive(prim),
        }
    }

    fn token(&mut self, message: Message) -> StepInfo {
        StepInfo {
            first: MatchSet::lower(message.clone()),
            step: Step::Token(message)
        }
    }

    fn token_top(&mut self, message: Message, inner: StepInfo) -> StepInfo {
        StepInfo {
            first: MatchSet::upper(message.clone()).followed_by(inner.first.clone()),
            step: Step::TokenTop(message, Box::new(inner))
        }
    }

    fn seq(&mut self, steps: Vec<StepInfo>) -> StepInfo {
        //TODO: check that each adjacent followlast and first are non-overlapping
        StepInfo {
            first: steps.iter().fold(MatchSet::epsilon(), |a, s| a.followed_by(s.first.clone())),
            step: Step::Seq(steps)
        }
    }

    fn repeat(&mut self, count: Expr, inner: StepInfo) -> StepInfo {
        let count_includes_zero = match count.get_type() {
            Type::Integer(lo, hi) => lo <= 0 && hi >= 0,
            count_type => {
                warn!("Loop count type is {:?} not int", count_type);
                false
            }
        };

        // TODO: check that inner followlast and first are nonoverlapping
        // TODO: require that inner is non-nullable?

        StepInfo {
            first: if count_includes_zero {
                inner.first.clone().alternative(MatchSet::epsilon())
            } else {
                inner.first.clone()
            },
            step: Step::Repeat(count, Box::new(inner))
        }
    }

    fn foreach(&mut self, length: u32, vars: Vec<(ValueId, Expr)>, inner: StepInfo) -> StepInfo {
        //TODO: check that inner followlast and first are non-overlapping

        StepInfo {
            first: inner.first.clone(),
            step: Step::Foreach(length, vars, Box::new(inner))
        }
    }

    fn alt(&mut self, opts: Vec<(Vec<(Expr, Expr)>, StepInfo)>) -> StepInfo {
        // TODO: check that first is nonoverlapping
        StepInfo {
            first: opts.iter().fold(MatchSet::null(), |a, &(_, ref inner)| a.alternative(inner.first.clone())),
            step: Step::Alt(opts)
        }
    }
}
