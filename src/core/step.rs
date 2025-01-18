use std::fmt::Debug;
use std::sync::Arc;

use crate::diagnostic::ErrorReported;
use crate::entitymap::{entity_key, EntityIntern, EntityMap};
use crate::runtime::PrimitiveProcess;
use crate::Dir;
use super::expr_dn::{ExprCtx, ExprDnId};
use super::{Shape, Predicate, ValueSrcId};

entity_key!(pub StepId);

impl StepId {
    pub(crate) const FAIL: StepId = StepId(0);
    pub(crate) const ACCEPT: StepId = StepId(1);
}

entity_key!(pub ConnectionId);
entity_key!(pub ProcId);

impl ConnectionId {
    pub fn dn(self) -> ChannelId { ChannelId::new(self, Dir::Dn) }
    pub fn up(self) -> ChannelId { ChannelId::new(self, Dir::Up) }
}

/// [`ConnectionId`] and [`Dir`] packed into a u32.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChannelId(u32);

impl Debug for ChannelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ChannelId")
            .field(&self.connection())
            .field(&self.dir())
            .finish()
    }
}

impl crate::entitymap::EntityKey for ChannelId {
    #[inline]
    fn new(index: usize) -> Self {
        assert!(index < (u32::MAX as usize));
        ChannelId(index as u32)
    }

    #[inline]
    fn index(self) -> usize {
        self.0 as usize
    }
}

impl ChannelId {
    pub fn new(conn: ConnectionId, dir: Dir) -> ChannelId {
        ChannelId(conn.0 << 1 | match dir {
            Dir::Up => 1,
            Dir::Dn => 0,
        })
    }

    pub fn dir(self) -> Dir {
        if self.0 & 0x1 == 1 { Dir::Up } else { Dir::Dn }
    }

    pub fn connection(self) -> ConnectionId {
        ConnectionId(self.0 >> 1)
    }
}

pub struct SubProc {
    pub func: Arc<dyn PrimitiveProcess + 'static>,
    pub channels: Vec<ChannelId>,
}

#[derive(Debug, Hash, PartialEq, Eq)]

pub enum Step {
    Fail,
    Accept,
    Pass,
    Stack { lo: StepId, conn: ConnectionId, hi: StepId },
    Send { chan: ChannelId, variant: usize, msg: Vec<ExprDnId> },
    Receive { chan: ChannelId, variant: usize, var: ValueSrcId, msg: Vec<Predicate> },
    Process(ProcId),
    Seq(StepId, StepId),
    Assign(ValueSrcId, ExprDnId),
    Guard(ExprDnId, Predicate),
    Repeat(StepId, bool),
    Alt(Box<[StepId]>),
}

pub(crate) struct StepBuilder {
    pub ecx: ExprCtx,
    pub steps: EntityIntern<StepId, Step>,
    pub connections: EntityMap<ConnectionId, Shape>,
    pub processes: EntityMap<ProcId, SubProc>,
}

impl StepBuilder {
    pub(crate) fn new() -> Self {
        let ecx = ExprCtx::new();
        let mut steps = EntityIntern::new();
        assert_eq!(steps.insert(Step::Fail), StepId::FAIL);
        assert_eq!(steps.insert(Step::Accept), StepId::ACCEPT);

        let connections = EntityMap::new();
        Self {
            ecx,
            steps,
            connections,
            processes: EntityMap::new(),
        }
    }
    
    pub(crate) fn invalid(&self, _r: ErrorReported) -> StepId {
        StepId::FAIL
    }
    
    pub(crate) fn accepting(&self) -> StepId {
        StepId::ACCEPT
    }
    
    pub(crate) fn seq(&mut self, first: StepId, second: StepId) -> StepId {
        match (first, second) {
            (StepId::ACCEPT, _) => return second,
            (_, StepId::ACCEPT) => return first,
            (StepId::FAIL, _) => return StepId::FAIL,
            _ => {}
        }

        if let Step::Seq(a, b) = self.steps[first] {
            let s1 = self.seq(b, second);
            return self.seq(a, s1)
        }

        self.steps.insert(Step::Seq(first, second))
    }

    pub(crate) fn seq_from<I>(&mut self, i: I) -> StepId where I: IntoIterator<Item = StepId>, I::IntoIter: DoubleEndedIterator {
        i.into_iter().rev()
            .reduce(|s2, s1| self.seq(s1, s2))
            .unwrap_or(self.accepting())
    }
    
    pub(crate) fn receive(&mut self, chan: ChannelId, variant: usize, val: ValueSrcId, msg: Vec<Predicate>) -> StepId {
        self.steps.insert(Step::Receive { chan, variant, var: val, msg })
    }
    
    pub(crate) fn send(&mut self, chan: ChannelId, variant: usize, msg: Vec<ExprDnId>) -> StepId {
        self.steps.insert(Step::Send { chan, variant, msg })
    }

    pub(crate) fn add_process(&mut self, func: Arc<dyn PrimitiveProcess + 'static>, channels: Vec<ChannelId>) -> StepId {
        let id = self.processes.push(SubProc { func, channels });
        self.steps.insert(Step::Process(id))
    }

    pub(crate) fn assign(&mut self, dest: ValueSrcId, src: ExprDnId) -> StepId {
        self.steps.insert(Step::Assign(dest, src))
    }

    pub(crate) fn guard(&mut self, src: ExprDnId, pred: Predicate) -> StepId {
        match pred {
            Predicate::Any => self.accepting(),
            pred => self.steps.insert(Step::Guard(src, pred))
        }
    }

    pub(crate) fn alt(&mut self, arms: Vec<StepId>) -> StepId {
        let mut new_arms = Vec::with_capacity(arms.len());
        for arm in arms {
            match &self.steps[arm] {
                Step::Fail => {}
                Step::Alt(inner) => new_arms.extend_from_slice(inner),
                _ => new_arms.push(arm),
            }
        }
        new_arms.sort_unstable();
        new_arms.dedup();

        if new_arms.len() == 0 {
            StepId::FAIL
        } else if new_arms.len() == 1 {
            new_arms[0]
        } else {
            self.steps.insert(Step::Alt(new_arms.into_boxed_slice()))
        }
    }

    pub(crate) fn repeat(&mut self, inner: StepId, nullable: bool) -> StepId {
        self.steps.insert(Step::Repeat(inner, nullable))
    }
    
    pub(crate) fn pass(&mut self) -> StepId {
        self.steps.insert(Step::Pass)
    }
    
    pub(crate) fn stack(&mut self, lo: StepId, conn: ConnectionId, hi: StepId) -> StepId {
        match (&self.steps[lo], &self.steps[hi]) {
            (Step::Fail, _) | (_, Step::Fail) => StepId::FAIL,
            (Step::Accept, Step::Accept) => StepId::ACCEPT,
            (Step::Pass, _) => hi,
            (_, Step::Pass) => lo,
            (Step::Seq(a, b), _) if matches!(self.steps[*a], Step::Pass) => {
                self.seq(hi, *b)
            }
            _ => {
                self.steps.insert(Step::Stack { lo, conn, hi })
            }
        }
    }
    
    pub(crate) fn add_connection(&mut self, shape: Shape) -> ConnectionId {
        self.connections.push(shape)
    }
}

pub fn write_tree(f: &mut dyn std::fmt::Write, indent: u32, steps: &EntityIntern<StepId, Step>, step: StepId) -> Result<(), std::fmt::Error> {
    let i: String = " ".repeat(indent as usize);
    match steps[step] {
        Step::Fail => writeln!(f, "{:6} {}Fail", step.0, i)?,
        Step::Accept => writeln!(f, "{:6} {}Accept", step.0, i)?,
        Step::Pass => writeln!(f, "{:6} {}Pass", step.0, i)?,
        Step::Stack{ lo, hi, ..} => {
            writeln!(f, "{:6} {}Stack:", step.0, i)?;
            write_tree(f, indent+2, steps, lo)?;
            write_tree(f, indent+2, steps, hi)?;
        }
        Step::Process(id) => {
            writeln!(f, "{:6} {}Primitive {id:?}", step.0, i)?
        }
        Step::Send { chan, variant, ref msg } => {
            writeln!(f, "{:6} {}Send: {:?} {:?} {:?}", step.0, i, chan, variant, msg)?;
        }
        Step::Receive { chan, variant, var: ref val, ref msg } => {
            writeln!(f, "{:6} {}Receive: {:?} {:?} {:?} {:?}", step.0, i, chan, variant, val, msg)?;
        }
        Step::Assign(dst, ref src) => {
            writeln!(f, "{:6} {}Assign {dst:?} = {src:?}", step.0, i)?;
        },
        Step::Guard(ref src, ref pred) => {
            writeln!(f, "{:6} {}Guard {src:?} is {pred:?}", step.0, i)?;
        }
        Step::Seq(s1, mut s2) => {
            writeln!(f, "{:6} {}Seq", step.0, i)?;
            write_tree(f, indent+1, steps, s1)?;

            // Flatten nested seq for readability
            while let Step::Seq(i1, i2) = steps[s2] {
                write_tree(f, indent+1, steps, i1)?;
                s2 = i2;
            }

            write_tree(f, indent+1, steps, s2)?;
        }
        Step::Repeat(inner, nullable) => {
            writeln!(f, "{:6} {}Repeat accepting={nullable}", step.0, i)?;
            write_tree(f, indent + 1, steps, inner)?;
        }
        Step::Alt(ref arms) => {
            writeln!(f, "{:6} {}Alt", step.0, i)?;
            for &arm in arms.iter() {
                write_tree(f, indent + 1, steps, arm)?;
            }
        }
    }
    Ok(())
}
