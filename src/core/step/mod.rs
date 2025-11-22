mod expr_dn;
mod build_step;
mod predicate;
mod step;
mod expr_lower;

use crate::entitymap::entity_key;

pub use self::step::{ Step, StepId, ChannelId, ProcId };
pub(crate) use self::step::{StepBuilder, ConnectionId, SubProc};

pub use self::expr_dn::{ ExprDn, ExprCtx, ExprDnId };
pub use self::predicate::Predicate;
pub(crate) use build_step::build_step_tree;

entity_key!(pub ValueSrcId);

impl ValueSrcId {
    pub fn fields(self) -> impl Iterator<Item = ValueSrc> {
        (0..u32::MAX).map(move |i| ValueSrc(self, i))
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub struct ValueSrc(pub ValueSrcId, pub u32);
