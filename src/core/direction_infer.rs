use bit_set::BitSet;
use crate::core::{ ValueId, Expr, Step, StepInfo, Message, Fields, DataMode };

/// Summary of the usage of values within an block and its children
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ResolveInfo {
    /// The set of variable IDs that will be down-evaluated to produce a value used by the
    /// block. The enclosing expression must set these variables.
    pub vars_down: BitSet,

    /// The set of variable IDs that are produced in up-evaluation within the block.
    pub vars_up: BitSet,

    /// Whether the expression contains blocks that force an enclosing repeat block to always
    /// up-evaluate its count.
    pub repeat_up_heuristic: bool,
}

impl ResolveInfo {
    pub fn new() -> ResolveInfo {
        ResolveInfo {
            vars_down: BitSet::new(),
            vars_up: BitSet::new(),
            repeat_up_heuristic: false,
        }
    }

    pub fn from_message(msg: &Message, bottom_fields: &Fields) -> ResolveInfo {
        assert_eq!(msg.len(), bottom_fields.len());
        let mut ri = ResolveInfo::new();
        for (m, f) in msg.iter().zip(bottom_fields.iter()) {
            if !f.is_tag {
                if let &Some(ref expr) = m {
                    ri.use_expr(expr, f.dir);
                }
            }
        }
        ri
    }

    pub fn with_up(&self) -> ResolveInfo {
        let mut ri = self.clone();
        ri.repeat_up_heuristic = true;
        ri
    }

    pub fn mode_of(&self, id: ValueId) -> DataMode {
        DataMode { up: self.vars_up.contains(id), down: self.vars_down.contains(id)}
    }

    fn use_expr(&mut self, e: &Expr, dir: DataMode) {
        e.each_var(&mut |id| {
            if dir.down { self.vars_down.insert(id); }
            if dir.up { self.vars_up.insert(id); }
        });
        self.repeat_up_heuristic |= dir.up && e.refutable();
    }

    pub fn merge_seq(mut self, o: &ResolveInfo) -> Self {
        self.vars_down.union_with(&o.vars_down);
        self.vars_up.union_with(&o.vars_up);
        self.repeat_up_heuristic |= o.repeat_up_heuristic;
        self
    }

    pub fn steps(steps: &Vec<StepInfo>) -> ResolveInfo {
        steps.iter().fold(ResolveInfo::new(), |ri, step| ri.merge_seq(&step.dir))
    }

    pub fn repeat(&self, count: &Expr) -> ResolveInfo {
        let mut ri = self.clone();
        let any_up = ri.repeat_up_heuristic;
        ri.use_expr(count, DataMode { down: !any_up, up: any_up });
        assert_eq!(any_up, ri.repeat_up_heuristic);
        ri
    }

    pub fn foreach(&self, inner_vars: &Vec<(ValueId, Expr)>) -> ResolveInfo {
        let mut ri = self.clone();
        for &(id, ref e) in inner_vars {
            let dir = ri.mode_of(id);
            ri.use_expr(e, dir);
        }
        ri
    }

    pub fn alt(arms: &Vec<(Vec<(Expr, Expr)>, StepInfo)>) -> ResolveInfo {
        let mut ri = ResolveInfo::new();
        for &(_, ref body) in arms {
            ri = ri.merge_seq(&body.dir); // TODO: alt != seq ?
        }

        let up = ri.repeat_up_heuristic;

        for &(ref checks, _) in arms {
            for &(_, ref r) in checks {
                ri.use_expr(r, DataMode { down: !up, up: up })
                // LHS is patterns that don't contain dynamic vars, so no need to mark them
            }
        }

        assert_eq!(up, ri.repeat_up_heuristic);
        ri
    }
}

pub fn infer_top_fields(step: &StepInfo, top_fields: &mut Fields) {
    use self::Step::*;
    match step.step {
        Nop => (),

        Process(ref processes) => {
            if let Some(ref last) = processes.processes.last() {
                if last.fields_up.len() > 0 {
                    unimplemented!();
                }
            }
        }

        TokenTop(ref msg, ref body) => {

            // Update the upward shape's direction with results of analyzing the usage of
            // its data in the `on x { ... }` body.
            for (m, f) in msg.iter().zip(top_fields.iter_mut()) {
                if !f.is_tag {
                    if let &Some(ref expr) = m {
                        let constraint = match *expr {
                            Expr::Variable(id, _) => body.dir.mode_of(id),

                            //TODO: is the down value right?
                            ref e => DataMode { up: e.down_evaluable(), down: false }
                        };
                        f.dir.constrain(constraint);
                    }
                }
            }
        }
        Seq(ref steps) => {
            for step in steps {
                infer_top_fields(step, top_fields);
            }
        }
        Repeat(_, ref body) | Foreach(_, _, ref body) => infer_top_fields(body, top_fields),
        Alt(ref arms) => {
            for &(_, ref body) in arms {
                infer_top_fields(body, top_fields);
            }
        }
    }
}
