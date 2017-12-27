use bit_set::BitSet;
use session::ValueID;
use super::eval::Expr;
use super::step::{ Step, StepInfo };
use protocol::Fields;
use data::DataMode;

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

    pub fn mode_of(&self, id: ValueID) -> DataMode {
        DataMode { up: self.vars_up.contains(id), down: self.vars_down.contains(id)}
    }

    fn use_expr(&mut self, e: &Expr, dir: DataMode) {
        e.each_var(&mut |id| {
            if dir.down { self.vars_down.insert(id); }
            if dir.up { self.vars_up.insert(id); }
        });
        self.repeat_up_heuristic |= dir.up && e.refutable();
    }

    fn merge_seq(&mut self, o: &ResolveInfo) {
        self.vars_down.union_with(&o.vars_down);
        self.vars_up.union_with(&o.vars_up);
        self.repeat_up_heuristic |= o.repeat_up_heuristic;
    }
}

pub fn infer_direction(step: &Step, bottom_fields: &Fields) -> ResolveInfo {
    use self::Step::*;
    match *step {
        Nop => ResolveInfo::new(),
        Token(ref msg) => {
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
        TokenTop(_, ref body) => {
            let mut ri = body.dir.clone();
            ri.repeat_up_heuristic = true;
            ri
        }
        Seq(ref steps) => {
            let mut ri = ResolveInfo::new();
            for step in steps {
                ri.merge_seq(&step.dir);
            }
            ri
        }
        Repeat(ref count, ref body) => {
            let mut ri = body.dir.clone();
            let any_up = ri.repeat_up_heuristic;
            ri.use_expr(&count, DataMode { down: !any_up, up: any_up });
            assert_eq!(any_up, ri.repeat_up_heuristic);
            ri
        }
        Foreach(_, ref inner_vars, ref body) => {
            let mut ri = body.dir.clone();
            for &(id, ref e) in inner_vars {
                let dir = ri.mode_of(id);
                ri.use_expr(e, dir);
            }
            ri
        }
        Alt(ref arms) => {
            let mut ri = ResolveInfo::new();
            for &(_, ref body) in arms {
                ri.merge_seq(&body.dir); // TODO: alt != seq ?
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
        Fork(ref bottom, _, ref top) => {
            let mut ri = bottom.dir.clone();
            ri.merge_seq(&top.dir);
            ri
        }
        Primitive(_) => panic!("Direction metadata for primitive must be provided directly")
    }
}


pub fn infer_top_fields(step: &StepInfo, top_fields: &mut Fields) {
    use self::Step::*;
    match step.step {
        Nop | Token(..) => (),
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

        Fork(_, _, ref upper) => {
            // Only the upper process has these top fields. The lower process already had its
            // direction inferred (they're stored in the middle field of this enum).
            infer_top_fields(upper, top_fields)
        }

        Primitive(_) => {
            // Primitives can only exist in calls, and do not affect the top direction of the containing process
        }
    }
}
