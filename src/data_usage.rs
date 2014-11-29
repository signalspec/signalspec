//! Data usage passes: Shape direction inference and unused value elimination
use std::collections::BitvSet;
use eval::{Ops, ValOp};
use exec::{ Message, Step };
use resolve::types::Shape;
use resolve::context::{ValueID};
use resolve::scope::{Dynamic, Ignored, Poison};
use resolve;


/*
Note that the algorithm here does more analysis than strictly necessary. We only need to propagate
usage information lexically outward, because propagation lexically inward has already been
performed by the resolve pass:

    * A passed down value is a promise that the value is defined (otherwise it would have passed Ignore)
    * A passed up value is a promise that the result is used (otherwise it would have passed Ignore)

However, more can be shared between direction inference and dead-value elimination if we don't
perform this optimization, and it will make it easier to handle future operators that reference
Down data in the Up direction.
*/


/// A set tracking which SSA variables are read and written
pub struct UsageSet {
    written: BitvSet,
    read: BitvSet,
}

impl UsageSet {
    pub fn new() -> UsageSet {
      let mut c = UsageSet {
        written: BitvSet::new(),
        read: BitvSet::new(),
      };
      // 0 is used for Check operations that don't have an output, but have a side effect on the
      // success of the basic block. Propagate the values it reads.
      c.read.insert(0);
      c
    }

    /// Propagate the `written` bit.
    /// Consumes an iterator of (dest, op) tuples, marking destination variables written if all
    /// source variables are marked written
    fn update_written<'a, I: DoubleEndedIterator<&'a (ValueID, ValOp)>>(&mut self, mut ops: I) {
        for &(id, ref op) in ops {
            if op.all_deps(|dep| self.written.contains(&dep)) {
                self.written.insert(id);
            }
        }
    }

    /// Propagate the `read` bit.
    /// Consume an iterator of (dest, op) tuples, marking source variables read if the destination
    /// variable is read.
    fn update_read<'a, I: DoubleEndedIterator<&'a(ValueID, ValOp)>>(&mut self, ops: I) {
        for &(id, ref op) in ops.rev() {
            if self.read.contains(&id) {
                op.each_dep(|id| { self.read.insert(id); })
            }
        }
    }

    /// When entering a block containing Ops, mark the down value IDs that are written (if all an Op's dependencies are written)
    /// and mark the up value IDs that are read (if a read value depends on it).
    fn enter_ops(&mut self, ops: &Ops) {
        self.update_written(ops.entry.iter());
        self.update_read(ops.exit.iter().rev());
    }

    /// When leaving a block containing Ops, mark the down value IDs that are read (if a read value depends on it),
    /// and mark the up value IDs as written (if all deps are written).
    fn leave_ops(&mut self, ops: &Ops) {
        self.update_read(ops.entry.iter());
        self.update_written(ops.exit.iter().rev());
    }

    /// Mark variables read and written as used in a Message on the downward stream.
    fn message_downward(&mut self, message: &Message, shape: &Shape) {
        match (message, shape) {
            (&Message::Value(down, up), &Shape::Val(_, is_down, is_up)) => {
                if is_down {
                    match down {
                        Dynamic(id) => {
                            assert!(self.written.contains(&id), "Use of value that was never written");
                            self.read.insert(id);
                        }
                        Poison(err) => panic!("Poison value used: {}", err),
                        Ignored => panic!("Required value is ignored"),
                    }
                }

                if is_up {
                    match up {
                        Dynamic(id) => { self.written.insert(id); },
                        Poison(err) => panic!("Poison value written: {}", err),
                        Ignored => (),
                    }
                }
            }
            (&Message::Tuple(ref ms), &Shape::Tup(ref ss)) => {
                for (m, s) in ms.iter().zip(ss.iter()) {
                    self.message_downward(m, s);
                }
            }
            _ => panic!("Message does not match shape")
        }
    }

    /// Mark variables read and written as used in a Message on the upward stream
    fn message_upward(&mut self, message: &Message) {
        match *message {
            Message::Value(down, up) => {
                match down {
                    Dynamic(id) => { self.written.insert(id); }
                    Poison(err) => panic!("Poison value used: {}", err),
                    Ignored => (),
                }
                match up {
                    Dynamic(id) => { self.read.insert(id); }
                    Poison(err) => panic!("Poison value used: {}", err),
                    Ignored => panic!("Required value is ignored"),
                }
            }
            Message::Tuple(ref ms) => {
                for m in ms.iter() {
                    self.message_upward(m);
                }
            }
        }
    }
}


/// Analyze the Step tree to infer the upward shape's data direction based on the known downward
/// shape's data direction.
pub fn direction_analysis(cx: &mut UsageSet, step: &Step, down: &mut Shape, up: &mut Shape) {

    fn update_upward_shape(set: &UsageSet, message: &Message, shape: &mut Shape) {
        match (message, shape) {
            (&Message::Value(down, up), &Shape::Val(_, ref mut is_down, ref mut is_up)) => {
                *is_down |= match down {
                  Dynamic(id) => set.read.contains(&id),
                  Ignored => false,
                  Poison(s) => panic!("Poison value written: {}", s)
                };
                *is_up &= match up {
                  Dynamic(id) => set.written.contains(&id),
                  Ignored => false,
                  Poison(s) => panic!("Poison value used: {}", s)
                };
            }
            (&Message::Tuple(ref ms), &Shape::Tup(ref mut ss)) => {
                for (m, s) in ms.iter().zip(ss.iter_mut()) {
                    update_upward_shape(set, m, s);
                }
            }
            _ => panic!("Shape doesn't match message")
        }
    }

    match *step {
        Step::Nop => (),
        Step::Token(ref ops, ref msg) => {
          cx.enter_ops(ops);
          cx.message_downward(msg, down);
          // direction_analysis(cx, body, down, up);
          cx.leave_ops(ops);
        }
        Step::TokenTop(ref ops, ref msg, box ref body) => {
          cx.message_upward(msg);
          cx.enter_ops(ops);
          direction_analysis(cx, body, down, up);
          cx.leave_ops(ops);
          update_upward_shape(cx, msg, up);
        }
        Step::Seq(ref steps) => {
          for c in steps.iter() {
              direction_analysis(cx, c, down, up);
          }
        }
        Step::Repeat((_cd, cu), box ref inner) => {
          direction_analysis(cx, inner, down, up);

          // TODO: support repeat down count
          if let Dynamic(id) = cu {
              cx.written.insert(id);
          }

        }
    }
}

/// Remove operations and message components from a Step tree that do not serve a useful function.
/// Note that as it is an error to read unset values, this pass changes the semantics of the program
/// (by making programs that do not have their data direction explicitly defined match the shape
/// inferred by the direction_analysis pass, and not touch invalid registers)
pub fn sweep_unused(cx: &mut UsageSet, step: &mut Step, down: &Shape, up: &Shape) {

    fn sweep_op_vec(cx: &UsageSet, vec: &mut Vec<(ValueID, ValOp)>) {
        vec.retain(|&(id, _)| cx.read.contains(&id) && cx.written.contains(&id));
    }

    fn sweep_ops(cx: &UsageSet, ops: &mut Ops) {
        sweep_op_vec(cx, &mut ops.entry);
        sweep_op_vec(cx, &mut ops.exit);
    }

    fn sweep_msg(cx: &UsageSet, message: &mut Message, shape: &Shape) {
        match (message, shape) {
            (&Message::Value(ref mut down, ref mut up), &Shape::Val(_, is_down, is_up)) => {
                if !is_down { *down = Ignored }
                if !is_up   { *up = Ignored }
            }
            (&Message::Tuple(ref mut ms), &Shape::Tup(ref ss)) => {
                for (m, s) in ms.iter_mut().zip(ss.iter()) {
                    sweep_msg(cx, m, s);
                }
            }
            _ => panic!("Shape doesn't match message")
        }
    }

    match *step {
        Step::Nop => (),
        Step::Token(ref mut ops, ref mut msg) => {
            sweep_msg(cx, msg, down);
            cx.enter_ops(ops);
            cx.message_downward(msg, down);
            cx.leave_ops(ops);
            sweep_ops(cx, ops);
        }
        Step::TokenTop(ref mut ops, ref mut msg, box ref mut body) => {
            sweep_msg(cx, msg, up);
            cx.message_upward(msg);
            cx.enter_ops(ops);
            sweep_unused(cx, body, down, up);
            cx.leave_ops(ops);
            sweep_ops(cx, ops);
        }
        Step::Seq(ref mut steps) => {
            for c in steps.iter_mut() {
                sweep_unused(cx, c, down, up);
            }
        }
        Step::Repeat((ref mut cd, ref mut cu), box ref mut inner) => {
            sweep_unused(cx, inner, down, up);
            if let Dynamic(id) = *cu {
                cx.written.insert(id);
            }
            *cd = Ignored;
        }
    }
}

/// Run both data usage passes
pub fn pass(step: &mut Step, signals: &mut resolve::SignalInfo) {
    let mut dctx = UsageSet::new();
    debug!("before: {}", signals);
    debug!("step: {}", step);
    direction_analysis(&mut dctx, step, &mut signals.downwards, &mut signals.upwards);

    let mut dctx = UsageSet::new();
    sweep_unused(&mut dctx, step, &signals.downwards, &signals.upwards);
    debug!("after: {}", signals);
    debug!("step: {}", step);
}
