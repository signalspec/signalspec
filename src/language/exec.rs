use super::step::{ StepInfo, Message };
use super::step::Step::*;
use data::Value;
use connection::{ Connection, ConnectionMessage };
use session::ValueID;
use vec_map::VecMap;
use super::matchset::MatchSet;

pub fn run(step: &StepInfo, downwards: &mut Connection, upwards: &mut Connection) -> bool {
    let mut cx = RunCx { downwards, upwards, state: State::new() };
    run_inner(step, &mut cx)
}


struct RunCx<'a> {
//    fields_below: &'a Fields,
    downwards: &'a mut Connection,
//    fields_above: &'a Fields,
    upwards: &'a mut Connection,

    state: State,
}


impl<'a> RunCx<'a> {
    fn test(&mut self, m: &MatchSet) -> bool {
        for opt in &m.options {
            let dn_match = if let Some(ref msg) = opt.lower {
                if let Ok(rx) = self.downwards.peek() {
                    message_test(msg, rx)
                } else { false }
            } else { true };

            let up_match = if let Some(ref msg) = opt.upper {
                if let Ok(rx) = self.upwards.peek() {
                    message_test(msg, rx)
                } else { false }
            } else { true };

            if dn_match && up_match {
                return true;
            }
        }
        false
    }

    fn send_lower(&mut self, msg: &Message) -> Result<(), ()> {
        let tx = msg.iter().zip(self.downwards.sends.iter()).map(|(oe, sends)| {
            match (oe, sends) {
                (&Some(ref e), true) => Some(e.eval_down(&mut |var| self.state.get(var).clone())),
                _ => None
            }
        }).collect();
        debug!("{:?} Send lower {:?}", ::std::thread::current().id(), tx);
        self.downwards.send(tx)
    }

    fn send_upper(&mut self, msg: &Message) -> Result<(), ()> {
        let tx = msg.iter().zip(self.upwards.sends.iter()).map(|(oe, sends)| {
            match (oe, sends) {
                (&Some(ref e), true) => Some(e.eval_down(&mut |var| self.state.get(var).clone())),
                _ => None
            }
        }).collect();
        debug!("{:?} Send upper {:?}", ::std::thread::current().id(), tx);
        self.upwards.send(tx)
    }
}

/// Map of register IDs to values
pub struct State {
    registers: VecMap<Value>,
}

impl State {
    pub fn new() -> State {
        State {
            registers: VecMap::new(),
        }
    }

    pub fn get(&self, reg: ValueID) -> &Value {
        debug!("get {}: {:?}", reg, self.registers.get(reg));
        &self.registers[reg]
    }

    pub fn set(&mut self, reg: ValueID, v: Value) -> bool {
        debug!("set {}: {:?}", reg, v);
        self.registers.insert(reg, v);
        true
    }

    pub fn take(&mut self, reg: ValueID) -> Value {
        self.registers.remove(reg).expect("value not set")
    }
}



fn message_test(msg: &Message, rx: &ConnectionMessage) -> bool {
    debug!("{:?} Test {:?} into {:?}", ::std::thread::current().id(), rx, msg);
    msg.iter().zip(rx.iter()).all(|(oe, ov)| {
        match (oe, ov) {
            (&None, &None) => true,
            (&Some(ref e), &Some(ref v)) => e.eval_up(&mut |_, _| true, v.clone()),
            _ => false,
        }
    })
}

fn message_match(cx: &mut RunCx, msg: &Message, rx: Result<ConnectionMessage, ()>) -> bool {
    if let Ok(rx) = rx {
        debug!("{:?} Rx {:?} into {:?}", ::std::thread::current().id(), rx, msg);
        msg.iter().zip(rx.into_iter()).all(|(oe, ov)| {
            match (oe, ov) {
                (&None, None) => true,
                (&Some(ref e), Some(ref v)) => e.eval_up(&mut |var, val| cx.state.set(var, val), v.clone()),
                _ => false,
            }
        })
    } else {
        false
    }
}

fn run_inner(step: &StepInfo, cx: &mut RunCx) -> bool {
    match step.step {
        Nop => true,
        Token(ref msg) => {
            cx.send_lower(msg).ok();

            let rx = cx.downwards.recv();
            message_match(cx, msg, rx)
        }
        TokenTop(ref msg, ref inner) => {
            let rx = cx.upwards.recv();
            if !message_match(cx, msg, rx) {
                return false;
            }

            if !run_inner(inner, cx) { return false; }

            cx.send_upper(msg).ok();
            true
        }
        Seq(ref steps) => {
            for step in steps {
                if !run_inner(step, cx) { return false; }
            }
            true
        }
        Repeat(ref count, ref inner) => {
            if step.dir.repeat_up_heuristic {
                debug!("repeat up");
                let mut c = 0;

                while cx.test(&inner.first) {
                    if !run_inner(inner, cx) { return false; }
                    c += 1;
                }

                debug!("Repeat end {}", c);
                count.eval_up(&mut |var, val| cx.state.set(var, val), Value::Integer(c))
            } else {
                debug!("repeat down");

                let c = match count.eval_down(&mut |var| cx.state.get(var).clone()) {
                    Value::Integer(i) => i,
                    other => panic!("Count evaluated to non-integer {:?}", other)
                };
                for _ in 0..c {
                    if !run_inner(inner, cx) { return false; }
                }
                true
            }
        }
        Foreach(count, ref assigns, ref inner) => {
            let mut lstate = assigns.iter().map(|&(id, ref expr)| {
                let dir = step.dir.mode_of(id);
                let d = if dir.down {
                    match expr.eval_down(&mut |var| cx.state.get(var).clone()) {
                        Value::Vector(v) => Some(v.into_iter()),
                        other => panic!("For loop argument must be a vector, found {}", other)
                    }
                } else { None };

                let u = if dir.up { Some(Vec::new()) } else { None };

                (id, d, u, expr)
            }).collect::<Vec<_>>();


            for _ in 0..count {
                for &mut (id, ref mut down, _, _) in &mut lstate {
                    if let Some(d) = down {
                        cx.state.set(id, d.next().expect("not enough elements in loop"));
                    }
                }

                if !run_inner(inner, cx) { return false; }

                for &mut (id, _, ref mut up, _) in &mut lstate {
                    if let Some(u) = up {
                        u.push(cx.state.take(id))
                    }
                }
            }

            for (_, _, up, expr) in lstate {
                if let Some(u) = up {
                    if !expr.eval_up(&mut |var, val| cx.state.set(var, val), Value::Vector(u)) { return false; }
                }
            }

            true
        }
        Alt(ref opts) => {
            if step.dir.repeat_up_heuristic {
                for &(ref vals, ref inner) in opts {
                    if cx.test(&inner.first) {
                        for &(ref l, ref r) in vals {
                            let v = l.eval_down(&mut |var| cx.state.get(var).clone());
                            r.eval_up(&mut |var, val| cx.state.set(var, val), v);
                        }

                        return run_inner(inner, cx);
                    }
                }
                false
            } else {
                debug!("alt dn");

                for &(ref vals, ref inner) in opts {
                    let matches = vals.iter().all(|&(ref l, ref r)| {
                        let v = r.eval_down(&mut |var| cx.state.get(var).clone());
                        l.eval_up(&mut |var, val| cx.state.set(var, val), v)
                    });

                    if matches {
                        return run_inner(inner, cx);
                    }
                }
                false
            }
        }
    }
}
