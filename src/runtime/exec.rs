use std::i64;
use scoped_pool::Pool;
use vec_map::VecMap;

use crate::syntax::Value;
use crate::core::{ Step, StepInfo, Message, ProcessInfo, ProcessChain, Process, Type, ValueId, MatchSet, Fields };

use crate::runtime::{ Connection, ConnectionMessage };


pub fn run(processes: &ProcessChain, downwards: &mut Connection, upwards: &mut Connection) -> bool {
    let pool = Pool::new(4);
    let mut cx = RunCx { downwards, upwards, vars_down: State::new(), vars_up: State::new(), threadpool: &pool };
    let result = run_processes(&processes.processes, &mut cx);
    pool.shutdown();
    result
}


struct RunCx<'a> {
    downwards: &'a mut Connection,
    upwards: &'a mut Connection,

    vars_down: State,
    vars_up: State,

    threadpool: &'a Pool,
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
                (&Some(ref e), true) => Some(e.eval_down(&mut |var| self.vars_down.get(var).clone())),
                _ => None
            }
        }).collect();
        debug!("{:?} Send lower {:?}", ::std::thread::current().id(), tx);
        self.downwards.send(tx)
    }

    fn send_upper(&mut self, msg: &Message) -> Result<(), ()> {
        let tx = msg.iter().zip(self.upwards.sends.iter()).map(|(oe, sends)| {
            match (oe, sends) {
                (&Some(ref e), true) => Some(e.eval_down(&mut |var| self.vars_up.get(var).clone())),
                _ => None
            }
        }).collect();
        debug!("{:?} Send upper {:?}", ::std::thread::current().id(), tx);
        self.upwards.send(tx)
    }
}

/// Map of register IDs to values
#[derive(Clone)]
pub struct State {
    registers: VecMap<Value>,
}

impl State {
    pub fn new() -> State {
        State {
            registers: VecMap::new(),
        }
    }

    pub fn get(&self, reg: ValueId) -> &Value {
        debug!("get {}: {:?}", reg, self.registers.get(reg));
        &self.registers[reg]
    }

    pub fn set(&mut self, reg: ValueId, v: Value) -> bool {
        debug!("set {}: {:?}", reg, v);
        self.registers.insert(reg, v);
        true
    }

    pub fn take(&mut self, reg: ValueId) -> Value {
        self.registers.remove(reg).expect("value not set")
    }

    pub fn merge(&mut self, other: State) {
        self.registers.extend(other.registers.into_iter());
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

fn message_match(state: &mut State, msg: &Message, rx: Result<ConnectionMessage, ()>) -> bool {
    if let Ok(rx) = rx {
        debug!("{:?} Rx {:?} into {:?}", ::std::thread::current().id(), rx, msg);
        msg.iter().zip(rx.into_iter()).all(|(oe, ov)| {
            match (oe, ov) {
                (&None, None) => true,
                (&Some(ref e), Some(ref v)) => e.eval_up(&mut |var, val| state.set(var, val), v.clone()),
                _ => false,
            }
        })
    } else {
        false
    }
}

fn run_processes(processes: &[ProcessInfo], cx: &mut RunCx<'_>) -> bool {
    let (child, chain) = match processes.split_first() {
        Some(x) => x,
        None => return true
    };

    if chain.len() == 0 {
        run_process(child, cx)
    } else {
        let (mut conn_u, mut conn_l) = Connection::new(&match &child.shape_up {
            Some(s) => s.fields(),
            None => Fields::null()
        });
        let mut upper_cx = RunCx {
            downwards: &mut conn_u,
            upwards: cx.upwards,
            vars_down: cx.vars_down.clone(),
            vars_up: State::new(),
            threadpool: cx.threadpool
        };
        let mut lower_cx = RunCx {
            downwards: cx.downwards,
            upwards: &mut conn_l,
            vars_down: cx.vars_down.clone(),
            vars_up: State::new(),
            threadpool: cx.threadpool
        };

        let mut ok1 = false;
        let mut ok2 = false;

        cx.threadpool.scoped(|scoped| {
            scoped.execute(|| {
                ok1 = run_processes(chain, &mut upper_cx);
                upper_cx.downwards.end();
                debug!("Fork upper end");
            });
            ok2 = run_process(child, &mut lower_cx);
            lower_cx.upwards.end();
            debug!("Fork lower end");
        });

        debug!("fork join");

        cx.vars_up.merge(upper_cx.vars_up);
        cx.vars_up.merge(lower_cx.vars_up);

        ok1 && ok2
    }
}

fn run_process(process: &ProcessInfo, cx: &mut RunCx<'_>) -> bool {
    match process.process {
        Process::Token(ref msg) => {
            cx.send_lower(msg).ok();
            let rx = cx.downwards.recv();
            message_match(&mut cx.vars_up, msg, rx)
        }
        Process::Seq(ref step) => run_step(step, cx),
        Process::Primitive(ref p) => p.run(cx.downwards, cx.upwards),
    }
}

fn run_step(step: &StepInfo, cx: &mut RunCx<'_>) -> bool {
    use self::Step::*;
    match step.step {
        Nop => true,
        Process(ref p) => run_processes(&p.processes, cx),
        TokenTop(ref msg, ref inner) => {
            let rx = cx.upwards.recv();
            if !message_match(&mut cx.vars_down, msg, rx) {
                return false;
            }

            if !run_step(inner, cx) { return false; }

            cx.send_upper(msg).ok();
            true
        }
        Seq(ref steps) => {
            for step in steps {
                if !run_step(step, cx) { return false; }
            }
            true
        }
        Repeat(ref count, ref inner) => {
            if !count.dir().down {
                debug!("repeat up");
                let mut c = 0;

                let count_type = count.get_type();
                let hi = match count_type {
                    Type::Integer(_, hi) => hi,
                    _ => { warn!("Loop count type is {:?} not int", count_type); i64::MAX }
                };

                while c < hi && cx.test(&inner.first) {
                    if !run_step(inner, cx) { return false; }
                    c += 1;
                }

                debug!("Repeat end {}", c);
                count.eval_up(&mut |var, val| cx.vars_up.set(var, val), Value::Integer(c))
            } else {
                debug!("repeat down");

                let c = match count.eval_down(&mut |var| cx.vars_down.get(var).clone()) {
                    Value::Integer(i) => i,
                    other => panic!("Count evaluated to non-integer {:?}", other)
                };
                for _ in 0..c {
                    if !run_step(inner, cx) { return false; }
                }
                true
            }
        }
        Foreach(count, ref assigns, ref inner) => {
            let mut lstate = assigns.iter().map(|&(id, ref expr)| {
                let dir = expr.dir();
                let d = if dir.down {
                    match expr.eval_down(&mut |var| cx.vars_down.get(var).clone()) {
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
                        cx.vars_down.set(id, d.next().expect("not enough elements in loop"));
                    }
                }

                if !run_step(inner, cx) { return false; }

                for &mut (id, _, ref mut up, _) in &mut lstate {
                    if let Some(u) = up {
                        u.push(cx.vars_up.take(id))
                    }
                }
            }

            for (_, _, up, expr) in lstate {
                if let Some(u) = up {
                    if !expr.eval_up(&mut |var, val| cx.vars_up.set(var, val), Value::Vector(u)) { return false; }
                }
            }

            true
        }
        Alt(ref opts) => {
            let up = opts.iter().any(|arm| {
                arm.0.is_empty() || arm.0.iter().any(|&(_, ref e)| !e.dir().down)
            });
            
            if up {
                for &(ref vals, ref inner) in opts {
                    if cx.test(&inner.first) {
                        for &(ref l, ref r) in vals {
                            let v = l.eval_down(&mut |var| cx.vars_up.get(var).clone());
                            r.eval_up(&mut |var, val| cx.vars_up.set(var, val), v);
                        }

                        return run_step(inner, cx);
                    }
                }
                false
            } else {
                debug!("alt dn");

                for &(ref vals, ref inner) in opts {
                    let matches = vals.iter().all(|&(ref l, ref r)| {
                        let v = r.eval_down(&mut |var| cx.vars_down.get(var).clone());
                        l.eval_up(&mut |var, val| cx.vars_down.set(var, val), v)
                    });

                    if matches {
                        return run_step(inner, cx);
                    }
                }
                false
            }
        }
    }
}
