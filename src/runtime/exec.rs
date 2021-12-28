use std::i64;
use std::vec::IntoIter;
use scoped_pool::Pool;
use vec_map::VecMap;

use crate::{Shape, syntax::Value};
use crate::core::{Expr, ExprDn, MatchSet, ProcessChain, Step, StepInfo, Type, ValueId};

use crate::runtime::{ Connection, ConnectionMessage };


pub fn run(processes: &ProcessChain, downwards: &mut Connection, upwards: &mut Connection) -> bool {
    let pool = Pool::new(4);
    let mut cx = RunCx { downwards, upwards, vars_down: State::new(), vars_up: State::new(), threadpool: &pool };
    let result = run_step(&processes.step, &mut cx);
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
        match &m {
            MatchSet::None | MatchSet::Process => true,
            MatchSet::MessageUp { receive} => {
                if let Ok(rx) = self.upwards.peek() {
                    receive.iter().any(|&(variant, ref dn)|  message_test(variant, &dn,  rx) )
                } else { false }
            }
            MatchSet::MessageDn { variant, receive, ..} => {
                if let Ok(rx) = self.downwards.peek() {
                    receive.iter().any(|up|  message_test(*variant, &up, rx) )
                } else { false }
            }
        }
    }

    fn send_lower(&mut self, variant: usize, dn: &Vec<ExprDn>) -> Result<(), ()> {
        let tx = dn.iter().map(|e| {
            e.eval(&mut |var| self.vars_down.get(var).clone())
        }).collect();
        debug!("{:?} Send lower {} {:?}", ::std::thread::current().id(), variant, tx);
        self.downwards.send(ConnectionMessage { variant: variant, values: tx })
    }

    fn send_upper(&mut self, variant: usize, up: &Vec<ExprDn>) -> Result<(), ()> {
        let tx = up.iter().map(|e| {
            e.eval(&mut |var| self.vars_up.get(var).clone())
        }).collect();
        debug!("{:?} Send upper {} {:?}", ::std::thread::current().id(), variant, tx);
        self.upwards.send(ConnectionMessage { variant: variant, values: tx })
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

fn message_test(msg_variant: usize, exprs: &[Expr], rx: &ConnectionMessage) -> bool {
    debug!("{:?} Test {:?} into {} {:?}", ::std::thread::current().id(), rx, msg_variant, exprs);

    if rx.variant == !0 {
        assert_eq!(exprs.len(), 0);
        return true;
    }

    if rx.variant != msg_variant { 
        debug!("{:?} wrong variant", ::std::thread::current().id());
        return false
    };

    exprs.iter().zip(rx.values.iter()).all(|(e, v)| {
        e.eval_up(&mut |_, _| true, v.clone())
    })
}

fn message_match(state: &mut State, msg_variant: usize, exprs: &[Expr], rx: Result<ConnectionMessage, ()>) -> bool {
    if let Ok(rx) = rx {
        debug!("{:?} Rx {:?} into {} {:?}", ::std::thread::current().id(), rx, msg_variant, exprs);

        if rx.variant == !0 {
            assert_eq!(exprs.len(), 0);
            return true;
        }

        if rx.variant != msg_variant { 
            debug!("{:?} wrong variant", ::std::thread::current().id());
            return false
        };

        exprs.iter().zip(rx.values.into_iter()).all(|(e, v)| {
            e.eval_up(&mut |var, val| state.set(var, val), v.clone())
        })
    } else {
        false
    }
}

fn run_processes(cx: &mut RunCx<'_>, steps: &[StepInfo], shapes:&[Shape]) -> bool {
    let (child, rest) = match steps.split_first() {
        Some(x) => x,
        None => return true
    };

    if rest.len() == 0 {
        run_step(child, cx)
    } else {
        let (shape_up, shape_rest) = shapes.split_first().unwrap();

        let (mut conn_u, mut conn_l) = Connection::new(shape_up.direction());
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
                ok1 = run_processes(&mut upper_cx, rest, shape_rest);
                upper_cx.downwards.end();
                debug!("Fork upper end");
            });
            ok2 = run_step(child, &mut lower_cx);
            lower_cx.upwards.end();
            debug!("Fork lower end");
        });

        debug!("fork join");

        cx.vars_up.merge(upper_cx.vars_up);
        cx.vars_up.merge(lower_cx.vars_up);

        ok1 && ok2
    }
}

fn run_step(step: &StepInfo, cx: &mut RunCx<'_>) -> bool {
    use self::Step::*;
    match &step.step {
        Chain(ref steps, ref shapes) => run_processes(cx, steps, shapes),
        Primitive(p) => p.run(cx.downwards, cx.upwards),
        Token { variant, send: dn, receive: up, ..}=> {
            cx.send_lower(*variant, dn).ok();
            let rx = cx.downwards.recv();
            message_match(&mut cx.vars_up, *variant, up, rx)
        }
        TokenTop { variant, send: dn, receive: up, inner } => {
            let rx = cx.upwards.recv();
            if !message_match(&mut cx.vars_down, *variant, dn, rx) {
                return false;
            }

            if !run_step(inner, cx) { return false; }

            cx.send_upper(*variant, up).ok();
            true
        }
        Seq(ref steps) => {
            for step in steps {
                if !run_step(step, cx) { return false; }
            }
            true
        }
        RepeatUp(ref count, ref inner) => {
            debug!("repeat up {:?}", inner.first);
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
        }
        RepeatDn(ref count, ref inner) => {
            debug!("repeat down");

            let c = match count.eval(&mut |var| cx.vars_down.get(var).clone()) {
                Value::Integer(i) => i,
                other => panic!("Count evaluated to non-integer {:?}", other)
            };
            for _ in 0..c {
                if !run_step(inner, cx) { return false; }
            }
            true
        }

        Foreach { iters, ref vars_dn, ref vars_up, ref inner} => {
            let mut state_dn: Vec<(ValueId, IntoIter<Value>)> = vars_dn.iter().map(|&(id, ref e)| {
                let d = match e.eval(&mut |var| cx.vars_down.get(var).clone()) {
                    Value::Vector(v) => v.into_iter(),
                    other => panic!("For loop argument must be a vector, found {}", other)
                };
                (id, d)
            }).collect();

            let mut state_up: Vec<(ValueId, Vec<Value>, &Expr)> = vars_up.iter().map(|&(id, ref e)|
                (id, Vec::new(), e)
            ).collect();

            for _ in 0..*iters {
                for &mut (id, ref mut d) in &mut state_dn {
                    cx.vars_down.set(id, d.next().expect("not enough elements in loop"));
                }

                if !run_step(inner, cx) { return false; }

                for &mut (id, ref mut u, _) in &mut state_up {
                    u.push(cx.vars_up.take(id))
                }
            }

            for (_, u, e) in state_up {
                if !e.eval_up(&mut |var, val| cx.vars_up.set(var, val), Value::Vector(u)) { return false; }
            }

            true
        }
        AltUp(ref opts) => {
            debug!("alt up");

            for &(ref vals, ref inner) in opts {
                if cx.test(&inner.first) {
                    for &(ref l, ref r) in vals {
                        let v = l.eval(&mut |var| cx.vars_up.get(var).clone());
                        r.eval_up(&mut |var, val| cx.vars_up.set(var, val), v);
                    }

                    return run_step(inner, cx);
                }
            }
            false
        }
        AltDn(ref opts) => {
            debug!("alt dn");

            for &(ref vals, ref inner) in opts {
                let matches = vals.iter().all(|&(ref l, ref r)| {
                    let v = r.eval(&mut |var| cx.vars_down.get(var).clone());
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
