use std::sync::mpsc::{Receiver, Sender, channel};
use std::iter::repeat;

use session::ValueID;
use ast::Value;
use eval::{ self, Expr, DataMode };

pub trait PrimitiveStep {
    fn display(&self) -> String;
    fn body<'a>(&'a self) -> Option<&'a Step> { None }
    fn exec(&self);
}

#[derive(Debug)]
pub struct Message {
    pub components: Vec<Expr>
}

impl Message {
    pub fn eval_down(&self, state: &eval::State) -> Vec<Value> {
        self.components.iter().map(|e| {
            if e.exists_down() {
                e.eval_down(state)
            } else {
                Value::Number(0.0)
            }
        }).collect()
    }

    pub fn eval_up(&self, state: &mut eval::State, values: Vec<Value>) -> bool {
        debug!("eval_up: {:?}", self);
        let mut matched = true;
        let mut vi = values.into_iter().fuse();
        for e in &self.components {
            let v = vi.next().unwrap_or(Value::Number(0.0));
            matched &= e.eval_up(state, v);
        }
        matched
    }
}

#[derive(Debug)]
pub enum Step {
    Nop,
    Token(Message),
    TokenTop(Message, Box<Step>),
    Seq(Vec<Step>),
    Repeat(Expr, Box<Step>, bool),
    Foreach(u32, Vec<(ValueID, Expr)>, Box<Step>)
    //PrimitiveStep(Box<PrimitiveStep>),
}

impl Step {
    /// Returns true if any data is sent in the up direction in this step or its lexical children
    pub fn any_up(&self) -> bool {
        match *self {
            Step::Nop => false,
            Step::Token(ref m) => m.components.iter().any(Expr::exists_up),
            Step::Seq(ref steps) => steps.iter().any(Step::any_up),
            Step::TokenTop(_, box ref _c) => true, //TODO: works on simple cases, but is this the right heuristic?
            Step::Repeat(_, box ref c, _) | Step::Foreach(_, _, box ref c) => c.any_up(),
        }
    }
}

pub fn print_step_tree(s: &Step, indent: u32) {
    let i: String = repeat(" ").take(indent as usize).collect();
    match *s {
        Step::Nop => println!("{}NOP", i),
        Step::Token(ref message) => {
            println!("{}Token: {:?}", i, message);
        }
        Step::TokenTop(ref message, box ref body) => {
            println!("{}Up: {:?}", i, message);
            print_step_tree(body, indent+1);
        }
        Step::Seq(ref steps) => {
            println!("{}Seq", i);
            for c in steps.iter() {
                print_step_tree(c, indent+1);
            }
        }
        Step::Repeat(ref count, box ref inner, up) => {
            println!("{}Repeat: {:?} {}", i, count, up);
            print_step_tree(inner, indent + 1);
        }
        Step::Foreach(width, ref vars, box ref inner) => {
            print!("{}For: {} ", i, width);
            for &(id, ref expr) in vars { print!("{}={:?}, ", id, expr); }
            println!("");
            print_step_tree(inner, indent + 1);
        }
        /*PrimitiveStep(ref h) => {
            println!("{}{}", i, h.display());
            h.body().map(|body| {
                print_step_tree(body, indent+1)
            });
        }*/
    }
}

pub struct Connection {
    rx: Option<Receiver<Vec<Value>>>,
    tx: Option<Sender<Vec<Value>>>,

    lookahead_tx: Option<Vec<Value>>,
    lookahead_rx: Option<Vec<Value>>,

    pub alive: bool,
}

impl Connection {
    pub fn new(direction: DataMode) -> (Connection, Connection) {
        debug!("New connection: {:?}", direction);
        let (s1, r1) = if direction.down {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let (s2, r2) = if direction.up {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let alive = direction.down || direction.up;

        (Connection{ tx: s1, rx: r2, lookahead_tx: None, lookahead_rx: None, alive: alive },
         Connection{ tx: s2, rx: r1, lookahead_tx: None, lookahead_rx: None, alive: alive })
    }

    pub fn send(&mut self, v: Vec<Value>) -> Result<(), ()> {
        if let Some(ref tx) = self.tx {
            let r = tx.send(v).map_err(|_| ());
            if r.is_err() {
                self.alive = false;
            }
            r
        } else {
            //assert!(v.len() == 0);
            if self.alive { Ok(()) } else { Err(()) }
        }
    }

    pub fn recv(&mut self) -> Result<Vec<Value>, ()> {
        if let Some(ref rx) = self.rx {
            let r = rx.recv().map_err(|_| ());
            if r.is_err() {
                self.alive = false;
            }
            r
        } else {
            if self.alive { Ok(vec![]) } else { Err(()) }
        }
    }

    pub fn lookahead_send(&mut self, v: Vec<Value>) {
        match self.lookahead_tx {
            Some(ref lv) => {
                if v != *lv { panic!("Committed {:?}, but sending {:?}", lv, v); }
            }
            None => {
                match self.send(v.clone()) {
                    Ok(..) => {
                        self.lookahead_tx = Some(v);
                    },
                    Err(..) => (),
                }
            }
        }
    }

    pub fn lookahead_receive(&mut self) -> Option<Vec<Value>> {
        match self.lookahead_rx {
            Some(ref lv) => Some(lv.clone()),
            None => {
                match self.recv() {
                    Ok(r) => {
                        debug!("recv: {:?}", r);
                        self.lookahead_rx = Some(r.clone());
                        Some(r)
                    },
                    Err(..) => None,
                }
            }
        }
    }

    pub fn accept(&mut self) {
        self.lookahead_rx.take();
        self.lookahead_tx.take();
    }

    pub fn can_tx(&self) -> bool { self.tx.is_some() }
    pub fn can_rx(&self) -> bool { self.rx.is_some() }
}

pub fn try_token(state: &mut eval::State, parent: &mut Connection, msg: &Message) -> bool {
    debug!("tokenstep {:?}", msg);
    let m = msg.eval_down(state);

    debug!("  down: {:?}", m);
    parent.lookahead_send(m);

    match parent.lookahead_receive() {
        Some(m) => {
            debug!("  up: {:?}", m);
            msg.eval_up(state, m)
        }
        None => false
    }
}

pub fn exec(state: &mut eval::State, s: &Step, parent: &mut Connection, child: &mut Connection) -> bool {
    match *s {
        Step::Nop => true,
        Step::Token(ref msg) => {
            let r = try_token(state, parent, msg);
            if r {
                debug!("  matched");
                parent.accept();
            }
            r
        }
        Step::TokenTop(ref msg, box ref body) => {
            debug!("tokentop: {:?}", msg);
            match child.lookahead_receive() {
                Some(m) => {
                    debug!("down: {:?}", m);
                    child.accept();

                    msg.eval_up(state, m);
                    let r = exec(state, body, parent, child);
                    let m = msg.eval_down(state);

                    debug!("up: {:?}", m);
                    if child.send(m).is_err() { return false; }
                    r
                }
                None(..) => false
            }
        }
        Step::Seq(ref steps) => {
            for c in steps.iter() {
                match exec(state, c, parent, child) {
                    true => (),
                    false => return false,
                }
            }
            true
        }
        Step::Repeat(ref count_expr, box ref inner, up) => {
            if up {
                let mut count = 0;
                loop {
                    fn try_first(state: &mut eval::State, f: &Step, parent: &mut Connection, child: &mut Connection) -> bool {
                        match *f {
                            Step::Nop => true,
                            Step::Token(ref msg) => {
                                try_token(state, parent, msg)
                            }
                            Step::TokenTop(_, box ref body) => {
                                child.lookahead_receive();
                                // TODO: variables in the block will not be assigned
                                // so we can't evaluate the child expression if they might be used.
                                // This works for unidirectional applications only.
                                let down = parent.tx.is_some();
                                parent.alive && child.alive &&
                                    (down || try_first(state, body, parent, child))
                            },
                            Step::Seq(ref steps) => steps.get(0)
                                .map_or(true, |x| try_first(state, x, parent, child)),
                            Step::Repeat(_, box ref inner, _) =>
                                try_first(state, inner, parent, child),
                            Step::Foreach(_, _, box ref inner) => {
                                parent.tx.is_some() || try_first(state, inner, parent, child)
                            }
                        }
                    }

                    if !try_first(state, inner, parent, child) {
                        debug!("  loop done");
                        break;
                    }

                    if !exec(state, inner, parent, child) {
                        debug!("loop up early exit");
                        return false;
                    }
                    count += 1;
                }
                debug!("loop up exit {}", count);
                count_expr.eval_up(state, Value::Integer(count))
            } else {
                let count = match count_expr.eval_down(state) {
                    Value::Integer(count) => count,
                    other => panic!("Count must be an integer, found {}", other),
                };

                for _ in 0..count {
                    if !exec(state, inner, parent, child) {
                        debug!("loop down early exit");
                        return false;
                    }
                }

                debug!("loop down exit {}", count);
                true
            }
        }
        Step::Foreach(width, ref vars, box ref inner) => {
            let mut lstate = vars.iter().map(|&(id, ref expr)| {
                let d = if expr.exists_down() {
                    match expr.eval_down(state) {
                        Value::Vector(v) => Some(v.into_iter()),
                        Value::Number(0.0) => None, //TODO: placeholder value because exists_down doesn't work on variables
                        other => panic!("For loop argument must be a vector, found {}", other)
                    }
                } else { None };
                (id, d, Vec::new(), expr)
            }).collect::<Vec<_>>();

            for i in 0..width {
                for &mut (id, ref mut down, _, _) in &mut lstate {
                    let d = down.as_mut().map_or(Some(Value::Integer(0)), |x| x.next());
                    debug!("Foreach {} {}, {:?}", i, id, d);
                    state.set(id, d.unwrap());
                }
                if !exec(state, inner, parent, child) { return false; }
                for &mut (id, _, ref mut up, _) in &mut lstate {
                    up.push(state.get(id).clone())
                }
            }

            for (_, _, up, expr) in lstate {
                if !expr.eval_up(state, Value::Vector(up)) { return false; }
            }

            true
        }
        //PrimitiveStep(..) => unimplemented!()
    }
}
