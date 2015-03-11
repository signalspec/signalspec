use std::sync::mpsc::{Receiver, Sender, channel};
use std::iter::repeat;

use session::ValueID;
use resolve::types::Shape;
use ast::Value;
use eval::{ self, Expr };

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
    Repeat(Expr, Box<Step>),
    //PrimitiveStep(Box<PrimitiveStep>),
}

fn first(s: &Step) -> Option<&Step> {
    match *s {
        Step::Nop => None,
        Step::Token(..) => Some(s),
        Step::TokenTop(..) => Some(s),
        Step::Seq(ref steps) => steps.as_slice().get(0).and_then(first),
        Step::Repeat(_, box ref inner) => first(inner),
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
        Step::Repeat(ref count, box ref inner) => {
            println!("{}Repeat: {:?}", i, count);
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
    pub fn new() -> (Connection, Connection) {
        let (is_down, is_up) = (true, true);

        let (s1, r1) = if is_down {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let (s2, r2) = if is_up {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let alive = is_up || is_down;

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
            assert!(v.len() == 0);
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
}

pub fn try_token(state: &mut eval::State, parent: &mut Connection, msg: &Message) -> bool {
    debug!("tokenstep {:?}", msg);
    let mut m = msg.eval_down(state);

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
            match child.recv() {
                Ok(m) => {
                    debug!("down: {:?}", m);

                    msg.eval_up(state, m);
                    let r = exec(state, body, parent, child);
                    let mut m = msg.eval_down(state);

                    debug!("up: {:?}", m);
                    if child.send(m).is_err() { return false; }
                    r
                }
                Err(..) => false
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
        Step::Repeat(ref count_expr, box ref inner) => {
            let f = first(inner).expect("Loop has no body");
            let mut count = 0;
            loop {
                match *f {
                    Step::Token(ref msg) => {
                        if !try_token(state, parent, msg) {
                            debug!("  loop exit");
                            break;
                        }
                    }
                    Step::TokenTop(..) => {
                        if !parent.alive || !child.alive {
                            debug!("  loop done");
                            break;
                        }
                    },
                    _ => panic!(),
                }
                if !exec(state, inner, parent, child) {
                    return false;
                }
                count += 1;
            }

            count_expr.eval_up(state, Value::Integer(count))
        }
        //PrimitiveStep(..) => unimplemented!()
    }
}
