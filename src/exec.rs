use std::sync::mpsc::{Receiver, Sender, channel};
use std::iter::repeat;

use data::{ Value, DataMode, Type, Shape };
use session::ValueID;
use eval::{ self, Expr };

pub trait PrimitiveStep {
    fn display(&self) -> String;
    fn body<'a>(&'a self) -> Option<&'a Step> { None }
    fn exec(&self);
}

pub type MessageTag = usize;

#[derive(Debug, Clone)]
pub struct Message {
    pub tag: MessageTag,
    pub components: Vec<Expr>
}

#[derive(Debug)]
pub enum Step {
    Nop,
    Token(Message),
    TokenTop(Message, Box<Step>),
    Seq(Vec<Step>),
    Repeat(Expr, Box<Step>, bool),
    Foreach(u32, Vec<(ValueID, Expr, DataMode)>, Box<Step>)
    //PrimitiveStep(Box<PrimitiveStep>),
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
            for &(id, ref expr, dir) in vars { print!("{}={:?} {:?}, ", id, expr, dir); }
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

pub type ConnectionMessage = (MessageTag, Vec<Value>);

pub struct Connection {
    types: Vec<Vec<(Type, /*tx*/ bool, /* rx */ bool)>>,

    rx: Option<Receiver<ConnectionMessage>>,
    tx: Option<Sender<ConnectionMessage>>,

    lookahead_tx: Option<ConnectionMessage>,
    lookahead_rx: Option<ConnectionMessage>,

    pub alive: bool,
}

/// Transfers data between two stacked abstractions
impl Connection {
    pub fn new(shape: &Shape) -> (Connection, Connection) {
        let direction = shape.data_mode();
        debug!("New connection: {:?} {:?}", shape, direction);

        let (s1, r1) = if direction.down {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let (s2, r2) = if direction.up {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let alive = direction.down || direction.up;

        let (t1, t2) = shape.variants.iter().map(|variant| (
            variant.values().map(|(t, d)| (t.clone(), d.down, d.up)).collect(),
            variant.values().map(|(t, d)| (t.clone(), d.up, d.down)).collect()
        )).unzip();

        (Connection{ types: t1, tx: s1, rx: r2, lookahead_tx: None, lookahead_rx: None, alive: alive },
         Connection{ types: t2, tx: s2, rx: r1, lookahead_tx: None, lookahead_rx: None, alive: alive })
    }

    pub fn send(&mut self, v: ConnectionMessage) -> Result<(), ()> {
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

    pub fn recv(&mut self) -> Result<ConnectionMessage, ()> {
        if let Some(ref rx) = self.rx {
            let r = rx.recv().map_err(|_| ());
            if r.is_err() {
                self.alive = false;
            }
            r
        } else {
            if self.alive { Ok((0, vec![])) } else { Err(()) }
        }
    }

    pub fn lookahead_send(&mut self, v: ConnectionMessage) {
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

    pub fn lookahead_receive(&mut self) -> Option<ConnectionMessage> {
        match self.lookahead_rx {
            Some(ref lv) => Some(lv.clone()),
            None => {
                match self.recv() {
                    Ok(r) => {
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

    pub fn lookahead_eval_down(&mut self, state: &eval::State, message: &Message) {
        let m = self.types[message.tag].iter().zip(message.components.iter())
            .filter(|&(&(_, tx, _), _)| tx)
            .map(|(_, ref e)| e.eval_down(state))
            .collect();
        debug!("tx: {}:{:?}", message.tag, m);
        self.lookahead_send((message.tag, m));
    }

    pub fn lookahead_eval_up(&mut self, state: &mut eval::State, message: &Message) -> bool {
        let (tag, m) = match self.lookahead_receive() {
            Some((tag, m)) => (tag, m),
            None => return false,
        };
        debug!("rx: {}:{:?}", tag, m);

        if self.rx.is_some() && message.tag != tag {
            return false;
        }

        self.types[tag].iter().zip(message.components.iter())
            .filter(|&(&(_, _, rx), _)| rx)
            .map(|(_, e)| e)
            .zip(m.into_iter())
            .map(|(e, v)| e.eval_up(state, v))
            .fold(true, |a, b| { a && b })
    }
}

pub fn try_token(state: &mut eval::State, parent: &mut Connection, msg: &Message) -> bool {
    debug!("tokenstep {:?}", msg);
    parent.lookahead_eval_down(state, msg);
    parent.lookahead_eval_up(state, msg)
}

/// Proof-of-concept direct execution of a Step tree
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

            if child.lookahead_eval_up(state, msg) {
                child.accept();
                let r = exec(state, body, parent, child);
                child.lookahead_eval_down(state, msg);
                child.accept();
                r
            } else {
                false
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
                            Step::TokenTop(ref msg, box ref body) => {
                                if let Some((tag, _)) = child.lookahead_receive() {
                                    // TODO: variables in the block will not be assigned
                                    // so we can't evaluate the child expression if they might be used.
                                    // This works for unidirectional applications only.
                                    let down = parent.tx.is_some();
                                    parent.alive && child.alive && (!child.rx.is_some() || tag == msg.tag) &&
                                        (down || try_first(state, body, parent, child))
                                } else {
                                    false
                                }
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
            let mut lstate = vars.iter().map(|&(id, ref expr, ref dir)| {
                debug!("foreach var {}, {:?} {:?}", id, expr, dir);
                let d = if dir.down {
                    match expr.eval_down(state) {
                        Value::Vector(v) => Some(v.into_iter()),
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
