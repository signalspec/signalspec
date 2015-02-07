use std::sync::mpsc::{Receiver, Sender, channel};
use std::iter::repeat;

use resolve::scope::{ValueRef, Dynamic};
use resolve::context::ValueID;
use resolve::types::Shape;
use ast::Value;
use eval;

pub trait PrimitiveStep {
    fn display(&self) -> String;
    fn body<'a>(&'a self) -> Option<&'a Step> { None }
    fn exec(&self);
}

pub type ValuePair = (/*down*/ ValueRef, /*up*/ ValueRef);

#[derive(Debug)]
pub enum Message {
    Value(/*down*/ ValueRef, /*up*/ ValueRef),
    Tuple(Vec<Message>),
}

impl Message {
    // TODO(rust #18835): shouldn't need &mut; see https://github.com/rust-lang/rust/pull/20578
    pub fn each_down_ref(&self, f: &mut FnMut(ValueID)) {
        match *self {
            Message::Value(Dynamic(id), _) => f(id),
            Message::Value(_, _) => (),
            Message::Tuple(ref children) => {
                for i in children.iter() { i.each_down_ref(f) }
            }
        }
    }

    pub fn each_up_ref(&self, f: &mut FnMut(ValueID)) {
        match *self {
            Message::Value(_, Dynamic(id)) => f(id),
            Message::Value(_, _) => (),
            Message::Tuple(ref children) => {
                for i in children.iter() { i.each_up_ref(f) }
            }
        }
    }
}

#[derive(Debug)]
pub enum Step {
    Nop,
    Token(eval::Ops, Message),
    TokenTop(eval::Ops, Message, Box<Step>),
    Seq(Vec<Step>),
    Repeat(ValuePair, Box<Step>),
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
        Step::Token(_, ref message) => {
            println!("{}Token: {:?}", i, message);
        }
        Step::TokenTop(_, ref message, box ref body) => {
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
    pub fn new(s: &Shape) -> (Connection, Connection) {
        let (is_down, is_up) = s.contains_direction();

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

pub fn try_token(state: &mut eval::State, parent: &mut Connection,
                   ops: &eval::Ops, msg: &Message) -> bool {
    debug!("tokenstep {:?} {:?}", ops, msg);
    state.enter(ops);

    let mut m = Vec::new();
    msg.each_down_ref(&mut |id| m.push(state.get(id).clone()) );

    debug!("  down: {:?}", m);
    parent.lookahead_send(m);

    match parent.lookahead_receive() {
        Some(m) => {
            debug!("  up: {:?}", m);

            let mut iter = m.into_iter();
            // TODO: replace the dummy value with .expect("Not enough values in message")
            msg.each_up_ref(&mut |id| {
                state.set(id, iter.next().unwrap_or(Value::Number(0.)));
            });

            state.exit(ops)
        }
        None => false
    }
}

pub fn exec(state: &mut eval::State, s: &Step, parent: &mut Connection, child: &mut Connection) -> bool {
    match *s {
        Step::Nop => true,
        Step::Token(ref ops, ref msg) => {
            let r = try_token(state, parent, ops, msg);
            if r {
                debug!("  matched");
                parent.accept();
            }
            r
        }
        Step::TokenTop(ref ops, ref msg, box ref body) => {
            match child.recv() {
                Ok(m) => {
                    debug!("tokentop: {:?}, {:?}", ops, msg);
                    debug!("down: {:?}", m);

                    let mut iter = m.into_iter();
                    // TODO: replace the dummy value with .expect("Not enough values in message")
                    msg.each_down_ref(&mut |id| {
                        state.set(id, iter.next().unwrap_or(Value::Number(0.)));
                    });

                    state.enter(ops);

                    let r = exec(state, body, parent, child);

                    state.exit(ops);

                    let mut m = Vec::new();
                    msg.each_up_ref(&mut |id| m.push(state.get(id).clone()) );

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
        Step::Repeat((_cd, cu), box ref inner) => {
            let f = first(inner).expect("Loop has no body");
            let mut count = 0;
            loop {
                match *f {
                    Step::Token(ref ops, ref msg) => {
                        if !try_token(state, parent, ops, msg) {
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
            if let Dynamic(id) = cu {
                state.set(id, Value::Integer(count));
            }
            true
        }
        //PrimitiveStep(..) => unimplemented!()
    }
}
