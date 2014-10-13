use std::comm;

use resolve::scope::ValueRef;
use ast::Value;
use eval;

pub trait PrimitiveStep {
    fn display(&self) -> String;
    fn body<'a>(&'a self) -> Option<&'a Step> { None }
    fn exec(&self);
}

#[deriving(Show)]
pub enum Message {
    MessageValue(/*down*/ ValueRef, /*up*/ ValueRef),
    MessageTuple(Vec<Message>),
}

pub enum Step {
    NopStep,
    TokenStep(eval::Ops, Message),
    TokenTopStep(eval::Ops, Message, Box<Step>),
    SeqStep(Vec<Step>),
    RepeatStep(Box<Step>),
    //PrimitiveStep(Box<PrimitiveStep>),
}

fn first(s: &Step) -> Option<(&eval::Ops, &Message)> {
    match *s {
        NopStep => None,
        TokenStep(ref ops, ref message) => Some((ops, message)),
        TokenTopStep(_, _, box ref body) => first(body),
        SeqStep(ref steps) => steps.as_slice().get(0).and_then(first),
        RepeatStep(box ref inner) => first(inner),
    }
}

pub fn print_step_tree(s: &Step, indent: uint) {
    let i = " ".repeat(indent);
    match *s {
        NopStep => println!("{}NOP", i),
        TokenStep(_, ref message) => {
            println!("{}Token: {}", i, message);
        }
        TokenTopStep(_, ref message, box ref body) => {
            println!("{}Up: {}", i, message);
            print_step_tree(body, indent+1);
        }
        SeqStep(ref steps) => {
            println!("{}Seq", i)
            for c in steps.iter() {
                print_step_tree(c, indent+1);
            }
        }
        RepeatStep(box ref inner) => {
            println!("{}Repeat:", i);
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
    rx: comm::Receiver<Vec<Value>>,
    tx: comm::Sender<Vec<Value>>,

    lookahead_tx: Option<Vec<Value>>,
    lookahead_rx: Option<Vec<Value>>,
}

impl Connection {
    pub fn new() -> (Connection, Connection) {
        let (s1, r1) = comm::channel();
        let (s2, r2) = comm::channel();
        (Connection{ tx: s1, rx: r2, lookahead_tx: None, lookahead_rx: None },
         Connection{ tx: s2, rx: r1, lookahead_tx: None, lookahead_rx: None })
    }

    pub fn send(&self, v: Vec<Value>) -> Result<(), Vec<Value>> {
        self.tx.send_opt(v)
    }

    pub fn recv(&self) -> Result<Vec<Value>, ()> {
        self.rx.recv_opt()
    }

    pub fn lookahead_send(&mut self, v: Vec<Value>) {
        match self.lookahead_tx {
            Some(ref lv) => {
                if v != *lv { fail!("Committed {}, but sending {}", lv, v); }
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
                        debug!("recv: {}", r);
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
    debug!("tokenstep {} {}", ops, msg);
    state.enter(ops);
    let m = state.tx_message(msg);
    debug!("  down: {}", m);
    parent.lookahead_send(m);

    match parent.lookahead_receive() {
        Some(m) => {
            debug!("  up: {}", m);
            state.rx_message(msg, m);
            state.exit(ops)
        }
        None => false
    }
}

pub fn exec(state: &mut eval::State, s: &Step, parent: &mut Connection, child: &mut Connection) -> bool {
    match *s {
        NopStep => true,
        TokenStep(ref ops, ref msg) => {
            let r = try_token(state, parent, ops, msg);
            if r {
                debug!("  matched");
                parent.accept();
            }
            r
        }
        TokenTopStep(ref ops, ref msg, box ref body) => {
            match child.recv() {
                Ok(m) => {
                    debug!("tokentop: {}, {}", ops, msg);
                    debug!("down: {}", m)
                    state.rx_message(msg, m);
                    state.enter(ops);

                    let r = exec(state, body, parent, child);

                    state.exit(ops);
                    let m = state.tx_message(msg);
                    debug!("up: {}", m);
                    if child.send(m).is_err() { return false; }
                    r
                }
                Err(..) => false
            }
        }
        SeqStep(ref steps) => {
            for c in steps.iter() {
                match exec(state, c, parent, child) {
                    true => (),
                    false => return false,
                }
            }
            true
        }
        RepeatStep(box ref inner) => {
            let (ops, msg) = first(inner).expect("Loop has no body");
            loop {
                if !try_token(state, parent, ops, msg) {
                    debug!("  loop exit");
                    break;
                }
                if !exec(state, inner, parent, child) {
                    return false;
                }
            }
            true
        }
        //PrimitiveStep(..) => unimplemented!()
    }
}
