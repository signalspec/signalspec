use std::comm;

use resolve::context::ValueID;
use ast::Value;
use eval;

pub trait PrimitiveStep {
    fn display(&self) -> String;
    fn body<'a>(&'a self) -> Option<&'a Step> { None }
    fn exec(&self);
}

pub enum Step {
    NopStep,
    TokenStep(eval::Ops, /*down*/ Vec<ValueID>, /*up*/ Vec<ValueID>),
    TokenTopStep(eval::Ops, /*down*/ Vec<ValueID>, /*up*/ Vec<ValueID>, Box<Step>),
    SeqStep(Vec<Step>),
    RepeatStep(Box<Step>),
    //PrimitiveStep(Box<PrimitiveStep>),
}

fn first(s: &Step) -> Option<(&eval::Ops, &[ValueID], &[ValueID])> {
    match *s {
        NopStep => None,
        TokenStep(ref ops, ref down, ref up) => Some((ops, down.as_slice(), up.as_slice())),
        TokenTopStep(_, _, _, box ref body) => first(body),
        SeqStep(ref steps) => steps.as_slice().get(0).and_then(first),
        RepeatStep(box ref inner) => first(inner),
    }
}

pub fn print_step_tree(s: &Step, indent: uint) {
    let i = " ".repeat(indent);
    match *s {
        NopStep => println!("{}NOP", i),
        TokenStep(_, ref down, ref up) => {
            println!("{}Token: {} {}", i, down, up);
        }
        TokenTopStep(_, ref down, ref up, box ref body) => {
            println!("{}Up: {} {}", i, down, up);
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
                   ops: &eval::Ops, down: &[ValueID], up: &[ValueID]) -> bool {
    debug!("tokenstep {} d:{} u:{}", ops, down, up);
    state.enter(ops);
    let m = state.tx_message(down);
    debug!("  down: {} {}", down, m);
    parent.lookahead_send(m);

    match parent.lookahead_receive() {
        Some(m) => {
            debug!("  up: {} {}", up, m);
            state.rx_message(up, m);
            state.exit(ops)
        }
        None => false
    }
}

pub fn exec(state: &mut eval::State, s: &Step, parent: &mut Connection, child: &mut Connection) -> bool {
    match *s {
        NopStep => true,
        TokenStep(ref ops, ref down, ref up) => {
            let r = try_token(state, parent, ops, down[], up[]);
            if r {
                debug!("  matched");
                parent.accept();
            }
            r
        }
        TokenTopStep(ref ops, ref down, ref up, box ref body) => {
            match child.recv() {
                Ok(m) => {
                    debug!("tokentop: {}, d:{} u:{}", ops, down[], up[]);
                    debug!("down: {} {}", down[], m)
                    state.rx_message(down[], m);
                    state.enter(ops);

                    let r = exec(state, body, parent, child);

                    state.exit(ops);
                    let m = state.tx_message(up[]);
                    debug!("up: {} {}", up[], m);
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
            let (ops, down, up) = first(inner).expect("Loop has no body");
            loop {
                if !try_token(state, parent, ops, down, up) {
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
