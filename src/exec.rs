use std::sync::mpsc::{Receiver, Sender, channel};
use std::iter::repeat;
use std::io::{Write, Result as IoResult};
use std::fmt;

use data::{ Value, DataMode, Type, Shape };
use session::ValueID;
use eval::Expr;

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

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{}(", self.tag));
        for (i, c) in self.components.iter().enumerate() {
            if i != 0 { try!(write!(f, ", ")); }
            try!(write!(f, "{}", c));
        }
        write!(f, ")")
    }
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

pub fn write_step_tree(f: &mut Write, s: &Step, indent: u32) -> IoResult<()> {
    let i: String = repeat(" ").take(indent as usize).collect();
    match *s {
        Step::Nop => {},
        Step::Token(ref message) => {
            try!(writeln!(f, "{}Token: {:?}", i, message));
        }
        Step::TokenTop(ref message, box ref body) => {
            try!(writeln!(f, "{}Up: {:?}", i, message));
            try!(write_step_tree(f, body, indent+1));
        }
        Step::Seq(ref steps) => {
            try!(writeln!(f, "{}Seq", i));
            for c in steps.iter() {
                try!(write_step_tree(f, c, indent+1));
            }
        }
        Step::Repeat(ref count, box ref inner, up) => {
            try!(writeln!(f, "{}Repeat: {:?} {}", i, count, up));
            try!(write_step_tree(f, inner, indent + 1));
        }
        Step::Foreach(width, ref vars, box ref inner) => {
            try!(write!(f, "{}For: {} ", i, width));
            for &(id, ref expr, dir) in vars { try!(write!(f, "{}={:?} {:?}, ", id, expr, dir)); }
            try!(writeln!(f, ""));
            try!(write_step_tree(f, inner, indent + 1));
        }
    }
    Ok(())
}

pub type ConnectionMessage = (MessageTag, Vec<Value>);

pub struct Connection {
    types: Vec<Vec<(Type, /*tx*/ bool, /* rx */ bool)>>,

    rx: Option<Receiver<ConnectionMessage>>,
    tx: Option<Sender<ConnectionMessage>>,

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

        (Connection{ types: t1, tx: s1, rx: r2, alive: alive },
         Connection{ types: t2, tx: s2, rx: r1, alive: alive })
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

    pub fn can_tx(&self) -> bool { self.tx.is_some() }
    pub fn can_rx(&self) -> bool { self.rx.is_some() }
}
