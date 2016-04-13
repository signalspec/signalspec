use std::sync::mpsc::{Receiver, Sender, channel};

use data::{ Value, Shape };
use data::MessageTag;

pub type ConnectionMessage = (MessageTag, Vec<Value>);

pub struct Connection {
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

        (Connection{ tx: s1, rx: r2, alive: alive },
         Connection{ tx: s2, rx: r1, alive: alive })
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
