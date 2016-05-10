use std::sync::mpsc::{Receiver, Sender, channel};
use std::io;
use std::io::prelude::*;

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

    pub fn read_bytes(&mut self) -> ConnectionRead {
        ConnectionRead(self)
    }

    pub fn write_bytes(&mut self) -> ConnectionWrite {
        ConnectionWrite(self)
    }
}

pub struct ConnectionRead<'a>(&'a mut Connection);
impl<'a> Read for ConnectionRead<'a> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        debug!("Read started: {} {}", self.0.alive, buf.len());

        let mut num_read = 0;
        for p in buf.iter_mut() {
            match self.0.recv() {
                Ok((0, v)) => {
                    debug!("rx {:?}", v);
                    assert_eq!(v.len(), 1);
                    match v[0] {
                        Value::Integer(b) => { *p = b as u8; }
                        ref x => panic!("Byte connection received {:?}", x)
                    }
                }
                Ok(..) => panic!("Received a message prohibited by shape"),
                Err(..) => break,
            }
            num_read += 1;
        }

        debug!("Read completed: {} {}", self.0.alive, num_read);

        Ok(num_read)
    }
}

pub struct ConnectionWrite<'a>(&'a mut Connection);
impl<'a> Write for ConnectionWrite<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {

        debug!("write started: {}", buf.len());

        for b in buf.iter() {
            if self.0.send((0, vec![Value::Integer(*b as i64)])).is_err() {
                return Err(io::Error::new(io::ErrorKind::BrokenPipe, "Stream ended"))
            }
        }

        debug!("write completed: {}", buf.len());

        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}