use std::sync::mpsc::{Receiver, Sender, channel};
use std::io;
use std::io::prelude::*;
use num_complex::Complex;
use protocol::Fields;

use data::{ Value };

pub type ConnectionMessage = Vec<Option<Value>>;

pub struct Connection {
    rx: Option<Receiver<ConnectionMessage>>,
    rx_peek: Option<ConnectionMessage>,

    tx: Option<Sender<ConnectionMessage>>,

    pub sends: Vec<bool>,
    pub alive: bool,
}

/// Transfers data between two stacked abstractions
impl Connection {
    pub fn new(fields: &Fields) -> (Connection, Connection) {
        let direction = fields.direction();
        debug!("New connection: {:?} {:?}", fields, direction);

        let (s1, r1) = if direction.down {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let (s2, r2) = if direction.up {
            let (a, b) = channel();
            (Some(a), Some(b))
        } else { (None, None) };

        let alive = direction.down || direction.up;

        let (sends1, sends2) = fields.iter().map(|f| (f.dir.down, f.dir.up)).unzip();

        (Connection{ tx: s1, rx: r2, rx_peek: None, alive: alive, sends: sends1 },
         Connection{ tx: s2, rx: r1, rx_peek: None, alive: alive, sends: sends2 })
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

    fn do_recv(&mut self) -> Result<ConnectionMessage, ()> {
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

    pub fn peek(&mut self) -> Result<&ConnectionMessage, ()> {
        if self.rx_peek.is_none() {
            self.rx_peek = Some(self.do_recv()?);
        }
        Ok(self.rx_peek.as_ref().unwrap())
    }

    pub fn recv(&mut self) -> Result<ConnectionMessage, ()> {
        self.peek()?;
        Ok(self.rx_peek.take().unwrap())
    }

    pub fn can_tx(&self) -> bool { self.tx.is_some() }
    pub fn can_rx(&self) -> bool { self.rx.is_some() }

    pub fn read_bytes(&mut self) -> ConnectionRead {
        ConnectionRead(self)
    }

    pub fn write_bytes(&mut self) -> ConnectionWrite {
        ConnectionWrite(self)
    }

    pub fn iter_number(&mut self) -> ConnectionIterNumber {
        ConnectionIterNumber(self)
    }

    pub fn iter_complex(&mut self) -> ConnectionIterComplex {
        ConnectionIterComplex(self)
    }

    pub fn end(&mut self) {
        self.rx = None;
        self.tx = None;
        self.alive = false;
    }
}

pub struct ConnectionRead<'a>(&'a mut Connection);
impl<'a> Read for ConnectionRead<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        debug!("Read started: {} {}", self.0.alive, buf.len());

        let mut num_read = 0;
        for p in buf.iter_mut() {
            match self.0.recv() {
                Ok(v) => {
                    debug!("rx {:?}", v);
                    assert_eq!(v.len(), 1);
                    match v[0] {
                        Some(Value::Integer(b)) => { *p = b as u8; }
                        ref x => panic!("Byte connection received {:?}", x)
                    }
                }
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
            if self.0.send(vec![Some(Value::Integer(*b as i64))]).is_err() {
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

pub struct ConnectionIterNumber<'a>(&'a mut Connection);
impl<'a> Iterator for ConnectionIterNumber<'a> {
    type Item = f64;

    fn next(&mut self) -> Option<f64> {
        match self.0.recv() {
            Ok(v) => {
                assert_eq!(v.len(), 1);
                match v[0] {
                    Some(Value::Number(c)) => Some(c),
                    ref x => panic!("Number connection received {:?}", x)
                }
            }
            Err(..) => None,
        }
    }
}

pub struct ConnectionIterComplex<'a>(&'a mut Connection);
impl<'a> Iterator for ConnectionIterComplex<'a> {
    type Item = Complex<f64>;

    fn next(&mut self) -> Option<Complex<f64>> {
        match self.0.recv() {
            Ok(v) => {
                debug!("rx {:?}", v);
                assert_eq!(v.len(), 1);
                match v[0] {
                    Some(Value::Complex(c)) => Some(c),
                    ref x => panic!("Complex connection received {:?}", x)
                }
            }
            Err(..) => None,
        }
    }
}
