use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::fs::File;

use ast::Value;
use exec;
use eval::{self, DataMode, Expr};
use session::Process;
use resolve::types::{self, Shape};
use resolve::scope::Item;
use session;
use ast;

pub struct ConnectionRead<'a>(pub &'a mut exec::Connection);
impl<'a> Read for ConnectionRead<'a> {
    fn read(&mut self, mut buf: &mut [u8]) -> io::Result<usize> {
        debug!("Read started: {} {}", self.0.alive, buf.len());

        let mut num_read = 0;
        for p in buf.iter_mut() {
            match self.0.recv() {
                Ok(v) => {
                    debug!("rx {:?}", v);
                    assert_eq!(v.len(), 1);
                    match v[0] {
                        Value::Integer(b) => { *p = b as u8; }
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

pub struct ConnectionWrite<'a>(pub &'a mut exec::Connection);
impl<'a> Write for ConnectionWrite<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {

        debug!("write started: {}", buf.len());

        for b in buf.iter() {
            if self.0.send(vec![Value::Integer(*b as i64)]).is_err() {
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

struct ReaderProcess(pub PathBuf);
impl Process for ReaderProcess {
    fn run(&self, _: &mut eval::State, _: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        debug!("reader started");

        let mut c = ConnectionWrite(upwards);
        let mut file = File::open(&self.0).unwrap();
        io::copy(&mut file, &mut c).is_ok()
    }

    fn shape_up(&self) -> &Shape {
        static SHAPE: Shape = Shape::Val(types::Integer(0, 255), DataMode { down: false, up: true });
        &SHAPE
    }
}

struct WriterProcess(pub PathBuf);
impl Process for WriterProcess {
    fn run(&self,  _: &mut eval::State, _: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        debug!("writer started {} {}", upwards.can_tx(), upwards.can_rx());

        let mut c = ConnectionRead(upwards);
        let mut file = File::create(&self.0).unwrap();
        io::copy(&mut c, &mut file).is_ok()
    }

    fn shape_up(&self) -> &Shape {
        static SHAPE: Shape = Shape::Val(types::Integer(0, 255), DataMode { down: true, up: false });
        &SHAPE
    }
}

pub fn file_process(arg: Item) -> Box<session::Process + 'static> {
    let args = match arg {
        Item::Tuple(v) => v,
        x => panic!("Unknown args type: {:?}", x)
    };

    let path = match &args[0] {
        &Item::String(ref v) => PathBuf::from(&v),
        x => panic!("Expected string, found {:?}", x)
    };

    match &args[1] {
        &Item::Value(Expr::Const(ast::Value::Symbol(ref v))) => {
            match &v[..] {
                "r" => box ReaderProcess(path),
                "w" => box WriterProcess(path),
                _ => panic!("Unknown file mode {:?}", args[1])
            }
        }
        _ => panic!("Expected symbol, found {:?}", args[1])
    }
}
