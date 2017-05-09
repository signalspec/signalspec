use std::io;
use std::io::prelude::*;

use data::{ Value, DataMode };
use protocol::{Shape, Fields};
use connection::Connection;
use process::{Process, PrimitiveDef};
use language::Item;

struct ValueDumpDown(Shape);
impl Process for ValueDumpDown {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut c = downwards.write_bytes();
        write_messages(&mut c, upwards, &self.0);
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }

    fn fields_up(&self) -> &Fields { unimplemented!() }
}

struct ValueDumpUp(Shape);
impl Process for ValueDumpUp {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut c = io::BufReader::new(downwards.read_bytes());
        read_values(&mut c, upwards);
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }

    fn fields_up(&self) -> &Fields { unimplemented!() }
}

pub struct ValueDumpPrint(pub Shape);
impl Process for ValueDumpPrint {
    fn run(&self, downwards: &mut Connection, _upwards: &mut Connection) -> bool {
        let mut stdout = ::std::io::stdout();
        write_messages(&mut stdout, downwards, &self.0);
        true
    }

    fn shape_up(&self) -> &Shape {
        lazy_static! {
            static ref SHAPE: Shape = Shape::null();
        }
        &SHAPE
    }

    fn fields_up(&self) -> &Fields { unimplemented!() }
}

pub struct DumpfileDef;
impl PrimitiveDef for DumpfileDef {
    fn invoke_def(&self, downward_shape: &Shape, arg: Item) -> Box<Process + 'static> {
        let dir = downward_shape.match_bytes()
            .expect("Invalid shape below dumpfile::process");

        let upward_shape = unimplemented!();

        match dir {
            DataMode { down: false, up: true } => box ValueDumpUp(upward_shape),
            DataMode { down: true, up: false } => box ValueDumpDown(upward_shape),
            _ => panic!("Invalid shape {:?} below dumpfile::process", downward_shape)
        }
    }
}
pub static DUMPFILE_DEF: DumpfileDef = DumpfileDef;

// TODO: how to support nesting, shape
pub fn parse_line(line: &str) -> Vec<Option<Value>> {
    if line.trim().len() == 0 {
        return Vec::new()
    }
    line.split(',').map(|x| Some(::language::parse_literal(x.trim()).unwrap())).collect()
}

pub fn read_values(reader: &mut BufRead, port: &mut Connection) {
    for line in reader.lines() {
        let lit = parse_line(&line.unwrap());
        if port.recv().is_err() { break; }
        if port.send(lit).is_err() { break; }
    }
}

pub fn write_message(w: &mut Write, values: &mut Iterator<Item=Option<Value>>, shape: &Shape) -> io::Result<()> {
    match *shape {
        Shape::Tup(ref t) => {
            try!(write!(w, "("));
            for (i, v) in t.iter().enumerate() {
                if i > 0 { try!(write!(w, ", ")); }
                try!(write_message(w, values, v));
            }
            try!(write!(w, ")"));
        }
        Shape::Val(_, ref mode) => {
            if mode.up {
                try!(write!(w, "{}", values.next().unwrap().expect("missing value in message")))
            } else {
                try!(write!(w, "_"));
            }
        }
        Shape::Const(ref c) => {
            try!(write!(w, "{}", c));
        }
        Shape::Protocol{..} => { unimplemented!() }
    }
    Ok(())
}

pub fn write_messages(w: &mut Write, port: &mut Connection, shape: &Shape) {
    if port.send(Vec::new()).is_err() { return; }
    loop {
        match port.recv() {
            Ok(v) => {
                write_message(w, &mut v.into_iter(), shape).unwrap();
                w.write_all(b"\n").unwrap();
            }
            Err(..) => break,
        }
        if port.send(Vec::new()).is_err() { break; }
    }
}
