use std::io;
use std::io::prelude::*;

use data::{ Value, DataMode, Shape, ShapeVariant, ShapeData };
use connection::Connection;
use process::{Process, PrimitiveDef};
use language::Item;
use connection_io::{ConnectionRead, ConnectionWrite};

struct ValueDumpDown(Shape);
impl Process for ValueDumpDown {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut c = ConnectionWrite(downwards);
        write_messages(&mut c, upwards, &self.0);
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }
}

struct ValueDumpUp(Shape);
impl Process for ValueDumpUp {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut c = io::BufReader::new(ConnectionRead(downwards));
        read_values(&mut c, upwards);
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }
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
}

pub struct DumpfileDef;
impl PrimitiveDef for DumpfileDef {
    fn invoke_def(&self, downward_shape: &Shape, arg: Item) -> Box<Process + 'static> {
        let dir = downward_shape.match_bytes()
            .expect("Invalid shape below dumpfile::process");

        let upward_shape = Shape { variants: vec![
            ShapeVariant { data: arg.into_data_shape(dir) }
        ]};

        match dir {
            DataMode { down: false, up: true } => box ValueDumpUp(upward_shape),
            DataMode { down: true, up: false } => box ValueDumpDown(upward_shape),
            _ => panic!("Invalid shape {:?} below dumpfile::process", downward_shape)
        }
    }
}
pub static DUMPFILE_DEF: DumpfileDef = DumpfileDef;

// TODO: how to support nesting, shape
pub fn parse_line(line: &str) -> Vec<Value> {
    if line.trim().len() == 0 {
        return Vec::new()
    }
    line.split(',').map(|x| ::language::parse_literal(x.trim()).unwrap()).collect()
}

pub fn read_values(reader: &mut BufRead, port: &mut Connection) {
    for line in reader.lines() {
        let lit = parse_line(&line.unwrap());
        if port.recv().is_err() { break; }
        if port.send((0, lit)).is_err() { break; }
    }
}

pub fn write_message(w: &mut Write, values: &mut Iterator<Item=&Value>, shape: &ShapeData) -> io::Result<()> {
    match *shape {
        ShapeData::Tup(ref t) => {
            try!(write!(w, "("));
            for (i, v) in t.iter().enumerate() {
                if i > 0 { try!(write!(w, ", ")); }
                try!(write_message(w, values, v));
            }
            try!(write!(w, ")"));
        }
        ShapeData::Val(_, ref mode) => {
            if mode.up {
                try!(write!(w, "{}", values.next().unwrap()))
            } else {
                try!(write!(w, "_"));
            }
        }
        ShapeData::Const(ref c) => {
            try!(write!(w, "{}", c));
        }
    }
    Ok(())
}

pub fn write_messages(w: &mut Write, port: &mut Connection, shape: &Shape) {
    if port.send((0, Vec::new())).is_err() { return; }
    loop {
        match port.recv() {
            Ok((tag, v)) => {
                write_message(w, &mut v.iter(), &shape.variants[tag].data).unwrap();
                w.write_all(b"\n").unwrap();
            }
            Err(..) => break,
        }
        if port.send((0, Vec::new())).is_err() { break; }
    }
}
