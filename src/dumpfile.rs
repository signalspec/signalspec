use std::io;
use std::io::prelude::*;

use grammar::literal;
use ast::Value;
use exec;
use eval::{self, DataMode};
use session::Process;
use resolve::types::{self, Shape};
use resolve::scope::Item;
use connection_io::{ConnectionRead, ConnectionWrite};

struct ValueDumpDown;
impl Process for ValueDumpDown {
    fn run(&self, _: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        let mut c = ConnectionWrite(downwards);
        write_values(&mut c, upwards);
        true
    }

    fn shape_up(&self) -> &Shape {
        static SHAPE: Shape = Shape::Val(types::Bottom, DataMode { down: true, up: false });
        &SHAPE
    }
}

struct ValueDumpUp;
impl Process for ValueDumpUp {
    fn run(&self, _: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        let mut c = io::BufReader::new(ConnectionRead(downwards));
        read_values(&mut c, upwards);
        true
    }

    fn shape_up(&self) -> &Shape {
        static SHAPE: Shape = Shape::Val(types::Bottom, DataMode { down: false, up: true });
        &SHAPE
    }
}

pub fn process(bottom_shape: &Shape, _arg: Item) -> Box<Process + 'static> {
    match *bottom_shape {
        Shape::Val(types::Integer, DataMode { down: false, up: true }) => box ValueDumpUp,
        Shape::Val(types::Integer, DataMode { down: true, up: false }) => box ValueDumpDown,
        _ => panic!("Invalid shape {:?} below dumpfile::process", bottom_shape)
    }
}

// TODO: how to support nesting, shape
pub fn parse_line(line: &str) -> Vec<Value> {
    if line.trim().len() == 0 {
        return Vec::new()
    }
    line.split(',').map(|x| literal(x.trim()).unwrap()).collect()
}

pub fn read_values(reader: &mut BufRead, port: &mut exec::Connection) {
    for line in reader.lines() {
        let lit = parse_line(&line.unwrap());
        if port.recv().is_err() { break; }
        if port.send(lit).is_err() { break; }
    }
}

pub fn write_values(w: &mut Write, port: &mut exec::Connection) {
    if port.send(Vec::new()).is_err() { return; }
    loop {
        match port.recv() {
            Ok(v) => {
                for (i, v) in v.iter().enumerate() {
                    if i != 0 { w.write_all(b", ").unwrap(); }
                    (write!(w, "{}", v)).unwrap();
                }
                w.write_all(b"\n").unwrap();
            }
            Err(..) => break,
        }
        if port.send(Vec::new()).is_err() { break; }
    }
}
