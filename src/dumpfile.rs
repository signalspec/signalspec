use std::io;
use std::io::prelude::*;

use grammar::literal;
use data::{ Value, DataMode, Shape, ShapeVariant };
use exec;
use eval;
use session::Process;
use resolve::scope::Item;
use connection_io::{ConnectionRead, ConnectionWrite};

struct ValueDumpDown(Shape);
impl Process for ValueDumpDown {
    fn run(&self, _: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        let mut c = ConnectionWrite(downwards);
        write_values(&mut c, upwards);
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }
}

struct ValueDumpUp(Shape);
impl Process for ValueDumpUp {
    fn run(&self, _: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
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
    fn run(&self, _: &mut eval::State, downwards: &mut exec::Connection, _upwards: &mut exec::Connection) -> bool {
        let mut stdout = ::std::io::stdout();
        write_values(&mut stdout, downwards);
        true
    }

    fn shape_up(&self) -> &Shape {
        lazy_static! {
            static ref SHAPE: Shape = Shape::null();
        }
        &SHAPE
    }
}

pub fn process(downward_shape: &Shape, arg: Item) -> Box<Process + 'static> {
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
        if port.send((0, lit)).is_err() { break; }
    }
}

pub fn write_values(w: &mut Write, port: &mut exec::Connection) {
    if port.send((0, Vec::new())).is_err() { return; }
    loop {
        match port.recv() {
            Ok((_, v)) => {
                for (i, v) in v.iter().enumerate() {
                    if i != 0 { w.write_all(b", ").unwrap(); }
                    (write!(w, "{}", v)).unwrap();
                }
                w.write_all(b"\n").unwrap();
            }
            Err(..) => break,
        }
        if port.send((0, Vec::new())).is_err() { break; }
    }
}
