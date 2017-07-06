use std::io::prelude::*;
use std::io;
use signalspec::{ ProcessInfo, Shape, Fields, Process, Connection, Value };

pub fn make(shape: Shape) -> ProcessInfo {
    ProcessInfo {
        implementation: Box::new(Console(shape)),
        shape_up: Shape::null(),
        fields_up: Fields::null(),
    }
}

struct Console(pub Shape);
impl Process for Console {
    fn run(&self, downwards: &mut Connection, _upwards: &mut Connection) -> bool {
        let stdout = ::std::io::stdout();
        if downwards.send(Vec::new()).is_err() { return false; }
        let shape = &self.0;

        loop {
            match downwards.recv() {
                Ok(v) => {
                    let mut w = stdout.lock();
                    format_message(&mut w, &mut v.into_iter(), shape).unwrap();
                    w.write_all(b"\n").unwrap();
                }
                Err(..) => break,
            }
            if downwards.send(Vec::new()).is_err() { break; }
        }

        true
    }
}

fn format_message<I: Iterator<Item=Option<Value>>>(w: &mut Write, values: &mut I, shape: &Shape) -> io::Result<()> {
    match *shape {
        Shape::Tup(ref t) => {
            try!(write!(w, "("));
            for (i, v) in t.iter().enumerate() {
                if i > 0 { try!(write!(w, ", ")); }
                try!(format_message(w, values, v));
            }
            try!(write!(w, ")"));
        }
        Shape::Const(ref c) => {
            try!(write!(w, "{}", c));
        }
        Shape::Val(_) => {
            if let Some(v) = values.next().expect("message doesn't match shape") {
                try!(write!(w, "{}", v));
            } else {
                try!(write!(w, "_"));
            }
        }
        _ => { unimplemented!() }
    }
    Ok(())
}
