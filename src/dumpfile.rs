use std::io::{BufferedReader};
use grammar::literal;
use ast::Value;
use exec;

// TODO: how to support nesting, shape
fn parse_line(line: &str) -> Vec<Value> {
  line.split(',').map(|x| literal(x.trim()).unwrap()).collect()
}

pub fn read_values(reader: &mut Reader, port: &mut exec::Connection) {
  for line in BufferedReader::new(reader).lines() {
    let lit = parse_line(line.unwrap().as_slice());
    if port.recv().is_err() { break; }
    if port.send(lit).is_err() { break; }
  }
}

pub fn write_values(w: &mut Writer, port: &mut exec::Connection) {
  if port.send(Vec::new()).is_err() { return; }
  loop {
    match port.recv() {
      Ok(v) => {
        for (i, v) in v.iter().enumerate() {
          if i != 0 { w.write(b", ").unwrap(); }
          (write!(w, "{}", v)).unwrap();
        }
        w.write(b"\n");
      }
      Err(..) => break,
    }
    if port.send(Vec::new()).is_err() { break; }
  }
}
