use std::io::{BufferedReader, BufferedWriter};
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

pub fn write_values(file: &mut Writer, port: &mut exec::Connection) {
  let mut w = BufferedWriter::new(file);
  loop {
    match port.recv() {
      Ok(v) => {
        w.write_line(v.to_string().as_slice()).unwrap();
      }
      Err(..) => break,
    }
    if port.send(Vec::new()).is_err() { break; }
  }
}
