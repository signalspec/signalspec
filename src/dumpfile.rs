use std::comm;
use std::io::{BufferedReader, BufferedWriter};
use grammar::literal;
use ast::Value;

pub fn read_values(reader: &mut Reader, port: &comm::DuplexStream<Option<Value>, Option<Value>>) {
  for line in BufferedReader::new(reader).lines() {
    let line = line.unwrap();
    let lit = match literal(line.as_slice().trim()) {
      Ok(lit) => lit,
      Err(lit) => fail!("Invalid line `{}`", line.as_slice())
    };
    port.recv();
    port.send(Some(lit));
  }
}

pub fn write_values(file: &mut Writer, port: &comm::DuplexStream<Option<Value>, Option<Value>>) {
  let mut w = BufferedWriter::new(file);
  loop {
    match port.recv_opt() {
      Ok(Some(v)) => {
        w.write_line(v.to_string().as_slice()).unwrap();
      }
      Ok(None) => (),
      Err(..) => break,
    }
    port.send(None);
  }
}