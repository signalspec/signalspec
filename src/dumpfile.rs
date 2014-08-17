use std::io::{BufferedReader, BufferedWriter};
use grammar::literal;
use exec;

pub fn read_values(reader: &mut Reader, port: &mut exec::Connection) {
  for line in BufferedReader::new(reader).lines() {
    let line = line.unwrap();
    let lit = match literal(line.as_slice().trim()) {
      Ok(lit) => lit,
      Err(_) => fail!("Invalid line `{}`", line.as_slice())
    };
    if port.recv().is_err() { break; }
    if port.send(Some(lit)).is_err() { break; }
  }
}

pub fn write_values(file: &mut Writer, port: &mut exec::Connection) {
  let mut w = BufferedWriter::new(file);
  loop {
    match port.recv() {
      Ok(Some(v)) => {
        w.write_line(v.to_string().as_slice()).unwrap();
      }
      Ok(None) => (),
      Err(..) => break,
    }
    if port.send(None).is_err() { break; }
  }
}
