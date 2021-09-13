use std::io::prelude::*;
use std::io;
use signalspec::{ Shape, ShapeMsg, Connection, ConnectionMessage, Value, Item };

pub fn run(shape: Option<&Shape>, downwards: &mut Connection) -> bool {
    let stdout = ::std::io::stdout();
    if downwards.send(ConnectionMessage::empty()).is_err() { return false; }
    let shape_msg = match shape {
        None => return true,
        Some(s) => &s.messages[..],
    };

    loop {
        match downwards.recv() {
            Ok(v) => {
                let mut w = stdout.lock();
                format_message(&mut w, &v.values[..], &shape_msg[v.variant]).unwrap();
                w.write_all(b"\n").unwrap();
            }
            Err(..) => break,
        }
        if downwards.send(ConnectionMessage::empty()).is_err() { break; }
    }

    true
}

fn format_message<'a, 'b, 'c>(w: &mut dyn Write, values: &[Value], variant: &ShapeMsg) -> io::Result<()> {
    let mut iter = values.iter();
    write!(w, "{}(", variant.name)?;
    for (i, param) in variant.params.iter().enumerate() {
        if i > 0 { write!(w, ", ")?; }
        if param.direction.up {
            format_message_item(w, &mut iter, &param.item)?
        } else {
            write!(w, "_")?;
        }
    }
    write!(w, ")")
           
}

fn format_message_item<'a, I: Iterator<Item=&'a Value>>(w: &mut dyn Write, values: &mut I, shape_msg: &Item) -> io::Result<()> {
    match shape_msg {
        Item::Tuple(ref t) => {
            write!(w, "(")?;
            for (i, v) in t.iter().enumerate() {
                if i > 0 { write!(w, ", ")?; }
                format_message_item(w, values, v)?;
            }
            write!(w, ")")?;
        }
        Item::Value(_) => {
            let v = values.next().expect("message doesn't match shape");
            write!(w, "{}", v)?;
        }
        ref e => panic!("Don't know how to format {:?}", e)
    }
    Ok(())
}
