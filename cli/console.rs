use std::io::prelude::*;
use std::io;
use signalspec::{ Shape, ShapeVariant, Connection, Value, Item };

pub fn run(shape: &Shape, downwards: &mut Connection) -> bool {
    let stdout = ::std::io::stdout();
    if downwards.send(Vec::new()).is_err() { return false; }
    let shape_msg = match shape {
        &Shape::None => return true,
        &Shape::Seq { ref messages, .. } => &messages[..],
    };

    loop {
        match downwards.recv() {
            Ok(v) => {
                let mut w = stdout.lock();
                format_message(&mut w, &v[..], shape_msg).unwrap();
                w.write_all(b"\n").unwrap();
            }
            Err(..) => break,
        }
        if downwards.send(Vec::new()).is_err() { break; }
    }

    true
}

fn format_message<'a, 'b, 'c>(w: &mut Write, values: &[Option<Value>], shape_msg: &[ShapeVariant]) -> io::Result<()> {
    match shape_msg.len() {
        0 => Ok(()),
        1 => format_message_item(w, &mut values.iter(), &shape_msg[0].shape),
        _ => {
            if let Some(&Some(Value::Integer(tag))) = values.get(0) {
                let mut iter = values[1..].iter();
                let variant = &shape_msg[tag as usize];
                write!(w, "{}(", variant.name)?;
                format_message_item(w, &mut iter, &variant.shape)?;
                write!(w, ")")
            } else {
                panic!("Message is missing tag")
            }
        }
    }
}

fn format_message_item<'a, I: Iterator<Item=&'a Option<Value>>>(w: &mut Write, values: &mut I, shape_msg: &Item) -> io::Result<()> {
    match *shape_msg {
        Item::Tuple(ref t) => {
            try!(write!(w, "("));
            for (i, v) in t.iter().enumerate() {
                if i > 0 { try!(write!(w, ", ")); }
                try!(format_message_item(w, values, v));
            }
            try!(write!(w, ")"));
        }
        Item::Value(_) => {
            if let &Some(ref v) = values.next().expect("message doesn't match shape") {
                try!(write!(w, "{}", v));
            } else {
                try!(write!(w, "_"));
            }
        }
        ref e => panic!("Don't know how to format {:?}", e)
    }
    Ok(())
}
