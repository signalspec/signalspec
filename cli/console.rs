use std::io::prelude::*;
use std::io;
use signalspec::{ Handle, ShapeMsg, Value, Item, LeafItem };

pub fn run(mut handle: Handle) -> Result<(), ()> {
    let stdout = ::std::io::stdout();

    if let Some(shape) = handle.shape().cloned() {
        let shape_msg = &shape.messages[..];

        while let Some(v) = handle.receive() {
            let mut w = stdout.lock();
            format_message(&mut w, &v.values[..], &shape_msg[v.variant]).unwrap();
            w.write_all(b"\n").unwrap();
        }
    }

    handle.finish()
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
        Item::Leaf(LeafItem::Value(_)) => {
            let v = values.next().expect("message doesn't match shape");
            write!(w, "{}", v)?;
        }
        ref e => panic!("Don't know how to format {:?}", e)
    }
    Ok(())
}
