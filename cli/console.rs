use std::io::prelude::*;
use std::io;
use signalspec::{ Handle, ShapeMsg, Value, Dir, TypeTree };

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
        match param.direction {
            Dir::Dn => write!(w, "_")?,
            Dir::Up => format_message_item(w, &mut iter, &param.ty)?,
        }
    }
    write!(w, ")")
           
}

fn format_message_item<'a, I: Iterator<Item=&'a Value>>(w: &mut dyn Write, values: &mut I, shape_msg: &TypeTree) -> io::Result<()> {
    match shape_msg {
        TypeTree::Tuple(ref t) => {
            write!(w, "(")?;
            for (i, v) in t.iter().enumerate() {
                if i > 0 { write!(w, ", ")?; }
                format_message_item(w, values, v)?;
            }
            write!(w, ")")?;
        }
        TypeTree::Leaf(_) => {
            let v = values.next().expect("message doesn't match shape");
            write!(w, "{}", v)?;
        }
    }
    Ok(())
}
