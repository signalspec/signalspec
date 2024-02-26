use std::{io::prelude::*, fmt::Display, ops::Neg};
use signalspec::{ Handle, Value, Dir, TypeTree, ChannelMessage, Shape, Type };
use num_traits::ToPrimitive;

pub fn run(mut handle: Handle) -> Result<(), ()> {
    let stdout = ::std::io::stdout();

    if let Some(shape) = handle.shape().cloned() {
        while let Some(message) = handle.receive() {
            let mut w = stdout.lock();
            writeln!(w, "{}", FormatMessage { message: &message, shape: &shape }).unwrap()
        }
    }

    handle.finish()
}

struct FormatMessage<'a> {
    message: &'a ChannelMessage,
    shape: &'a Shape,
}

impl<'a> Display for FormatMessage<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(variant) = self.shape.messages.iter().find(|m| m.tag == self.message.variant) {
            let mut values = self.message.values.iter();
    
            write!(f, "{}", variant.name)?;
            variant.params.format(f, |param, f| {
                match param.direction {
                    Dir::Dn => write!(f, "_"),
                    Dir::Up => format_message_item(f, &mut values, &param.ty),
                }
            })
        } else {
            write!(f, "{:?}", self.message)
        }
    }
}

fn format_message_item<'a>(f: &mut std::fmt::Formatter<'_>, values: &mut impl Iterator<Item=&'a Value>, shape_msg: &TypeTree) -> std::fmt::Result {
    match shape_msg {
        TypeTree::Tuple(ref t) => {
            t.format(f, |v, f| {
                format_message_item(f, values, v)
            })
        }
        TypeTree::Leaf(t) => {
            let v = values.next().expect("message doesn't match shape");

            match (t, v) {
                (Type::Number(nt), Value::Number(v)) => {
                    let value = v.to_f64().unwrap_or(f64::NAN);
                    let scale = nt.scale().to_f64().unwrap_or(1.0);
                    let signed = nt.min() < 0;
                    let max = nt.max() as f64 * scale;
                    let digits = max.log10().max(0.0).ceil() as usize;
                    let decimals = scale.log10().min(0.0).neg().ceil() as usize;
                    let width = digits + decimals + if decimals > 0 { 1 } else { 0 } + if signed { 1 } else { 0 };

                    write!(f, "{value:width$.*}", decimals)
                }
                (_, v) => write!(f, "{}", v)
            }
        }
    }
}
