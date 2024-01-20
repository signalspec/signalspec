use std::{io::prelude::*, fmt::Display};
use signalspec::{ Handle, Value, Dir, TypeTree, ChannelMessage, Shape };

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
        let variant = &self.shape.messages[self.message.variant];
        let mut values = self.message.values.iter();

        write!(f, "{}", variant.name)?;
        variant.params.format(f, |param, f| {
            match param.direction {
                Dir::Dn => write!(f, "_"),
                Dir::Up => format_message_item(f, &mut values, &param.ty),
            }
        })
    }
}

fn format_message_item<'a>(f: &mut std::fmt::Formatter<'_>, values: &mut impl Iterator<Item=&'a Value>, shape_msg: &TypeTree) -> std::fmt::Result {
    match shape_msg {
        TypeTree::Tuple(ref t) => {
            t.format(f, |v, f| {
                format_message_item(f, values, v)
            })
        }
        TypeTree::Leaf(_) => {
            let v = values.next().expect("message doesn't match shape");
            write!(f, "{}", v)
        }
    }
}
