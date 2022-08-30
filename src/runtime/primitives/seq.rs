use std::future::Future;
use std::sync::Arc;

use crate::runtime::channel::Channel;
use crate::core::{ Index, PrimitiveDef, Item };
use crate::runtime::{ChannelMessage, PrimitiveProcess};
use super::super::channel::item_to_msgs;

// This wouldn't need to be a primitive if vectors could contain tuples -- could
// be a simple `for` loop.
pub fn add_primitives(index: &mut Index) {
    index.define_primitive("with Base() def seq(const ty, const #up, const seq): Seq(ty, #up)", PrimitiveDef {
        id: "const_seq_up",
        instantiate: primitive_args!(|seq: &Item| {
            Ok(Arc::new(SeqUpProcess(item_to_msgs(seq))))
        })
    });

    index.define_primitive("with Base() def seq(const ty, const #dn, const seq): Seq(ty, #dn)", PrimitiveDef {
        id: "const_seq_down",
        instantiate: primitive_args!(|seq: &Item| {
            Ok(Arc::new(SeqDownProcess(item_to_msgs(seq))))
        })
    });
}

#[derive(Debug)]
struct SeqUpProcess(Vec<ChannelMessage>);
impl PrimitiveProcess for SeqUpProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        assert_eq!(chan.len(), 1);
        let chan = chan.into_iter().next().unwrap();
        let data = self.0.clone();
        Box::pin(async move {
            for i in data {
                chan.send(i);
            }
            Ok(())
        })
    }
    
}

#[derive(Debug)]
struct SeqDownProcess(Vec<ChannelMessage>);
impl PrimitiveProcess for SeqDownProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        assert_eq!(chan.len(), 1);
        let chan = chan.into_iter().next().unwrap();
        let expected = self.0.clone();
        Box::pin(async move {
            let mut received = vec![];
            while let Some(msg) = chan.receive().await {
                received.push(msg);
            }

            if received != expected {
                debug!("Expected {expected:?}, got {received:?}");
                Err(())
            } else {
                debug!("Correctly received {received:?}");
                Ok(())
            }
        })
    }
    
}