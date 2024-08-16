use std::future::Future;
use std::sync::Arc;

use crate::{ Item, Shape };
use crate::runtime::channel::Channel;
use crate::runtime::{ChannelMessage, PrimitiveProcess};
use super::super::channel::item_to_msgs;

#[derive(Debug)]
pub(crate) struct SeqTxProcess(Vec<ChannelMessage>);

impl SeqTxProcess {
    pub fn dn(args: Item, shape_dn: &Shape, _shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        Self::new(args, shape_dn)
    }

    pub fn up(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        Self::new(args, shape_up.ok_or_else(|| "requires top signal".to_owned())?)
    }

    fn new(args: Item, shape: &Shape) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let (ty, seq) = args.try_into()?;

        if shape.tag_offset != 1 {
            return Err("can only be used on root shapes".into());
        }

        let msgs = item_to_msgs(&ty, &seq)
                .map_err(|()| format!("argument can't be converted to values"))?;
        Ok(Arc::new(SeqTxProcess(msgs)))
    }
}

impl PrimitiveProcess for SeqTxProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        assert_eq!(chan.len(), 1);
        let chan = chan.into_iter().next().unwrap();
        let data = self.0.clone();
        Box::pin(async move {
            for i in data {
                debug!("send {i:?}");
                chan.send(i);
            }
            Ok(())
        })
    }

}

#[derive(Debug)]
pub(crate) struct SeqRxProcess(Vec<ChannelMessage>);

impl SeqRxProcess {
    pub fn dn(args: Item, shape_dn: &Shape, _shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        Self::new(args, shape_dn)
    }

    pub fn up(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        Self::new(args, shape_up.ok_or_else(|| "requires top signal".to_owned())?)
    }


    fn new(args: Item, shape: &Shape) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let (ty, seq) = args.try_into()?;

        if shape.tag_offset != 1 {
            return Err("can only be used on root shapes".into());
        }

        let msgs = item_to_msgs(&ty, &seq)
                .map_err(|()| format!("argument can't be converted to values"))?;
        Ok(Arc::new(SeqRxProcess(msgs)))
    }
}

impl PrimitiveProcess for SeqRxProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        assert_eq!(chan.len(), 1);
        let chan = chan.into_iter().next().unwrap();
        let expected = self.0.clone();
        Box::pin(async move {
            let mut received = vec![];
            while let Some(msg) = chan.receive().await.pop_if(1) {
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
