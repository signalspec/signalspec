use std::future::Future;
use std::sync::Arc;

use crate::{ Item, Shape };
use crate::runtime::channel::Channel;
use crate::runtime::{ChannelMessage, PrimitiveProcess};
use super::super::channel::item_to_msgs;

#[derive(Debug)]
pub(crate) struct SeqUpProcess(Vec<ChannelMessage>);

impl SeqUpProcess {
    pub fn instantiate(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let (ty, seq) = args.try_into()?;
        assert!(shape_up.unwrap().tag_offset == 0);
        let msgs = item_to_msgs(&ty, &seq)
                .map_err(|()| format!("argument can't be converted to values"))?;
        Ok(Arc::new(SeqUpProcess(msgs)))
    }
}

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
pub(crate) struct SeqDownProcess(Vec<ChannelMessage>);

impl SeqDownProcess {
    pub fn instantiate(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let (ty, seq) = args.try_into()?;
        assert!(shape_up.unwrap().tag_offset == 0);
        let msgs = item_to_msgs(&ty, &seq)
                .map_err(|()| format!("argument can't be converted to values"))?;
        Ok(Arc::new(SeqDownProcess(msgs)))
    }
}

impl PrimitiveProcess for SeqDownProcess {
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