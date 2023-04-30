use std::{cell::{RefCell, RefMut}, rc::Rc, task::{Poll, Context, Waker}, collections::VecDeque, future::Future, io, pin::Pin};
use futures_lite::{ ready, AsyncRead, AsyncWrite };

use crate::{Value, Shape, Item, LeafItem, core::Expr, tree::Zip};

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelMessage {
    pub variant: usize,
    pub values: Vec<Value>,
}

impl ChannelMessage {
    pub fn empty() -> ChannelMessage { ChannelMessage { variant: !0, values: vec![] }}
    pub fn one(v: Value) -> ChannelMessage { ChannelMessage { variant: 0, values: vec![v] }}
}

struct ChannelInner {
    value: VecDeque<ChannelMessage>,
    closed: bool,
    read_waker: Option<Waker>,
}

#[derive(Clone)]
pub struct Channel {
    inner: Rc<RefCell<ChannelInner>>,
}

pub struct ChannelReadRef<'a> {
    inner: RefMut<'a, ChannelInner>
}

impl Channel {
    pub fn new() -> Channel {
        let inner = Rc::new(RefCell::new(ChannelInner {
            value: VecDeque::new(),
            closed: false,
            read_waker: None,
        }));
        Channel { inner }
    }

    pub fn poll_receive(&self, cx: &mut Context) -> Poll<()> {
        let mut i = self.inner.borrow_mut();
        if i.value.is_empty() && !i.closed {
            i.read_waker = Some(cx.waker().clone());
            Poll::Pending
        } else {
            Poll::Ready(())
        }
    }

    pub fn receive<'a>(&'a self) -> impl Future<Output = Option<ChannelMessage>> + 'a {
        async {
            futures_lite::future::poll_fn(|cx| self.poll_receive(cx)).await;
            self.read().pop()
        }
    }

    pub fn read(&self) -> ChannelReadRef {
        ChannelReadRef { inner: self.inner.borrow_mut() }
    }

    pub fn send(&self, m: ChannelMessage) {
        let mut i = self.inner.borrow_mut();
        i.value.push_back(m);
        if let Some(waker) = i.read_waker.take() {
            waker.wake();
        }
    }

    pub fn set_closed(&self, closed: bool) {
        let mut i = self.inner.borrow_mut();
        i.closed = closed;
        if closed && i.value.is_empty() {
            if let Some(waker) = i.read_waker.take() {
                waker.wake();
            }
        }
    }

    pub fn read_bytes(&mut self) -> ReadBytes { ReadBytes(self) }
    pub fn write_bytes(&mut self) -> WriteBytes { WriteBytes(self) }
}

impl<'a> ChannelReadRef<'a> {
    pub fn peek(&self) -> Option<&ChannelMessage> {
        self.inner.value.front()
    }

    pub fn pop(&mut self) -> Option<ChannelMessage> {
        self.inner.value.pop_front()
    }

    pub fn is_end(&self) -> bool {
        self.inner.value.is_empty() && self.inner.closed
    }
}


pub struct ReadBytes<'a>(&'a mut Channel);
impl<'a> AsyncRead for ReadBytes<'a> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut [u8]) -> Poll<io::Result<usize>> {
        let mut num_read = 0;
        ready!(self.0.poll_receive(cx));

        let mut rx = self.0.read();

        for dst in buf {
            if let Some(v) = rx.pop() {
                debug!("rx {:?}", v);
                assert_eq!(v.variant, 0);
                assert_eq!(v.values.len(), 1);
                match v.values[0].as_byte() {
                    Some(b) => { *dst = b; }
                    _ => panic!("Byte connection received {:?}", v.values[0])
                }
                num_read += 1;
            } else {
                break;
            }
        }

        Poll::Ready(Ok(num_read))
    }
}

pub struct WriteBytes<'a>(&'a mut Channel);
impl<'a> AsyncWrite for WriteBytes<'a> {
    fn poll_write(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &[u8]) -> Poll<io::Result<usize>> {
        for &b in buf.iter() {
            let m = ChannelMessage::one(Value::from_byte(b));
            self.0.send(m);
        }

        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_close(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }
}

#[derive(Clone)]
pub struct SeqChannels {
    pub dn: Option<Channel>,
    pub up: Option<Channel>,
}

impl SeqChannels {
    pub fn null() -> SeqChannels {
        SeqChannels { dn: None, up: None }
    }

    pub fn for_shape(shape: &Shape) -> SeqChannels {
        let dir = shape.direction();
        SeqChannels { 
            dn: dir.down.then(Channel::new),
            up: dir.up.then(Channel::new),
        }
    }
}

pub(crate) fn item_to_msgs(ty: &Item, seq: &Item) -> Result<Vec<ChannelMessage>, ()> {
    seq.as_tuple().iter().map(|i| {
        let mut msg = Vec::new();
        let mut valid = true;

        ty.zip(i, &mut |m| match m {
            Zip::Both(_, &Item::Leaf(LeafItem::Value(Expr::Const(ref c)))) => {
                msg.push(c.clone());
            },
            _ => {
                valid = false;
            }
        });

        if !valid { return Err(()) }
        Ok(ChannelMessage { variant: 0, values: msg })
    }).collect()
}
