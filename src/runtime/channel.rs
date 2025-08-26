use std::{cell::{RefCell, RefMut}, rc::Rc, task::{Poll, Context, Waker}, collections::VecDeque, future::Future, io, pin::Pin};
use futures_lite::{ AsyncRead, AsyncWrite };

use crate::{Value, Shape, Item, LeafItem, core::Expr, tree::Zip};

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelMessage {
    pub variant: usize,
    pub values: Vec<Value>,
}

struct ChannelInner {
    value: VecDeque<ChannelMessage>,
    end: bool,
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
            read_waker: None,
            end: false,
        }));
        Channel { inner }
    }

    pub fn poll_receive(&self, cx: &mut Context) -> Poll<ChannelReadRef<'_>> {
        let mut inner = self.inner.borrow_mut();
        if inner.value.is_empty() && !inner.end {
            inner.read_waker = Some(cx.waker().clone());
            Poll::Pending
        } else {
            Poll::Ready(ChannelReadRef { inner })
        }
    }

    pub fn receive<'a>(&'a self) -> impl Future<Output = ChannelReadRef<'a>> + 'a {
        futures_lite::future::poll_fn(|cx| self.poll_receive(cx))
    }

    pub fn send(&self, m: ChannelMessage) {
        let mut i = self.inner.borrow_mut();
        debug_assert!(!i.end);
        i.value.push_back(m);
        if let Some(waker) = i.read_waker.take() {
            waker.wake();
        }
    }

    pub fn end(&self, end: bool) {
        let mut i = self.inner.borrow_mut();
        i.end = end;
    }

    pub fn read_bytes(&mut self) -> ReadBytes<'_> { ReadBytes(self) }
    pub fn write_bytes(&mut self) -> WriteBytes<'_> { WriteBytes(self) }

    pub(crate) fn take_all(&self) -> Vec<ChannelMessage> {
        self.inner.borrow_mut().value.drain(..).collect()
    }
}

impl<'a> ChannelReadRef<'a> {
    pub fn peek(&self) -> &ChannelMessage {
        if self.inner.end && self.inner.value.is_empty() {
            const { &ChannelMessage { variant: 0, values: Vec::new() } }
        } else {
            self.inner.value.front().unwrap()
        }
    }

    pub fn pop(mut self) -> ChannelMessage {
        self.inner.value.pop_front().unwrap()
    }

    pub(crate) fn pop_if(self, tag: usize) -> Option<ChannelMessage> {
        if self.peek().variant == tag {
            Some(self.pop())
        } else {
            None
        }
    }
}

pub struct ReadBytes<'a>(&'a mut Channel);

impl<'a> AsyncRead for ReadBytes<'a> {
    fn poll_read(self: Pin<&mut Self>, cx: &mut Context<'_>, buf: &mut [u8]) -> Poll<io::Result<usize>> {
        let mut num_read = 0;

        for dst in buf {
            match self.0.poll_receive(cx) {
                Poll::Pending if num_read == 0 => return Poll::Pending,
                Poll::Pending => break,
                Poll::Ready(r) => {
                    match r.peek().variant {
                        0 => break,
                        1 => {
                            let v = r.pop();
                            debug!("rx {:?}", v);
                            assert_eq!(v.values.len(), 1);
                            match v.values[0].as_byte() {
                                Some(b) => { *dst = b; }
                                _ => panic!("Byte connection received {:?}", v.values[0])
                            }
                            num_read += 1;
                        }
                        t => panic!("Byte connection received unexpected tag {t}")
                    }
                }
            }
        }

        Poll::Ready(Ok(num_read))
    }
}

pub struct WriteBytes<'a>(&'a mut Channel);
impl<'a> AsyncWrite for WriteBytes<'a> {
    fn poll_write(self: Pin<&mut Self>, _cx: &mut Context<'_>, buf: &[u8]) -> Poll<io::Result<usize>> {
        for &b in buf.iter() {
            let m = ChannelMessage { variant: 1, values: vec![Value::from_byte(b)] };
            debug!("tx: {:?}", m);
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
        SeqChannels { 
            dn: shape.mode.has_dn_channel().then(Channel::new),
            up: shape.mode.has_up_channel().then(Channel::new),
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
        Ok(ChannelMessage { variant: 1, values: msg })
    }).collect()
}
