use std::{sync::Arc, path::PathBuf, future::Future};

use crate::{Channel, ChannelMessage, Item, Shape, Value};
use crate::runtime::PrimitiveProcess;

use i2cdev::core::I2CTransfer;
use i2cdev::linux::{LinuxI2CBus, LinuxI2CMessage, I2CMessage};

pub(crate) struct I2CProcess{
    devname: PathBuf
}

impl I2CProcess {
    pub fn instantiate(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let devname: String = args.try_into()?;
        assert!(shape_up.unwrap().tag_offset == 1);
        Ok(Arc::new(I2CProcess{ devname: PathBuf::from(devname) }))
    }
}

#[derive(Debug, Clone, Copy)]
enum Operation {
    Read,
    Write,
}

#[derive(Debug)]
struct Message {
    addr: u8,
    op: Operation,
    size: usize,
}

const TAG_STOP: usize = 1;
const TAG_START: usize = 2;
const TAG_READ: usize = 3;
const TAG_WRITE: usize = 4;

impl PrimitiveProcess for I2CProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        let [chan_up, chan_dn] = <[_; 2]>::try_from(chan).map_err(|_| "wrong channels").unwrap();

        let fname = self.devname.clone();
        Box::pin(async move {
            let mut dev = match LinuxI2CBus::new(fname) {
                Ok(dev) => dev,
                Err(e) => { eprintln!("Failed to open I2C: {e}"); return Err(()); }
            };

            let mut buf: Vec<u8> = Vec::new();
            let mut ops: Vec<Message> = Vec::new();

            'process: loop {
                buf.clear();
                ops.clear();

                'transaction: loop {
                    let rx = chan_dn.receive().await;
                    match rx.peek().variant {
                        0 => {
                            break 'process;
                        }
                        TAG_START => {
                            let values = rx.pop().values;
                            let addr = values[0].as_bits(7).expect("i2cdev: invalid address on receive channel") as u8;
                            let op = match values[1].as_symbol() {
                                Some("r") => Operation::Read,
                                Some("w") => Operation::Write,
                                _ => panic!("i2cdev: invalid rw value on receive channel"),
                            };
                            debug!("< start {addr:x} {op:?}");
                            ops.push(Message { addr, op, size: 0});
                        }
                        TAG_READ => {
                            rx.pop();
                            let Some(message) = ops.last_mut() else {
                                error!("read without start");
                                return Err(())
                            };
                            if !matches!(message.op, Operation::Read) {
                                error!("read in write message");
                                return Err(())
                            };
                            debug!("< read _");
                            buf.push(0);
                            message.size += 1;
                        }
                        TAG_WRITE => {
                            let values = rx.pop().values;
                            let Some(message) = ops.last_mut() else {
                                error!("write without start");
                                return Err(())
                            };
                            if !matches!(message.op, Operation::Write) {
                                error!("write in read message");
                                return Err(())
                            };
                            let v = values[0].as_byte().expect("i2cdev: invalid value on receive channel");
                            debug!("< write {v:x}");
                            buf.push(v);
                            message.size += 1;
                        }
                        TAG_STOP => {
                            rx.pop();
                            debug!("< stop");
                            break 'transaction;
                        }
                        e => panic!("i2cdev: unexpected message tag {e:?}"),
                    }
                }

                debug!("submitting transaction: {:?}", ops);

                {
                    let mut messages: Vec<LinuxI2CMessage> = Vec::new();
                    let mut buf = &mut buf[..];
                    for m in &ops {
                        let (slice, next_slice) = buf.split_at_mut(m.size);

                        let i: LinuxI2CMessage = match m.op {
                            Operation::Read => LinuxI2CMessage::read(slice),
                            Operation::Write => LinuxI2CMessage::write(slice),
                        };
                        messages.push(i.with_address(m.addr as u16));
                        buf = next_slice;
                    }

                    dev.transfer(&mut messages).unwrap();
                }

                let mut pos = 0;
                for m in &ops {
                    chan_up.send(ChannelMessage { variant: TAG_START, values: vec![] });

                    match m.op {
                        Operation::Read => {
                            for &b in &buf[pos..pos+m.size] {
                                debug!("> read {b:x}");
                                chan_up.send(ChannelMessage { variant: TAG_READ, values: vec![Value::from_byte(b)] });
                            }
                        },
                        Operation::Write => {
                            for _ in 0..m.size {
                                debug!("> write ..");
                                chan_up.send(ChannelMessage { variant: TAG_WRITE, values: vec![] });
                            }
                        },
                    }
                    pos += m.size;
                }

                chan_up.send(ChannelMessage { variant: TAG_STOP, values: vec![] });
            }

            debug!("end");
            return if ops.is_empty() { Ok(()) } else { Err(()) };
        })
    }

}

impl ::std::fmt::Debug for I2CProcess {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "I2CProcess({})", self.devname.display())
    }
}
