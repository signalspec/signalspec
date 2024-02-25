use core::slice;
use std::{sync::Arc, path::PathBuf, future::Future};
use log::{info, error};

use crate::{Channel, Item, Shape, Value, ChannelMessage};
use crate::runtime::PrimitiveProcess;

use serial2::SerialPort;

pub(crate) struct SerialProcess{
    devname: PathBuf,
    rate: u32,
}

impl SerialProcess {
    pub fn instantiate(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let (devname, rate) = args.try_into()?;
        let devname: String = devname.try_into()?;
        let rate: Value = rate.try_into()?;
        let rate: u32 = (&rate).try_into().map_err(|()| "rate should be a number")?;
        assert!(shape_up.unwrap().tag_offset == 1);
        Ok(Arc::new(SerialProcess { devname: PathBuf::from(devname), rate }))
    }
}

const TAG_WRITE: usize = 1;
const TAG_READ: usize = 2;

impl PrimitiveProcess for SerialProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        let [chan_up, chan_dn] = <[_; 2]>::try_from(chan).map_err(|_| "wrong channels").unwrap();

        let devname = self.devname.clone();
        let rate = self.rate;

        Box::pin(async move {
            let dev = match SerialPort::open(devname, rate) {
                Ok(dev) => dev,
                Err(e) => { error!("Failed to open serial port: {e}"); return Err(()); }
            };

            loop {
                let rx = chan_dn.receive().await;
                match rx.peek().variant {
                    0 => {
                        break;
                    }
                    TAG_READ => {
                        rx.pop();
                        let mut b = 0;
                        match dev.read(slice::from_mut(&mut b)) {
                            Ok(1) => (),
                            Ok(n) => {
                                error!("unexpected read of {n}");
                                return Err(());
                            }
                            Err(e) => {
                                error!("read error: {e}");
                                return Err(());
                            }
                        }
                        info!("serial rx: {b:02x}");
                        chan_up.send(ChannelMessage { variant: TAG_READ, values: vec![Value::from_byte(b)] });
                    }
                    TAG_WRITE => {
                        let values = rx.pop().values;
                        let v = values[0].as_byte().expect("serial: invalid value on receive channel");
                        match dev.write(&[v]) {
                            Ok(1) => (),
                            Ok(n) => {
                                error!("write returned unexpected {n}");
                                return Err(());
                            }
                            Err(e) => {
                                error!("write error: {e}");
                                return Err(());
                            }
                        }
                        info!("serial tx: {v:02x}");
                        chan_up.send(ChannelMessage { variant: TAG_WRITE, values: vec![] });
                    }
                    e => panic!("serial: unexpected message tag {e:?}"),
                }
            }

            Ok(())
        })
    }

}

impl ::std::fmt::Debug for SerialProcess {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "SerialProcess({})", self.devname.display())
    }
}
