use std::{sync::Arc, path::PathBuf, future::Future};

use crate::{Channel, ChannelMessage, Item, Shape, Value};
use crate::runtime::PrimitiveProcess;

use spidev::{Spidev, SpidevOptions, SpiModeFlags, SpidevTransfer};

pub(crate) struct SpiProcess{
    devname: PathBuf
}

impl SpiProcess {
    pub fn instantiate(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let devname: String = args.try_into()?;
        assert!(shape_up.unwrap().tag_offset == 1);
        Ok(Arc::new(SpiProcess{ devname: PathBuf::from(devname) }))
    }
}

const TAG_DATA: usize = 2;
const TAG_END: usize = 1;

impl PrimitiveProcess for SpiProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        let [chan_up, chan_dn] = <[_; 2]>::try_from(chan).map_err(|_| "wrong channels").unwrap();
        let fname = self.devname.clone();
        Box::pin(async move {
            let mut dev = match Spidev::open(fname) {
                Ok(dev) => dev,
                Err(e) => { eprintln!("Failed to open SPI: {e}"); return Err(()); }
            };
            let options = SpidevOptions::new()
                .bits_per_word(8)
                .max_speed_hz(20_000)
                .mode(SpiModeFlags::SPI_MODE_0)
                .build();
            dev.configure(&options).unwrap();

            let mut tx_buf: Vec<u8> = Vec::new();
            let mut rx_buf: Vec<u8> = Vec::new();

            'process: loop {
                tx_buf.clear();
                rx_buf.clear();

                'transaction: loop {
                    let rx = chan_dn.receive().await ;
                    match rx.peek().variant {
                        0 => {
                            break 'process;
                        }
                        TAG_DATA => {
                            let values = rx.pop().values;
                            debug!("data byte");
                            tx_buf.push(values[0].as_byte().expect("invalid value on spidev receive channel"));
                            rx_buf.push(0u8);
                        }
                        TAG_END => {
                            rx.pop();
                            debug!("end transaction");
                            break 'transaction;  
                        }
                        e => panic!("spidev: unexpected message {e:?}"),
                    }
                }
                
                dev.transfer(&mut SpidevTransfer::read_write(&tx_buf, &mut rx_buf)).unwrap();
                debug!("spi: {tx_buf:?} {rx_buf:?}");


                for &b in &rx_buf {
                    chan_up.send(ChannelMessage { variant: TAG_DATA, values: vec![Value::from_byte(b)] });
                }
                chan_up.send(ChannelMessage { variant: TAG_END, values: vec![] });

            }

            debug!("end");
            return if tx_buf.is_empty() { Ok(()) } else { Err(()) };
        })
    }
    
}

impl ::std::fmt::Debug for SpiProcess {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "SpiProcess({})", self.devname.display())
    }
}
