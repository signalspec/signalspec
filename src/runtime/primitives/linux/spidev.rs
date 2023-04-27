use std::{sync::Arc, path::PathBuf, future::Future};

use crate::{Index, PrimitiveDef, PrimitiveProcess, Channel, ChannelMessage};

use spidev::{Spidev, SpidevOptions, SpiModeFlags, SpidevTransfer};

pub fn add_primitives(index: &mut Index) {
    index.define_primitive("with Base() def spidev(const devname): Spi(#async_controller)", PrimitiveDef {
        id: "spidev",
        instantiate: primitive_args!(|devname: &str| {
            Ok(Arc::new(SpiProcess{ devname: PathBuf::from(devname) }))
        })
    });
}

struct SpiProcess{ devname: PathBuf }
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
                    match chan_dn.receive().await {
                        None => {
                            break 'process;
                        }
                        Some(ChannelMessage { variant: 0, values }) => { // data
                            debug!("data byte");
                            tx_buf.push((&values[0]).try_into().expect("invalid value on spidev receive channel"));
                            rx_buf.push(0u8);
                        }
                        Some(ChannelMessage { variant: 1, .. }) => { // end
                            debug!("end transaction");
                            break 'transaction;  
                        } 
                        Some(e) => panic!("spidev: unexpected message {e:?}"),
                    }
                }
                
                dev.transfer(&mut SpidevTransfer::read_write(&tx_buf, &mut rx_buf)).unwrap();
                debug!("spi: {tx_buf:?} {rx_buf:?}");


                for &b in &rx_buf {
                    chan_up.send(ChannelMessage { variant: 0, values: vec![b.into()] });
                }
                chan_up.send(ChannelMessage { variant: 1, values: vec![] });

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
