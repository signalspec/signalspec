#[macro_use] extern crate signalspec;
#[macro_use] extern crate log;
extern crate libusb;

use signalspec::{ Index, Connection, PrimitiveProcess, PrimitiveDef, Value };

mod starfish_usb;
use starfish_usb::{ StarfishUsb, find_device };
mod proto;
use proto::{ StarfishProto, Reply };

fn bitvec_to_u8(v: &Value) -> u8 {
    match *v {
        Value::Vector(ref v) => {
            v.iter().fold(0, |acc, x| {
                match *x {
                    Value::Integer(bit) => (acc << 1) | (bit as u8),
                    _ => panic!("Expected byte")
                }
            })
        }
        _ => panic!("Expected byte")
    }
}

fn u8_to_bitvec(v: u8) -> Value {
    Value::Vector((0..8).rev().map(|bit| Value::Integer(((v >> bit) & 1) as i64) ).collect())
}

#[derive(Debug)]
struct StarfishProcess;
impl PrimitiveProcess for StarfishProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        let context = libusb::Context::new().unwrap();
        let dev = match find_device(&context, 0x59e3, 0x5555) {
            Ok(d) => d,
            Err(e) => {
                 println!("starfish: {}", e);
                 return false;
            }
        };
        let mut starfish = StarfishProto::new(StarfishUsb::new(dev).unwrap());

        ::std::thread::sleep(::std::time::Duration::from_millis(10));

        let freq = 100000.;
        let baud = ((48e6/freq - 48e6*15e-9)/2. - 5.0f32) as u8;
        starfish.enable_i2c(baud).unwrap();

        while let Ok(mut tok) = upwards.recv() {

            let variant = if let Some(&Some(Value::Integer(x))) = tok.get(0) { x } else {
                println!("starfish: invalid data on top");
                return false;
            };

            debug!("{:?}", tok);

            match variant {
                0 /*start*/ => {
                    debug!("start {:x}", bitvec_to_u8(tok[1].as_ref().unwrap()));
                    starfish.start(bitvec_to_u8(tok[1].as_ref().unwrap())).unwrap();
                }
                1 /* r */ => {
                    starfish.rx(1).unwrap();
                    let r = starfish.recv();
                    debug!("rx => {:?}", r);
                    if let Ok(Reply::Data(b)) = r {
                        tok[2] = Some(u8_to_bitvec(b));
                    } else {
                        return false;
                    }
                }
                2 /* w */ => {
                    debug!("tx {:x}", bitvec_to_u8(tok[3].as_ref().unwrap()));
                    starfish.tx_b(bitvec_to_u8(tok[3].as_ref().unwrap())).unwrap();
                }
                3 /* stop */ => {
                    starfish.stop().unwrap();
                }
                _ => return false
            }
            if upwards.send(tok).is_err() {
                return false;
            }
        }

        true
    }
}


pub fn add_primitives(index: &mut Index) {
    index.define_prelude(r#"
        protocol Starfish() {
            start(var(#dn) byte),
            r(var(#up) byte),
            w(var(#dn) byte),
            stop(),
        }
    "#);

    index.define_primitive("with Base() def starfish(): Starfish()", PrimitiveDef {
        id: "starfish_usb",
        instantiate: primitive_args!(|| { Ok(Box::new(StarfishProcess)) })
    });
}
