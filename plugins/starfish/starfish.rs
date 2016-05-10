extern crate signalspec;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate libusb;

use signalspec::{ ModuleLoader, Connection, Process, PrimitiveDef, Value, DataMode, Type, Shape, ShapeVariant, ShapeData, Item };

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

struct StarfishProcess;
impl Process for StarfishProcess {
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
        let baud = ((48e6/freq - 48e6*15e-9)/2. - 5.) as u8;
        starfish.enable_i2c(baud).unwrap();

        while let Ok(mut tok) = upwards.recv() {

            debug!("{:?}", tok);
            match tok.0 {
                0 /*start*/ => {
                    debug!("start {:x}", bitvec_to_u8(&tok.1[0]));
                    starfish.start(bitvec_to_u8(&tok.1[0])).unwrap();
                }
                1 /* r */ => {
                    starfish.rx(1).unwrap();
                    let r = starfish.recv();
                    debug!("rx => {:?}", r);
                    if let Ok(Reply::Data(b)) = r {
                        tok.1.push(u8_to_bitvec(b));
                    } else {
                        return false;
                    }
                }
                2 /* w */ => {
                    debug!("tx {:x}", bitvec_to_u8(&tok.1[0]));
                    starfish.tx_b(bitvec_to_u8(&tok.1[0])).unwrap();
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

    fn shape_up(&self) -> &Shape {
        lazy_static! {
            static ref SHAPE: Shape = Shape {
                variants: vec![
                    ShapeVariant {
                        data: ShapeData::Tup(vec![
                            ShapeData::Const(Value::Symbol("start".into())),
                            ShapeData::Val(Type::Vector(8, Box::new(Type::Integer(0, 1))), DataMode { down: true, up: false })
                        ])
                    },
                    ShapeVariant {
                        data: ShapeData::Tup(vec![
                            ShapeData::Const(Value::Symbol("r".into())),
                            ShapeData::Val(Type::Vector(8, Box::new(Type::Integer(0, 1))), DataMode { down: false, up: true })
                        ])
                    },
                    ShapeVariant {
                        data: ShapeData::Tup(vec![
                            ShapeData::Const(Value::Symbol("w".into())),
                            ShapeData::Val(Type::Vector(8, Box::new(Type::Integer(0, 1))), DataMode { down: true, up: false })
                        ])
                    },
                    ShapeVariant {
                        data: ShapeData::Const(Value::Symbol("stop".into()))
                    }
                ]
            };
        }
        &SHAPE
    }
}


struct StarfishDef;
impl PrimitiveDef for StarfishDef {
    fn invoke_def(&self, _downward_shape: &Shape, _arg: Item) -> Box<Process + 'static> {
        Box::new(StarfishProcess)
    }
}
static STARFISH_DEF: StarfishDef = StarfishDef;

pub fn load_plugin(loader: &ModuleLoader) {
    loader.add_primitive_def("starfish", &STARFISH_DEF);
}
