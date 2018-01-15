#[macro_use]
extern crate signalspec;
extern crate vcd;
extern crate ref_slice;

use std::io;
use ref_slice::ref_slice;

use signalspec::{ Connection, Process, Value, DataMode, Item, Ctxt, PrimitiveDef, PrimitiveDefFields, Fields };

/// Represent a shape as a VCD scope declaration, creating mapping from message index to VCD idcode
fn shape_to_scope(s: &Item) -> (vcd::Scope, Vec<vcd::IdCode>) {
    let mut ids = Vec::new();

    fn scope(ids: &mut Vec<vcd::IdCode>, l: &[Item], name: String) -> vcd::Scope {
        vcd::Scope {
            scope_type: vcd::ScopeType::Module,
            identifier: name,
            children: l.iter().enumerate()
                .map(|(i, x)| inner(ids, x, i.to_string())).collect()
        }
    }

    fn inner(ids: &mut Vec<vcd::IdCode>, s: &Item, name: String) -> vcd::ScopeItem {
        match *s {
            Item::Value(_) => {
                let code = vcd::IdCode::from(ids.len() as u32);
                ids.push(code);

                vcd::ScopeItem::Var( vcd::Var {
                    var_type: vcd::VarType::Wire,
                    size: 1,
                    code: code,
                    reference: name
                })
            }
            Item::Tuple(ref l) => vcd::ScopeItem::Scope(scope(ids, &l[..], name)),
            _ => unimplemented!()
        }
    }

    let top = if let &Item::Tuple(ref l) = s {
        scope(&mut ids, l, "top".to_owned())
    } else {
        scope(&mut ids, ref_slice(s), "top".to_owned())
    };
    (top, ids)
}

#[derive(Debug)]
struct VcdWrite(Item);
impl Process for VcdWrite {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut c = downwards.write_bytes();
        let mut w = vcd::Writer::new(&mut c);

        let (scope, ids) = shape_to_scope(&self.0);

        w.header(&vcd::Header {
            scope: scope,
            ..Default::default()
        }).unwrap();

        let mut time = 0u64;

        fn map_value(v: Option<Value>) -> vcd::Value {
            match v {
                Some(Value::Symbol(ref v)) if &v[..] == "h" => vcd::Value::V1,
                Some(Value::Symbol(ref v)) if &v[..] == "l" => vcd::Value::V0,
                _ => vcd::Value::X
            }
        }

        // TODO: more optimized VCD output
        while let Ok(v) = upwards.recv() {
            w.timestamp(time).unwrap();
            time += 1;
            for (i, v) in ids.iter().zip(v.into_iter().map(map_value)) {
                w.change_scalar(*i, v).unwrap();
            }
        }
        w.timestamp(time).unwrap();

        true
    }
}

/// Check that a shape matches a VCD scope declaration, creating a mapping from message index to VCD idcode
fn shape_from_scope(s: &Item, v: &vcd::Scope) -> Vec<vcd::IdCode> {
    let mut ids = Vec::new();

    fn inner_tuple(ids: &mut Vec<vcd::IdCode>, shapes: &[Item], scope: &vcd::Scope) {
        for (child_shape, child_scope) in shapes.iter().zip(scope.children.iter()) {
            inner(ids, child_shape, child_scope);
        }
    }

    fn inner(ids: &mut Vec<vcd::IdCode>, shape: &Item, scope_item: &vcd::ScopeItem) {
        match (shape, scope_item) {
            (&Item::Value(..), &vcd::ScopeItem::Var(ref var)) => {
                ids.push(var.code)
            }
            (&Item::Tuple(ref t), &vcd::ScopeItem::Scope(ref scope)) => {
                inner_tuple(ids, &t[..], scope)
            }
            (shape, scope) => {
                panic!("Shape {:?} doesn't match scope {:?}", shape, scope)
            }
        }
    }

    match *s {
        Item::Tuple(ref t) => inner_tuple(&mut ids, &t[..], v),
        ref d @ Item::Value(..) => {
            if let Some(first_child) = v.children.first() {
                inner(&mut ids, d, first_child);
            } else {
                panic!("Shape {:?} doesn't match scope {:?}", s, v)
            }
        }
        _ => unimplemented!()
    }

    ids
}

#[derive(Debug)]
struct VcdRead(Item);
impl Process for VcdRead {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut c = io::BufReader::new(downwards.read_bytes());
        let mut r = vcd::Parser::new(&mut c);
        let h = r.parse_header().unwrap();
        let ids = shape_from_scope(&self.0, &h.scope);

        fn map_value(v: vcd::Value) ->  Value {
            Value::Symbol(match v {
                vcd::Value::V1 => "h",
                vcd::Value::V0 => "l",
                vcd::Value::X => "x",
                vcd::Value::Z => "z",
            }.to_string())
        }

        let mut time = 0;
        let mut next_time;
        let mut values = ids.iter().map(|_| Value::Symbol("x".to_string())).collect::<Vec<_>>();

        while upwards.alive {
            loop {
                match r.next() {
                    Some(Ok(vcd::Command::Timestamp(s))) => {
                        next_time = s;
                        break;
                    }
                    Some(Ok(vcd::Command::ChangeScalar(id, v))) => {
                        for (i, val) in ids.iter().zip(values.iter_mut()) {
                            if *i == id {
                                *val = map_value(v);
                                break;
                            }
                        }
                    }
                    Some(Ok(..)) => panic!("Invalid command in VCD body"),
                    Some(Err(e)) => {
                        println!("VCD error: {}", e);
                        return false;
                    }
                    None => return true
                }
            }

            while time < next_time {
                if upwards.send(values.iter().map(|x| Some(x.clone())).collect()).is_err() {
                    break;
                }
                time += 1;
            }
        }

        true
    }
}

pub fn add_primitives(loader: &Ctxt) {
    loader.define_primitive("with Bytes() def vcd(shape): Seq(shape)", vec![
        PrimitiveDef {
            id: "vcd_write",
            fields_down: Fields::bytes(DataMode { up: false, down: true, }),
            fields_up: PrimitiveDefFields::Auto(DataMode { up: false, down: true, }),
            instantiate: primitive_args!(|shape: &Item| { Ok(Box::new(VcdWrite(shape.clone()))) })
        },
        PrimitiveDef {
            id: "vcd_read",
            fields_down: Fields::bytes(DataMode { up: true, down: false, }),
            fields_up: PrimitiveDefFields::Auto(DataMode { up: true, down: false, }),
            instantiate: primitive_args!(|shape: &Item| { Ok(Box::new(VcdRead(shape.clone()))) })
        }
    ]);
}
