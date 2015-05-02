use std::io;
use std::slice::ref_slice;

use exec;
use eval::{self, DataMode};
use session::Process;
use resolve::types::{self, Shape};
use resolve::scope::Item;
use connection_io::{ConnectionRead, ConnectionWrite};
use ast::Value;

extern crate vcd;

fn shape_to_scope(s: &Shape) -> (vcd::Scope, Vec<vcd::IdCode>) {
    let mut ids = Vec::new();

    fn scope(ids: &mut Vec<vcd::IdCode>, l: &[Shape], name: String) -> vcd::Scope {
        vcd::Scope {
            scope_type: vcd::ScopeType::Module,
            identifier: name,
            children: l.iter().enumerate()
                .map(|(i, x)| inner(ids, x, i.to_string())).collect()
        }
    }

    fn inner(ids: &mut Vec<vcd::IdCode>, s: &Shape, name: String) -> vcd::ScopeItem {
        match *s {
            Shape::Val(_, _) => {
                let code = vcd::IdCode::from(ids.len() as u32);
                ids.push(code);

                vcd::ScopeItem::Var( vcd::Var {
                    var_type: vcd::VarType::Wire,
                    size: 1,
                    code: code,
                    reference: name
                })
            }
            Shape::Tup(ref l) => vcd::ScopeItem::Scope(scope(ids, &l[..], name))
        }
    }

    let top = scope(&mut ids, match *s {
        Shape::Val(..) => ref_slice(s),
        Shape::Tup(ref l) => &l[..],
    }, "top".to_string());

    (top, ids)
}

struct VcdDown(Shape);
impl Process for VcdDown {
    fn run(&self, _: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        let mut c = ConnectionWrite(downwards);
        let mut w = vcd::write::Writer::new(&mut c);

        let (scope, ids) = shape_to_scope(&self.0);

        w.header(&vcd::Header {
            scope: scope,
            ..Default::default()
        }).unwrap();

        let mut time = 0u64;

        fn map_value(v: Value) -> vcd::Value {
            match v {
                Value::Symbol(ref v) if &v[..] == "h" => vcd::Value::V1,
                Value::Symbol(ref v) if &v[..] == "l" => vcd::Value::V0,
                _ => vcd::Value::X
            }
        }

        // TODO: more optimized VCD output
        while let Ok(n) = upwards.recv() {
            w.timestamp(time).unwrap();
            time += 1;
            for (i, v) in ids.iter().zip(n.into_iter().map(map_value)) {
                w.change_scalar(*i, v).unwrap();
            }
        }

        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }
}

fn shape_from_scope(s: &Shape, v: &vcd::Scope) -> Vec<vcd::IdCode> {
    let mut ids = Vec::new();

    fn inner_tuple(ids: &mut Vec<vcd::IdCode>, shapes: &[Shape], scope: &vcd::Scope) {
        for (child_shape, child_scope) in shapes.iter().zip(scope.children.iter()) {
            inner(ids, child_shape, child_scope);
        }
    }

    fn inner(ids: &mut Vec<vcd::IdCode>, shape: &Shape, scope_item: &vcd::ScopeItem) {
        match (shape, scope_item) {
            (&Shape::Val(..), &vcd::ScopeItem::Var(ref var)) => {
                ids.push(var.code)
            }
            (&Shape::Tup(ref t), &vcd::ScopeItem::Scope(ref scope)) => {
                inner_tuple(ids, &t[..], scope)
            }
            (shape, scope) => {
                panic!("Shape {:?} doesn't match scope {:?}", shape, scope)
            }
        }
    }

    match s {
        &Shape::Tup(ref t) => inner_tuple(&mut ids, &t[..], v),
        &Shape::Val(..) => {
            if let Some(first_child) = v.children.first() {
                inner(&mut ids, s, first_child);
            } else {
                panic!("Shape {:?} doesn't match scope {:?}", s, v)
            }
        }
    }

    ids
}

struct VcdUp(Shape);
impl Process for VcdUp {
    fn run(&self, _: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        let mut c = io::BufReader::new(ConnectionRead(downwards));
        let mut r = vcd::read::Parser::new(&mut c);
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
                debug!("{} {}", time, next_time);
                if upwards.send(values.clone()).is_err() {
                    break;
                }
                time += 1;
            }
        }

        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }
}

pub fn process(downward_shape: &Shape, arg: Item) -> Box<Process + 'static> {
    let dir = match *downward_shape {
        Shape::Val(types::Integer, dir) => dir,
        _ => panic!("Invalid shape {:?} below vcd::process", downward_shape)
    };

    let upward_shape = arg.into_shape(dir);

    match dir {
        DataMode { down: false, up: true } => box VcdUp(upward_shape),
        DataMode { down: true, up: false } => box VcdDown(upward_shape),
        _ => panic!("Invalid direction {:?} below vcd::process", downward_shape)
    }
}