use std::slice;

use crate::syntax::Value;
use crate::core::{ DataMode, Fields, Index, PrimitiveDef, PrimitiveDefFields, Item };
use crate::runtime::{ Connection, PrimitiveProcess };

// This wouldn't need to be a primitive if vectors could contain tuples -- could
// be a simple `for` loop.
pub fn add_primitives(index: &mut Index) {
    index.define_primitive("with Base() def seq(const ty, const #up, const seq): Seq(ty, #up)", vec![
        PrimitiveDef {
            id: "const_seq_up",
            fields_down: Fields::null(),
            fields_up: PrimitiveDefFields::Auto(DataMode { up: true, down: false, }),
            instantiate: primitive_args!(|seq: &Item| {
                Ok(Box::new(SeqUpProcess(item_to_msgs(seq))))
            })
        }
    ]);

    index.define_primitive("with Base() def seq(const ty, const #dn, const seq): Seq(ty, #dn)", vec![
        PrimitiveDef {
            id: "const_seq_down",
            fields_down: Fields::null(),
            fields_up: PrimitiveDefFields::Auto(DataMode { up: false, down: true, }),
            instantiate: primitive_args!(|seq: &Item| {
                Ok(Box::new(SeqDownProcess(item_to_msgs(seq))))
            })
        }
    ]);
}

fn item_to_msgs(item: &Item) -> Vec<Vec<Option<Value>>> {
    let items = match item {
        Item::Tuple(t) => &t[..],
        single => slice::from_ref(single),
    };

    fn inner(m: &mut Vec<Option<Value>>, i: &Item) {
        match i {
            Item::Value(e) => m.push(Some(e.eval_down(&|_| panic!("Runtime variable not expected here")))),
            Item::Tuple(t) => for e in t { inner(m, e) },
            _ => panic!("Item {:?} not allowed in seq literal", i)
        }
    }

    items.iter().map(|i| {
        let mut msg = Vec::new();
        inner(&mut msg, i);
        msg
    }).collect()
}

#[derive(Debug)]
struct SeqUpProcess(Vec<Vec<Option<Value>>>);
impl PrimitiveProcess for SeqUpProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        for i in &self.0 {
            if upwards.send(i.clone()).is_err() {
                return false;
            }
        }
        return true;
    }
}

#[derive(Debug)]
struct SeqDownProcess(Vec<Vec<Option<Value>>>);
impl PrimitiveProcess for SeqDownProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        for i in &self.0 {
            match upwards.recv() {
                Ok(x) if &x == i => continue,
                _ => return false,
            }
        }
        return true;
    }
}