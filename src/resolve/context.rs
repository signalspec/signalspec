use std::cell::RefCell;
use std::vec;
use session::Session;
use eval;
use exec;
use resolve::types::{mod, Shape, ShapeUnknown, ShapeTup, ShapeVal};
use resolve::scope::{ValueRef, Dynamic, Item, ValueItem, ConstantItem, TupleItem};

/// Dynamic Cell
pub type ValueID = uint;

pub struct SignalInfo {
    pub downwards: RefCell<Shape>,
    pub upwards: RefCell<Shape>,
}

impl SignalInfo {
    pub fn new() -> SignalInfo {
        SignalInfo {
            downwards: RefCell::new(ShapeUnknown(false, true)),
            upwards: RefCell::new(ShapeVal(types::TopType, false, true)),
        }
    }
}

pub struct Context<'session> {
    pub session: &'session Session<'session>,
    pub signal_info: &'session SignalInfo,
    pub ops: eval::Ops,
}

impl<'session> Context<'session> {
    pub fn new<'s>(session: &'s Session<'s>, signals: &'s SignalInfo) -> Context<'s> {
        Context {
            session: session,
            signal_info: signals,
            ops: eval::Ops::new(),
        }
    }

    pub fn child(&self) -> Context<'session> {
        Context {
            session: self.session,
            signal_info: self.signal_info,
            ops: eval::Ops::new(),
        }
    }

    pub fn into_ops(self) -> eval::Ops {
        self.ops
    }

    pub fn make_register(&mut self) -> ValueID {
        self.session.make_id()
    }

    pub fn down_op(&mut self, v: eval::ValOp) -> ValueRef {
        let id = self.make_register();
        self.ops.entry.push((id, v));
        Dynamic(id)
    }

    pub fn add_up_op(&mut self, id:ValueID, op: eval::ValOp) {
        self.ops.exit.push((id, op));
    }

    pub fn up_op(&mut self, dest: ValueID, v: |ValueID| -> eval::ValOp) -> ValueRef {
        let cell = self.make_register();
        self.add_up_op(dest, v(cell));
        Dynamic(cell)
    }

    fn item_to_refs(&mut self, item: &Item) -> (types::Type, ValueRef, ValueRef) {
        match *item {
            ConstantItem(ref v) => (
                v.get_type(),
                self.down_op(eval::ConstOp(v.clone())),
                self.up_op(0, |c| eval::CheckOp(c, v.clone()))
            ),
            ValueItem(t, d, u) => (t, d, u),
            _ => fail!("Item does not match shape: expected value, found {}", item),
        }
    }

    /// Create a message downward from the given Item. Checks that the Item matches the Shape,
    /// and fills in the Shape's type information.
    pub fn message_downward(&mut self, item: Item) -> exec::Message {
        let mut shape = self.signal_info.downwards.borrow_mut();

        fn recurse(ctx: &mut Context, shape: &mut Shape, item: Item) -> exec::Message {
            // If the shape is unknown, fill it in based on the item
            if let ShapeUnknown(is_down, is_up) = *shape {
                *shape = match item {
                    TupleItem(ref t) => ShapeTup(Vec::from_elem(t.len(), ShapeUnknown(is_down, is_up))),
                    _ => ShapeVal(types::TopType, is_down, is_up)
                }
            }

            match *shape {
                ShapeVal(ref _ty, _, _) => {
                    let (_t, d, u) = ctx.item_to_refs(&item);
                    exec::MessageValue(d, u)
                }

                ShapeTup(ref mut sub) => {
                    if let TupleItem(t) = item {
                        exec::MessageTuple(
                            t.into_iter().zip(sub.iter_mut())
                            .map(|(i, s)| recurse(ctx, s, i))
                            .collect()
                        )
                    } else {
                        fail!("Item does not match shape: expected tuple, found {}", item)
                    }
                }

                _ => fail!("Unconstrained shape")
            }
        }
        recurse(self, &mut *shape, item)
    }

    pub fn message_upward(&mut self) -> (exec::Message, Item<'session>) {
        let shape = self.signal_info.upwards.borrow();

        fn recurse<'s>(ctx: &mut Context<'s>, shape: &Shape) -> (exec::Message, Item<'s>) {
            match *shape {
                ShapeVal(t, _, _) => {
                    let d = Dynamic(ctx.make_register());
                    let u = Dynamic(ctx.make_register());
                    (exec::MessageValue(d, u), ValueItem(t, d, u))
                }
                ShapeTup(ref v) => {
                    let (ms, is) = vec::unzip(v.iter().map(|s| recurse(ctx, s)));
                    (exec::MessageTuple(ms), TupleItem(is))
                }
                ShapeUnknown(..) => fail!("Signal shape not fully constrained"),
            }
        }

        recurse(self, &*shape)
    }
}
