use std::cell::RefCell;
use session::Session;
use eval;
use resolve::types::{mod, Shape, ShapeUnknown, ShapeTup, ShapeVal};
use resolve::scope::{ValueRef, Dynamic, Item, ValueItem, ConstantItem, TupleItem, DefItem};

/// Dynamic Cell
pub type ValueID = uint;

pub struct SignalInfo {
    pub downwards: RefCell<Shape>,
    pub upwards: RefCell<Shape>,
}

impl SignalInfo {
    pub fn new() -> SignalInfo {
        SignalInfo {
            downwards: RefCell::new(ShapeUnknown),
            upwards: RefCell::new(ShapeVal(types::TopType)),
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

    pub fn message_downward(&mut self, item: Item) -> (Vec<ValueID>, Vec<ValueID>) {
        let mut down = Vec::new();
        let mut up = Vec::new();

        fn flatten_into(ctx: &mut Context, item: Item, down: &mut Vec<ValueID>, up: &mut Vec<ValueID>) {
            // TODO: check shape
            match item {
                ConstantItem(ref v) => {
                    down.push(ctx.down_op(eval::ConstOp(v.clone())).value_id());
                    up.push(ctx.up_op(0, |c| eval::CheckOp(c, v.clone())).value_id());
                },
                ValueItem(_, ref d, ref u) => {
                    down.push(d.value_id());
                    up.push(u.value_id());
                },
                TupleItem(t) => for i in t.into_iter() { flatten_into(ctx, i, down, up) },
                DefItem(..) => fail!("Cannot flatten non-sendable expression")
            }
        }

        flatten_into(self, item, &mut down, &mut up);
        (down, up)
    }

    pub fn message_upward(&mut self) -> (Vec<ValueID>, Vec<ValueID>, Item<'session>) {
        let shape = self.signal_info.upwards.borrow();
        let len = shape.count().expect("Signal shape not fully constrained");
        let mut down = Vec::with_capacity(len);
        let mut up = Vec::with_capacity(len);

        fn recurse<'s>(ctx: &mut Context<'s>, shape: &Shape, down: &mut Vec<ValueID>, up: &mut Vec<ValueID>) -> Item<'s> {
            match *shape {
                ShapeVal(t) => {
                    let d = ctx.make_register();
                    let u = ctx.make_register();
                    down.push(d);
                    up.push(u);
                    ValueItem(t, Dynamic(d), Dynamic(u))
                }
                ShapeTup(ref v) => {
                    TupleItem(v.iter().map(|s| recurse(ctx, s, down, up) ).collect())
                }
                ShapeUnknown => fail!("Signal shape not fully constrained"),
            }
        }

        let item = recurse(self, &*shape, &mut down, &mut up);
        (down, up, item)
    }
}
