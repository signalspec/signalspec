use std::cell::RefCell;
use std::vec;
use session::Session;
use eval;
use exec;
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

    pub fn message_downward(&mut self, item: Item) -> exec::Message {
        // TODO: collect information on shape
        fn flatten_into(ctx: &mut Context, item: Item) -> exec::Message {
            // TODO: check shape
            match item {
                ConstantItem(ref v) => {
                    exec::MessageValue(
                        ctx.down_op(eval::ConstOp(v.clone())).value_id(),
                        ctx.up_op(0, |c| eval::CheckOp(c, v.clone())).value_id()
                    )
                }
                ValueItem(_, ref d, ref u) => {
                    exec::MessageValue(d.value_id(), u.value_id())
                }
                TupleItem(t) => {
                    exec::MessageTuple(t.into_iter().map(|i| flatten_into(ctx, i)).collect())
                }
                DefItem(..) => fail!("Cannot flatten non-sendable expression")
            }
        }
        flatten_into(self, item)
    }

    pub fn message_upward(&mut self) -> (exec::Message, Item<'session>) {
        let shape = self.signal_info.upwards.borrow();

        fn recurse<'s>(ctx: &mut Context<'s>, shape: &Shape) -> (exec::Message, Item<'s>) {
            match *shape {
                ShapeVal(t) => {
                    let d = ctx.make_register();
                    let u = ctx.make_register();
                    (exec::MessageValue(Some(u), Some(d)), ValueItem(t, Dynamic(d), Dynamic(u)))
                }
                ShapeTup(ref v) => {
                    let (ms, is) = vec::unzip(v.iter().map(|s| recurse(ctx, s)));
                    (exec::MessageTuple(ms), TupleItem(is))
                }
                ShapeUnknown => fail!("Signal shape not fully constrained"),
            }
        }

        recurse(self, &*shape)
    }
}
