use session::Session;
use eval;
use exec;
use resolve::types::{ self, Shape };
use resolve::scope::{ ValueRef, Dynamic, Item };

/// Dynamic Cell
pub type ValueID = usize;

#[derive(Show)]
pub struct SignalInfo {
    pub downwards: Shape,
    pub upwards: Shape,
}

pub struct Context<'session> {
    pub session: &'session Session<'session>,
    pub ops: eval::Ops,
}

impl<'session> Context<'session> {
    pub fn new<'s>(session: &'s Session<'s>) -> Context<'s> {
        Context {
            session: session,
            ops: eval::Ops::new(),
        }
    }

    pub fn child(&self) -> Context<'session> {
        Context {
            session: self.session,
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

    pub fn up_op<F: FnOnce(ValueID) -> eval::ValOp>(&mut self, dest: ValueID, v: F) -> ValueRef {
        let cell = self.make_register();
        self.add_up_op(dest, v(cell));
        Dynamic(cell)
    }

    pub fn item_to_refs(&mut self, item: &Item) -> (types::Type, ValueRef, ValueRef) {
        match *item {
            Item::Constant(ref v) => (
                v.get_type(),
                self.down_op(eval::ValOp::Const(v.clone())),
                self.up_op(0, |c| eval::ValOp::Check(c, v.clone()))
            ),
            Item::Value(t, d, u) => (t, d, u),
            _ => panic!("Item does not match shape: expected value, found {:?}", item),
        }
    }

    /// Create a message downward from the given Item. Checks that the Item matches the Shape,
    /// and fills in the Shape's type information.
    pub fn message_downward(&mut self, signals: &mut SignalInfo, item: Item) -> exec::Message {
        let shape = &mut signals.downwards;

        fn recurse(ctx: &mut Context, shape: &mut Shape, item: Item) -> exec::Message {
            // If the shape is unknown, fill it in based on the item
            if let Shape::Unknown(is_down, is_up) = *shape {
                *shape = match item {
                    Item::Tuple(ref t) => Shape::Tup(t.iter().map(|_| Shape::Unknown(is_down, is_up)).collect()),
                    _ => Shape::Val(types::Bottom, is_down, is_up)
                }
            }

            match *shape {
                Shape::Val(ref _ty, _, _) => {
                    let (_t, d, u) = ctx.item_to_refs(&item);
                    exec::Message::Value(d, u)
                }

                Shape::Tup(ref mut sub) => {
                    if let Item::Tuple(t) = item {
                        exec::Message::Tuple(
                            t.into_iter().zip(sub.iter_mut())
                            .map(|(i, s)| recurse(ctx, s, i))
                            .collect()
                        )
                    } else {
                        panic!("Item does not match shape: expected tuple, found {:?}", item)
                    }
                }

                _ => panic!("Unconstrained shape")
            }
        }
        recurse(self, &mut *shape, item)
    }

    pub fn message_upward(&mut self, signals: &mut SignalInfo) -> (exec::Message, Item<'session>) {
        let shape = &mut signals.upwards;

        fn recurse<'s>(ctx: &mut Context<'s>, shape: &Shape) -> (exec::Message, Item<'s>) {
            match *shape {
                Shape::Val(t, _, _) => {
                    let d = Dynamic(ctx.make_register());
                    let u = Dynamic(ctx.make_register());
                    (exec::Message::Value(d, u), Item::Value(t, d, u))
                }
                Shape::Tup(ref v) => {
                    let (ms, is) = v.iter().map(|s| recurse(ctx, s)).unzip();
                    (exec::Message::Tuple(ms), Item::Tuple(is))
                }
                Shape::Unknown(..) => panic!("Signal shape not fully constrained"),
            }
        }

        recurse(self, &*shape)
    }
}
