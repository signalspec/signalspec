use crate::syntax::Value;
use super::{ Type, DataMode, Item, Expr, ProtocolRef };

fn count_item_fields(i: &Item) -> usize {
    match *i {
        Item::Value(Expr::Const(_)) => 0,
        Item::Value(_) => 1,
        Item::Tuple(ref t) => t.iter().map(count_item_fields).sum(),
        _ => panic!("Item {:?} not allowed in shape", i),
    }
}

#[derive(Clone, Debug)]
pub struct ShapeMsg {
    pub name: String,
    pub params: Vec<ShapeMsgParam>,
    pub num_fields: usize,
}

#[derive(Clone, Debug)]
pub struct ShapeMsgParam {
    pub item: Item,
    pub direction: DataMode,
}

impl ShapeMsgParam {
    fn count_fields(&self) -> usize {
        if self.direction.down || self.direction.up {
            count_item_fields(&self.item)
        } else { 0 }
    }
}

impl ShapeMsg {
    pub fn new(name: String, params: Vec<ShapeMsgParam>) -> ShapeMsg {
        let num_fields = params.iter().map(|x| x.count_fields()).sum();
        ShapeMsg { name, params, num_fields }
    }
}

/// Representation of token alphabet between state machine layers of abstraction.
#[derive(Clone, Debug)]
pub enum Shape {
    None,
    Seq {
        def: ProtocolRef,
        param: Item,
        messages: Vec<ShapeMsg>,
    },
}


impl Shape {
    pub fn count_fields(&self) -> usize {
        match *self {
            Shape::None => 0,
            Shape::Seq { ref messages, .. } => {
                let tag = if messages.len() <= 1 { 0usize } else { 1usize };
                let inner: usize = messages.iter().map(|m| m.num_fields).sum();
                tag + inner
            }
        }
    }

    pub fn fields(&self) -> Fields {
        match *self {
            Shape::None => Fields::null(),
            Shape::Seq { ref messages, .. } => {
                let mut fields = vec![];
                if messages.len() > 1 {
                    fields.push(Field {
                        ty: Type::Integer(0, messages.len() as i64),
                        is_tag: true,
                        dir: DataMode{ up: true, down: true }
                    });
                }

                fn item_fields(fields: &mut Vec<Field>, i: &Item, dir: DataMode) {
                    match *i {
                        Item::Value(Expr::Const(_)) => (),
                        Item::Value(ref e) => fields.push(Field {
                            ty: e.get_type(),
                            is_tag: false,
                            dir: dir
                        }),
                        Item::Tuple(ref t) => for x in t { item_fields(fields, x, dir) },
                        _ => panic!("Item {:?} not allowed in shape", i),
                    }
                }

                for msg in messages {
                    for param in &msg.params {
                        if param.direction.down || param.direction.up {
                            item_fields(&mut fields, &param.item, param.direction);
                        }
                    }
                }

                Fields::new(fields)
            }
        }
    }

    pub fn has_variant_named(&self, name: &str) -> bool {
        match *self {
            Shape::Seq { ref messages, .. } => messages.iter().any(|m| m.name == name),
            _ => false
        }
    }

    pub fn build_variant_fields<F>(&self, name: &str, with_variant: F) -> Vec<Option<Expr>> where F: FnOnce(&[ShapeMsgParam], &mut dyn FnMut(Expr)) {
        match *self {
            Shape::Seq { ref messages, .. } => {
                messages.iter().position(|m| m.name == name).map(|variant_idx| {
                    let variant = &messages[variant_idx];
                    let mut v = vec![None; self.count_fields()];

                    let mut offset: usize = messages[..variant_idx].iter().map(|m| m.num_fields).sum();

                    if messages.len() > 1 {
                        v[0] = Some(Expr::Const(Value::Integer(variant_idx as i64)));
                        offset += 1;
                    }

                    let mut remaining_fields = variant.num_fields;

                    with_variant(&variant.params[..], &mut |e| {
                        assert!(remaining_fields > 0);
                        v[offset] = Some(e);
                        offset += 1;
                        remaining_fields -= 1;
                    });

                    assert_eq!(remaining_fields, 0);

                    v
                }).unwrap_or_else(|| panic!("Field {:?} not found on shape {:?}", name, self))
            }
            _ => panic!("Invalid shape for build_variant_fields: {:?}", self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub ty: Type,
    pub is_tag: bool,
    pub dir: DataMode
}

#[derive(Clone, PartialEq, Debug)]
pub struct Fields {
    fields: Vec<Field>
}

impl Fields {
    pub fn new(fields: Vec<Field>) -> Fields { Fields { fields } }

    pub fn null() -> Fields { Fields { fields: Vec::new() }}

    pub fn len(&self) -> usize { self.fields.len() }
    pub fn iter(&self) -> ::std::slice::Iter<'_, Field> { self.fields.iter() }
    pub fn iter_mut(&mut self) -> ::std::slice::IterMut<'_, Field> { self.fields.iter_mut() }
    pub fn direction(&self) -> DataMode {
        DataMode {
            up: self.iter().any(|f| f.dir.up && !f.is_tag),
            down: self.iter().any(|f| f.dir.down && !f.is_tag)
        }
    }
}
