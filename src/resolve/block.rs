use std::collections::BitSet;
use std::cell::RefCell;

use ast;
use session::{Session, ValueID};
use resolve::expr;
pub use exec::{ Step, Message };
pub use resolve::scope::{ Scope, Item };
use resolve::types::{self, Shape, ShapeData, Type};
use eval::{Expr, DataMode};

pub fn resolve_module<'s>(session: &'s Session<'s>, ast: &'s ast::Module) -> &'s RefCell<Scope<'s>> {
    let ref_scope = session.scope_arena.alloc(RefCell::new(session.prelude.clone()));
    let mut scope = ref_scope.borrow_mut();

    for _import in ast.imports.iter() {
        panic!("Imports unimplemented");
    }

    resolve_letdef(session, &mut scope, &ast.lets);

    for def in &ast.defs {
        let ed = Item::Def(def, ref_scope);
        scope.names.insert(def.name.clone(), ed);
    }

    ref_scope
}

pub struct ResolveInfo {
    pub vars_down: BitSet,
    pub vars_up: BitSet,
    pub repeat_up_heuristic: bool,
}

impl ResolveInfo {
    fn new() -> ResolveInfo {
        ResolveInfo {
            vars_down: BitSet::new(),
            vars_up: BitSet::new(),
            repeat_up_heuristic: false,
        }
    }

    fn mode_of(&self, id: ValueID) -> DataMode {
        DataMode { up: self.vars_up.contains(&id), down: self.vars_down.contains(&id)}
    }

    fn use_expr(&mut self, e: &Expr, dir: DataMode) {
        e.each_var(&mut |id| {
            if dir.down { self.vars_down.insert(id); }
            if dir.up { self.vars_up.insert(id); }
        });
        self.repeat_up_heuristic |= dir.up && e.refutable();
    }

    fn merge_seq(&mut self, o: &ResolveInfo) {
        self.vars_down.union_with(&o.vars_down);
        self.vars_up.union_with(&o.vars_up);
        self.repeat_up_heuristic |= o.repeat_up_heuristic;
    }
}

pub fn call<'s>(item: &Item<'s>, session: &'s Session<'s>, shape_down: &Shape, param: Item<'s>) ->
        (Shape, Step, ResolveInfo) {

    match *item {
        Item::Def(ast, scope) => {
            let mut scope = scope.borrow().child(); // Base on lexical parent

            let mut shape_up = if let Some(ref intf_expr) = ast.interface {
                Shape {
                    data: expr::rexpr(session, &scope, intf_expr).into_data_shape(DataMode { down: false, up: true }),
                    child: None,
                }
            } else {
                types::NULL_SHAPE.clone()
            };

            expr::assign(session, &mut scope, &ast.param, param);
            let (step, ri) = resolve_seq(session, &scope, shape_down, &mut shape_up, &ast.block);

            (shape_up, step, ri)
        }
        _ => panic!("Not callable")
    }
}

fn resolve_action<'s>(session: &'s Session<'s>,
                      scope: &Scope<'s>,
                      shape_down: &Shape,
                      shape_up: &mut Shape,
                      action: &ast::Action) -> (Step, ResolveInfo) {
    match *action {
        ast::Action::Seq(ref block) => resolve_seq(session, scope, shape_down, shape_up, block),
        ast::Action::Call(ref expr, ref arg, ref body) => {
            let arg = expr::rexpr(session, scope, arg);

            if body.is_some() {
                unimplemented!();
            }

            let item = expr::rexpr(session, scope, expr);
            let (_shape_child, step, ri) = call(&item, session, shape_down, arg);
            (step, ri)
        }
        ast::Action::Token(ref expr, ref body) => {
            debug!("Token: {:?}", expr);
            if body.is_some() { panic!("Body unimplemented"); }

            let item = expr::rexpr(session, scope, expr);
            let (message, ri) = resolve_token(item, shape_down);

            (Step::Token(message), ri)
        }
        ast::Action::On(ref expr, ref body) => {
            let mut body_scope = scope.child();

            debug!("Upper message, shape: {:?}", shape_up);
            let msg = expr::on_expr_message(session, &mut body_scope, shape_up, expr);

            let (step, mut ri) = body.as_ref().map(|body| {
                resolve_seq(session, &body_scope, shape_down, shape_up, body)
            }).unwrap_or((Step::Nop, ResolveInfo::new()));

            for (e, (_, ref mut dir)) in msg.components.iter().zip(shape_up.values_mut()) {
                if let &Expr::Variable(id, _) = e {
                    let DataMode { up, down } = ri.mode_of(id);
                    dir.up &= up;
                    dir.down |= down;
                }
            }

            ri.repeat_up_heuristic = true;

            (Step::TokenTop(msg, box step), ri)
        }
        ast::Action::Repeat(ref count_ast, ref block) => {
            let count = expr::value(session, scope, count_ast);
            let (step, mut ri) = resolve_seq(session, scope, shape_down, shape_up, block);
            let any_up = ri.repeat_up_heuristic;
            debug!("repeat: any_up {}", any_up);
            ri.use_expr(&count, DataMode { down: !any_up, up: any_up });
            (Step::Repeat(count, box step, any_up), ri)
        }
        ast::Action::For(ref pairs, ref block) => {
            let mut body_scope = scope.child();
            let mut count = None;
            let mut inner_vars = Vec::with_capacity(pairs.len());

            for &(ref name, ref expr) in pairs {
                let e = expr::value(session, scope, expr);
                let t = e.get_type();
                if let Type::Vector(c, box ty) = t {
                    match count {
                        Some(count) => assert_eq!(count, c),
                        None => count = Some(c),
                    }
                    let id = body_scope.new_variable(session, name, ty);
                    inner_vars.push((id, e, DataMode { up: false, down: false}));
                } else {
                    panic!("Foreach must loop over vector type, not {:?}", t)
                }
            }

            debug!("Foreach count: {:?}", count);

            let (step, mut ri) = resolve_seq(session, &body_scope, shape_down, shape_up, block);

            for &mut (id, ref e, ref mut dir) in &mut inner_vars {
                *dir = ri.mode_of(id);
                ri.use_expr(e, *dir);
            }

            (Step::Foreach(count.unwrap_or(0) as u32, inner_vars, box step), ri)
        }
    }
}

pub fn resolve_seq<'s>(session: &'s Session<'s>,
                  pscope: &Scope<'s>,
                  shape_down: &Shape,
                  shape_up: &mut Shape,
                  block: &ast::Block) -> (Step, ResolveInfo) {
    let mut scope = pscope.child();
    resolve_letdef(session, &mut scope, &block.lets);

    let mut ri = ResolveInfo::new();

    let steps = block.actions.iter().map(|action| {
        let (step, i) = resolve_action(session, &scope, shape_down, shape_up, action);
        ri.merge_seq(&i);
        step
    }).collect();

    (Step::Seq(steps), ri)
}

pub fn resolve_letdef<'s>(session: &'s Session<'s>, scope: &mut Scope<'s>, lets: &[ast::LetDef]) {
    for &ast::LetDef(ref name, ref expr) in lets.iter() {
        let item = expr::rexpr(session, scope, expr);
        scope.bind(&name, item);
    }
}

fn resolve_token(item: Item, shape: &Shape) -> (Message, ResolveInfo) {
    let mut state = (
        Message { components: Vec::new() },
        ResolveInfo::new(),
    );

    fn inner<'s>(i: Item<'s>, shape: &ShapeData, state: &mut (Message, ResolveInfo)) {
        match shape {
            &ShapeData::Val(ref _t, dir) => {
                if let Item::Value(v) = i {
                    state.1.use_expr(&v, dir);
                    state.0.components.push(v)
                } else {
                    panic!("Expected value but found {:?}", i);
                }
            }
            &ShapeData::Tup(ref m) => {
                if let Item::Tuple(t) = i {
                    if t.len() == m.len() {
                        for (mi, i) in m.iter().zip(t.into_iter()) {
                            inner(i, mi, state)
                        }
                    } else {
                        panic!("Expected tuple length {}, found {}", m.len(), t.len());
                    }
                } else {
                    panic!("Expected tuple of length {}, found {:?}", m.len(), i);
                }
            }
        }
    }

    inner(item, &shape.data, &mut state);
    state
}
