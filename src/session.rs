use std::sync::atomic::{AtomicUsize, Ordering};
use typed_arena::Arena;
use std::thread;
use std::cell::RefCell;

use ast;
use resolve;
use resolve::Scope;
use resolve::scope::Item;
use exec::Step;
use data::{ Value, Type, Shape, DataMode };
use grammar;
use eval::Expr;
use exec;
use eval;

pub type ValueID = usize;

/// The data common to an entire resolve pass
pub struct Session<'session> {
    pub scope_arena: Arena<RefCell<Scope<'session>>>,
    pub ast_arena: Arena<ast::Module>,
    id_counter: AtomicUsize,
    pub prelude: Scope<'session>,
}

impl<'session> Session<'session> {
    pub fn new() -> Session<'session> {
        Session {
            scope_arena: Arena::new(),
            ast_arena: Arena::new(),
            id_counter: AtomicUsize::new(1),
            prelude: Scope::new(),
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }

    pub fn parse(&'session self, source: &str) -> Result<&'session ast::Module, grammar::ParseError> {
        grammar::module(source).map(|ast| &*self.ast_arena.alloc(ast))
    }

    pub fn parse_module(&'session self, source: &str) -> Result<Module<'session>, grammar::ParseError> {
         self.parse(source).map(|ast| self.resolve_module(ast))
    }

    pub fn resolve_module(&'session self, modast: &'session ast::Module) -> Module<'session> {
        Module {
            session: self,
            scope: resolve::block::resolve_module(self, modast),
        }
    }

    pub fn var(&self, ty: Type, is_down: bool, is_up: bool) -> Var {
        Var {
            id: self.make_id(),
            is_down: is_down,
            is_up: is_up,
            ty: ty,
        }
    }
}

pub trait IntoItem<'s>: Clone {
    fn into_item(self) -> Item<'s>;
}

impl <'s> IntoItem<'s> for Item<'s> {
    fn into_item(self) -> Item<'s> { self }
}

impl <'s> IntoItem<'s> for Value {
    fn into_item(self) -> Item<'s> { Item::Value(Expr::Const(self)) }
}

impl <'s> IntoItem<'s> for Expr {
    fn into_item(self) -> Item<'s> { Item::Value(self) }
}

#[derive(Clone, Debug)]
pub struct Var {
    pub id: ValueID,
    pub is_down: bool,
    pub is_up: bool,
    pub ty: Type,
}

impl <'s> IntoItem<'s> for Var {
    fn into_item(self) -> Item<'s> {
        let var = Expr::Variable(self.id, self.ty.clone());
        Item::Value(match (self.is_down, self.is_up) {
            (true, true) => var,
            (false, true) => Expr::Flip(box Expr::Ignored, box var),
            (true, false) => Expr::Flip(box var, box Expr::Ignored),
            (false, false) => Expr::Ignored
        })
    }
}


#[macro_escape]
macro_rules! tuple_item[
    ($($x:expr),*) => ({
        use session::IntoItem;
        use resolve::scope::Item::Tuple;
        Tuple(vec![$($x.into_item()),*])
    });
    ($($x:expr,)*) => (tuple_item![$($x),*])
];

impl <'s> IntoItem<'s> for () {
    fn into_item(self) -> Item<'s> { Item::Tuple(Vec::new()) }
}

pub struct Module<'s> {
    session: &'s Session<'s>,
    pub scope: &'s RefCell<Scope<'s>>,
}

impl <'s> Module<'s> {
    pub fn compile_call<T: IntoItem<'s>>(&self, name: &str,
                        shape_down: Shape,
                        param: T) -> Result<Program, ()> {
        if let Some(item) = self.scope.borrow().get(name) {
            let (shape_up, step, _) = resolve::block::call(&item, &self.session, &shape_down, param.into_item());
            Ok(Program{ step: step, shape_down: shape_down, shape_up: shape_up})
        } else {
            Err(())
        }
    }
}

pub struct Program {
    pub step: Step,
    pub shape_down: Shape,
    pub shape_up: Shape,
}

pub trait Process: Send {
    fn run(&self, state: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool;
    fn shape_up(&self) -> &Shape;
}

impl Process for Program {
    fn run(&self, state: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        exec::exec(state, &self.step, downwards, upwards)
    }

    fn shape_up(&self) -> &Shape {
        &self.shape_up
    }
}

pub struct ProgramFlip {
    pub step: Step,
    pub shape_down: Shape,
    pub shape_up: Shape,
}

impl Process for ProgramFlip {
    fn run(&self, state: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        exec::exec(state, &self.step, upwards, downwards)
    }

    fn shape_up(&self) -> &Shape {
        &self.shape_up
    }
}



pub fn resolve_process<'s>(sess: &'s Session<'s>, modscope: &Module<'s>, shape: &Shape, p: &ast::Process) -> Box<Process> {
    use {connection_io, dumpfile, vcd};
    match *p {
        ast::Process::Call(ref name, ref arg) => {
            let arg = resolve::expr::rexpr(sess, &*modscope.scope.borrow(), arg);
            match &name[..] {
                "file" => connection_io::file_process(arg),
                "dump" => dumpfile::process(shape, arg),
                "vcd" => vcd::process(shape, arg),
                name => (box modscope.compile_call(name, shape.clone(), arg)
                  .ok().expect("Failed to compile call"))
            }
        }
        ast::Process::Block(ref block) => {
            let mut shape_up = Shape::null();
            let (step, _) = resolve::block::resolve_seq(sess, &*modscope.scope.borrow(), shape, &mut shape_up, block);

            box Program { step: step, shape_down: shape.clone(), shape_up: shape_up }
        }
        ast::Process::Literal(dir, ref shape_up_expr, ref block) => {
            let shape_item = resolve::expr::rexpr(sess, &*modscope.scope.borrow(), shape_up_expr);
            let is_up = match dir {
                ast::ProcessLiteralDirection::Up => true,
                ast::ProcessLiteralDirection::Down => false,
                ast::ProcessLiteralDirection::Both => panic!("@both is only usable in tests"),
            };
            let shape_up = shape_item.clone().into_shape(sess, DataMode { down: !is_up, up: is_up });
            let shape_flip = shape_item.into_shape(sess, DataMode { down: is_up, up: !is_up });

            let mut shape_dn = Shape::null();
            let (step, _) = resolve::block::resolve_seq(sess, &*modscope.scope.borrow(), &shape_flip, &mut shape_dn, block);

            box ProgramFlip { step: step, shape_down: shape_dn, shape_up: shape_up }
        }
    }
}

pub fn run_process_chain(processes: Vec<Box<Process>>) -> bool {
    let (_, mut connection) = exec::Connection::new(&Shape::null());
    let threads = processes.into_iter().map(|p| {
        let (mut c2, c1) = exec::Connection::new(p.shape_up());
        ::std::mem::swap(&mut c2, &mut connection);
        thread::spawn(move || {
            let mut downward = c2;
            let mut upward = c1;
            let mut state = eval::State::new();
            p.run(&mut state, &mut downward, &mut upward)
        })
    }).collect::<Vec<_>>();

    threads.into_iter().all(|x| x.join().unwrap())
}
