use std::sync::atomic::{AtomicUsize, Ordering};
use arena::{TypedArena};

use ast;
use resolve;
use resolve::Scope;
use resolve::block::EventClosure;
use exec::Step;
use ast::Value;
use resolve::scope::Item;
use resolve::types::Shape;
use grammar;
use eval::Expr;

use std::str;
use std::io::{self, Cursor};
use std::thread;
use exec;
use dumpfile;
use eval;

pub type ValueID = usize;

/// The data common to an entire resolve pass
pub struct Session<'session> {
    pub closure_arena: TypedArena<EventClosure<'session>>,
    module_ast_arena: TypedArena<ast::Module>,
    id_counter: AtomicUsize,
    pub prelude: Scope<'session>,
}

impl<'session> Session<'session> {
    pub fn new() -> Session<'session> {
        Session {
            closure_arena: TypedArena::new(),
            module_ast_arena: TypedArena::new(),
            id_counter: AtomicUsize::new(1),
            prelude: Scope::new(),
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }

    pub fn parse_module(&'session self, source: &str) -> Result<Module<'session>, grammar::ParseError> {
        grammar::module(source).map(|ast|
            self.resolve_module(self.module_ast_arena.alloc(ast))
        )
    }

    pub fn resolve_module(&'session self, modast: &'session ast::Module) -> Module<'session> {
        Module {
            session: self,
            scope: resolve::block::resolve_module(self, modast),
        }
    }

    pub fn var(&self, _ty: resolve::types::Type, is_down: bool, is_up: bool) -> Var {
        Var {
            id: self.make_id(),
            is_down: is_down,
            is_up: is_up,
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

#[derive(Copy, Clone, Debug)]
pub struct Var {
    pub id: ValueID,
    pub is_down: bool,
    pub is_up: bool,
}

impl <'s> IntoItem<'s> for Var {
    fn into_item(self) -> Item<'s> {
        let var = Expr::Variable(self.id);
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
    scope: Scope<'s>,
}


impl <'s> Module<'s> {
    pub fn get_def<'a>(&'a self, name: &str) -> &'a EventClosure<'s> {
        // TODO: return Result
        match self.scope.get(name).unwrap() {
            resolve::scope::Item::Def(s) => s,
            _ => panic!("Main is not an event"),
        }
    }

    pub fn compile_call<T: IntoItem<'s>>(&self, name: &str,
                        shape_down: Shape,
                        param: T) -> Result<Program, ()> {
        let def = self.get_def(name);

        let (shape_up, step) = def.resolve_call(&self.session, &shape_down, param.into_item());

        Ok(Program{ step: step, shape_down: shape_down, shape_up: shape_up})
    }
}

pub struct Program {
    pub step: Step,
    pub shape_down: Shape,
    pub shape_up: Shape,
}

impl Program {
    pub fn run_test(&self, bottom: &'static str, top: &'static str) -> (bool, eval::State) {
        let (mut s1, mut s2) = exec::Connection::new(self.shape_down.data_mode());
        let (mut t1, mut t2) = exec::Connection::new(self.shape_up.data_mode());
        let reader_thread = thread::scoped(move || {
            let mut reader = Cursor::new(bottom.as_bytes().to_vec());
            dumpfile::read_values(&mut reader, &mut s2);
        });
        let writer_thread = thread::scoped(move || {
            let mut writer = Vec::new();
            dumpfile::write_values(&mut writer, &mut t1);
            assert_eq!(top, str::from_utf8(&writer).unwrap());
        });
        let mut state = eval::State::new();

        let r = exec::exec(&mut state, &self.step, &mut s1, &mut t2);

        drop(s1);
        drop(t2);
        reader_thread.join();
        writer_thread.join();

        (r, state)
    }

    pub fn run_test_pass(&self, bottom: &'static str, top: &'static str) -> eval::State {
        let (m, r) = self.run_test(bottom, top);
        if !m {
            panic!("run_pass test failed to match: {}", bottom);
        }
        r
    }

    pub fn run_test_fail(&self, bottom: &'static str, top: &'static str) -> eval::State {
        let (m, r) = self.run_test(bottom, top);
        if m {
            panic!("run_fail test matched: {}", bottom);
        }
        r
    }
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

pub trait IoProcess {
    fn run(&mut self, upwards: &mut exec::Connection) -> io::Result<()>;
}
