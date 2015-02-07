use std::sync::atomic::{AtomicUint, Ordering};
use arena::{TypedArena};

use ast;
use resolve;
use resolve::Scope;
use resolve::block::EventClosure;
use exec::Step;
use ast::Value;
use resolve::scope::{ Item, ValueRef, Dynamic, Ignored };
use resolve::types::Shape;
use data_usage;
use grammar;

use std::str;
use std::old_io::{IoResult, MemReader, MemWriter};
use std::thread::Thread;
use exec;
use dumpfile;
use eval;

/// The data common to an entire resolve pass
pub struct Session<'session> {
    pub closure_arena: TypedArena<EventClosure<'session>>,
    module_ast_arena: TypedArena<ast::Module>,
    id_counter: AtomicUint,
    pub prelude: Scope<'session>,
}

impl<'session> Session<'session> {
    pub fn new() -> Session<'session> {
        Session {
            closure_arena: TypedArena::new(),
            module_ast_arena: TypedArena::new(),
            id_counter: AtomicUint::new(1),
            prelude: Scope::new(),
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }

    pub fn parse_module(&'session self, source: &str) -> Result<Module<'session>, String> {
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

    pub fn var(&self, ty: resolve::types::Type, is_down: bool, is_up: bool) -> Var {
        Var {
            ty: ty,
            down: if is_down { Dynamic(self.make_id()) } else { Ignored },
            up:   if is_up   { Dynamic(self.make_id()) } else { Ignored },
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
    fn into_item(self) -> Item<'s> { Item::Constant(self) }
}

#[derive(Copy, Clone, Debug)]
pub struct Var {
    pub ty: resolve::types::Type,
    pub down: ValueRef,
    pub up: ValueRef,
}

impl<'s> IntoItem<'s> for Var {
    fn into_item(self) -> Item<'s> { Item::Value(self.ty, self.down, self.up) }
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
                        shape_down: Shape, shape_up: Shape,
                        param: T) -> Result<Program, ()> {
        let def = self.get_def(name);
        let mut signal_info = resolve::SignalInfo { downwards: shape_down, upwards: shape_up };
        let mut ctx = resolve::Context::new(self.session);

        let mut step = def.resolve_call(&mut ctx, &mut signal_info, param.into_item(), None);
        data_usage::pass(&mut step, &mut signal_info);

        Ok(Program{ step: step, signals: signal_info })
    }
}

pub struct Program {
    pub step: Step,
    pub signals: resolve::SignalInfo,
}

impl Program {
    pub fn run_test(&self, bottom: &'static str, top: &'static str) -> (bool, eval::State) {
        let (mut s1, mut s2) = exec::Connection::new(&self.signals.downwards);
        let (mut t1, mut t2) = exec::Connection::new(&self.signals.upwards);
        let reader_thread = Thread::scoped(move || {
            let mut reader = MemReader::new(bottom.as_bytes().to_vec());
            dumpfile::read_values(&mut reader, &mut s2);
        });
        let writer_thread = Thread::scoped(move || {
            let mut writer = MemWriter::new();
            dumpfile::write_values(&mut writer, &mut t1);
            let v = writer.into_inner();
            assert_eq!(top, str::from_utf8(&v[]).unwrap());
        });
        let mut state = eval::State::new();

        let r = exec::exec(&mut state, &self.step, &mut s1, &mut t2);

        drop(s1);
        drop(t2);
        reader_thread.join().ok().unwrap();
        writer_thread.join().ok().unwrap();

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

pub trait Process {
    fn run(&self, state: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool;
}

impl Process for Program {
    fn run(&self, state: &mut eval::State, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        exec::exec(state, &self.step, downwards, upwards)
    }
}

pub trait IoProcess {
    fn run(&mut self, upwards: &mut exec::Connection) -> IoResult<()>;
}
