use std::thread;
use std::sync::{Arc, Mutex, MutexGuard};

mod connection;
mod exec;
mod test_runner;
mod primitives;

pub use self::connection::{ Connection, ConnectionMessage };
pub use self::test_runner::run as run_tests_in_file;
pub use self::primitives::{ PrimitiveProcess, add_primitives };
pub use self::exec::run;

use crate::core::{ Ctxt, Config, Index, Shape, Fields, Item, ProcessChain };

pub struct Handle<'a> {
    index: &'a Index,
    config: Config,
    top_shape: Shape,
    top_fields: Fields,
    connection: Arc<Mutex<Connection>>,
    thread: Option<thread::JoinHandle<bool>>,
}

impl<'a> Handle<'a> {
    pub fn new(config: Config, index: &'a Index, shape: Shape, fields: Fields, connection: Connection, thread: Option<thread::JoinHandle<bool>>) -> Handle<'a> {
        Handle {
            index,
            config,
            top_shape: shape,
            top_fields: fields,
            connection: Arc::new(Mutex::new(connection)),
            thread,
        }
    }

    pub fn base(config: Config, index: &'a Index) -> Handle<'a> {
        let base = index.find_protocol("Base").cloned().expect("No `Base` protocol found in prelude");
        let base_shape = Shape::Seq {
            def: base,
            param: Item::Tuple(vec![]),
            messages: vec![],
        };

        Handle::new(config, index, base_shape, Fields::null(), Connection::null(), None)
    }

    pub fn top_shape(&self) -> &Shape { &self.top_shape }
    pub fn top_fields(&self) -> &Fields { &self.top_fields }

    pub fn spawn(&mut self, processes: ProcessChain) -> Handle<'a> {
        let shape_up = processes.shape_up().clone();
        let fields_up = processes.fields_up().clone();

        let (c2, mut c1) = Connection::new(&fields_up);
        let conn = self.connection.clone();

        let thread = thread::spawn(move || {
            let mut conn = conn.try_lock().expect("already in use");
            exec::run(&processes, &mut conn, &mut c1)
        });

        Handle::new(self.config.clone(), self.index, shape_up, fields_up, c2, Some(thread))
    }

    pub fn parse_spawn(&mut self, call: &str) -> Result<Handle<'a>, String> {
        info!("parse_spawn `{}` on {:?}", call, self.top_shape());

        let ctx = Ctxt::new(self.config.clone(), &self.index);
        let process = ctx.parse_process(call, self.top_shape(), self.top_fields())
            .map_err(|e| e.to_string())?;
        info!("parse_spawn starting {:?} {:?}", process.shape_up(), process.fields_up());
        Ok(self.spawn(process))
    }

    pub fn connection(&self) -> MutexGuard<'_, Connection> {
        self.connection.try_lock().expect("already in use")
    }

    pub fn join(self) -> bool {
        self.connection().end();
        self.thread.map_or(true, |t| t.join().unwrap())
    }
}
