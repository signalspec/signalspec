use std::sync::{Arc, Mutex, MutexGuard};
use std::fmt::Debug;
use protocol::{ Shape, Fields };
use language::{ Ctxt, Item, StepInfo, Message };
use connection::Connection;
use std::thread;

pub trait PrimitiveProcess: Debug + Send + Sync {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool;
}

pub struct FnProcess<T: Fn(&mut Connection, &mut Connection) -> bool>(pub T, pub &'static str);

impl<T: Sync + Send + Fn(&mut Connection, &mut Connection) -> bool> PrimitiveProcess for FnProcess<T> {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        (self.0)(downwards, upwards)
    }
}

impl<T: Sync +Send + Fn(&mut Connection, &mut Connection) -> bool> Debug for FnProcess<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        write!(f, "{}", self.1)
    }
}

#[derive(Debug)]
pub enum Process {
    Token(Message),
    Seq(StepInfo),
    Primitive(Box<PrimitiveProcess + 'static>),
}

#[derive(Debug)]
pub struct ProcessInfo {
    pub process: Process,
    pub shape_up: Shape,
    pub fields_up: Fields,
}

#[derive(Debug)]
pub struct ProcessChain {
    pub processes: Vec<ProcessInfo>
}

impl ProcessChain {
    pub fn fields_up(&self) -> &Fields {
        &self.processes.last().unwrap().fields_up
    }

    pub fn shape_up(&self) -> &Shape {
        &self.processes.last().unwrap().shape_up
    }
}

pub struct Handle<'a> {
    loader: &'a Ctxt,
    top_shape: Shape,
    top_fields: Fields,
    connection: Arc<Mutex<Connection>>,
    thread: Option<thread::JoinHandle<bool>>,
}

impl<'a> Handle<'a> {
    pub fn new(loader: &'a Ctxt, shape: Shape, fields: Fields, connection: Connection, thread: Option<thread::JoinHandle<bool>>) -> Handle<'a> {
        Handle {
            top_shape: shape,
            top_fields: fields,
            loader,
            connection: Arc::new(Mutex::new(connection)),
            thread,
        }
    }

    pub fn base(loader: &'a Ctxt) -> Handle<'a> {
        let base_item = loader.prelude.borrow().get("Base").expect("No `Base` protocol found in prelude");
        let base_id = if let Item::Protocol(id) = base_item { id } else { panic!("`Base` is not a protocol")};
        let base_shape = Shape::Seq {
            def: base_id,
            param: Item::Tuple(vec![]),
            messages: vec![],
        };

        Handle::new(loader, base_shape, Fields::null(), Connection::null(), None)
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
            ::language::run(&processes, &mut conn, &mut c1)
        });

        Handle::new(self.loader, shape_up, fields_up, c2, Some(thread))
    }

    pub fn parse_spawn(&mut self, call: &str) -> Result<Handle<'a>, String> {
        info!("parse_spawn `{}` on {:?}", call, self.top_shape());
        let process = try!(self.loader.parse_process(call, self.top_shape(), self.top_fields())
            .map_err(|e| e.to_string()));
        info!("parse_spawn starting {:?} {:?}", process.shape_up(), process.fields_up());
        Ok(self.spawn(process))
    }

    pub fn connection(&self) -> MutexGuard<Connection> {
        self.connection.try_lock().expect("already in use")
    }

    pub fn join(self) -> bool {
        self.connection().end();
        self.thread.map_or(true, |t| t.join().unwrap())
    }
}
