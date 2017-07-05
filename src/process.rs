use std::{thread, mem};
use protocol::{ Shape, Fields };
use language::{Item, Ctxt};
use connection::Connection;

pub trait Process: Send {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool;
}

pub struct FnProcess<T: Fn(&mut Connection, &mut Connection) -> bool>(pub T);

impl<T: Send + Fn(&mut Connection, &mut Connection) -> bool> Process for FnProcess<T> {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        (self.0)(downwards, upwards)
    }
}

pub struct ProcessInfo {
    pub implementation: Box<Process + 'static>,
    pub shape_up: Shape,
    pub fields_up: Fields,
}

pub struct ProcessStack<'a> {
    loader: &'a Ctxt<'a>,
    processes: Vec<ProcessInfo>,
}

impl<'a> ProcessStack<'a> {
    pub fn new(loader: &'a Ctxt<'a>) -> ProcessStack<'a> {
        ProcessStack {
            processes: vec![],
            loader: loader,
        }
    }

    pub fn top_shape(&self) -> &Shape {
        lazy_static! {
            static ref NULL_SHAPE: Shape = Shape::null();
        }
        self.processes.last().map_or(&NULL_SHAPE, |x| &x.shape_up)
    }

    pub fn top_fields(&self) -> &Fields {
        lazy_static! {
            static ref NULL_FIELDS: Fields = Fields::null();
        }
        self.processes.last().map_or(&NULL_FIELDS, |x| &x.fields_up)
    }

    pub fn add(&mut self, p: ProcessInfo) {
        self.processes.push(p)
    }

    pub fn parse_add(&mut self, call: &str) -> Result<(), String> {
        let process = try!(self.loader.parse_process(call, self.top_shape(), self.top_fields())
            .map_err(|e| e.to_string()));
        self.processes.push(process);
        Ok(())
    }

    pub fn add_print_process(&mut self) {
        let shape = self.top_shape().clone();
        self.add(unimplemented!());
    }

    fn spawn(mut self, bottom: Connection, top: Connection) -> Vec<thread::JoinHandle<bool>> {
        let mut threads = Vec::new();
        let mut connection = bottom;

        let last = self.processes.pop().expect("Spawn requires at least one process");

        for process in self.processes {
            let (c2, upward) = Connection::new(&process.fields_up);
            let downward = mem::replace(&mut connection, c2);
            threads.push(thread::spawn(move || {
                let mut downward = downward;
                let mut upward = upward;
                process.implementation.run(&mut downward, &mut upward)
            }))
        }

        threads.push(thread::spawn(move || {
            let mut connection = connection;
            let mut top = top;
            last.implementation.run(&mut connection, &mut top)
        }));

        threads
    }

    pub fn run(self) -> bool {
        // TODO: should this take &self?
        let (_, bottom) = Connection::new(&Fields::null());
        let (top, _) = Connection::new(&Fields::null());

        let threads = self.spawn(bottom, top);

        let mut success = true;
        for t in threads {
            success &= t.join().unwrap();
        }
        success
    }

    pub fn run_with_flipped(self, other: ProcessStack<'a>) -> bool {
        let (_, first) = Connection::new(&Fields::null());
        let (_, last) = Connection::new(&Fields::null());
        //TODO: assert that self.top_shape() is other.top_shape() with inverse direction
        let (a, b) = Connection::new(self.top_fields());

        let threads1 = self.spawn(first, b);
        let threads2 = other.spawn(last, a);

        let mut success = true;
        for t in threads1.into_iter().chain(threads2.into_iter()) {
            success &= t.join().unwrap();
        }
        success
    }
}
