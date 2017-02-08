use std::{thread, mem};
use protocol::Shape;
use language::{Item, Ctxt};
use connection::Connection;

pub trait PrimitiveDef: Send {
    fn invoke_def(&self, &Shape, Item) -> Box<Process + 'static>;
}

pub trait Process: Send {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool;
    fn shape_up(&self) -> &Shape;
}



pub struct ProcessStack<'a> {
    loader: &'a Ctxt<'a>,
    processes: Vec<Box<Process>>,
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
        self.processes.last().map_or(&NULL_SHAPE, |x| x.shape_up())
    }

    pub fn add(&mut self, p: Box<Process>) {
        self.processes.push(p)
    }

    pub fn parse_add(&mut self, call: &str) -> Result<(), String> {
        let process = try!(self.loader.parse_process(call, self.top_shape())
            .map_err(|e| e.to_string()));
        self.processes.push(process);
        Ok(())
    }

    pub fn add_print_process(&mut self) {
        let shape = self.top_shape().clone();
        self.add(box ::dumpfile::ValueDumpPrint(shape));
    }

    fn spawn(mut self, bottom: Connection, top: Connection) -> Vec<thread::JoinHandle<bool>> {
        let mut threads = Vec::new();
        let mut connection = bottom;

        let last = self.processes.pop().expect("Spawn requires at least one process");

        for process in self.processes {
            let (c2, upward) = Connection::new(process.shape_up());
            let downward = mem::replace(&mut connection, c2);
            threads.push(thread::spawn(move || {
                let mut downward = downward;
                let mut upward = upward;
                process.run(&mut downward, &mut upward)
            }))
        }

        threads.push(thread::spawn(move || {
            let mut connection = connection;
            let mut top = top;
            last.run(&mut connection, &mut top)
        }));

        threads
    }

    pub fn run(self) -> bool {
        // TODO: should this take &self?
        let (_, bottom) = Connection::new(&Shape::null());
        let (top, _) = Connection::new(&Shape::null());

        let threads = self.spawn(bottom, top);

        let mut success = true;
        for t in threads {
            success &= t.join().unwrap();
        }
        success
    }

    pub fn run_with_flipped(self, other: ProcessStack<'a>) -> bool {
        let (_, first) = Connection::new(&Shape::null());
        let (_, last) = Connection::new(&Shape::null());
        //TODO: assert that self.top_shape() is other.top_shape() with inverse direction
        let (a, b) = Connection::new(self.top_shape());

        let threads1 = self.spawn(first, b);
        let threads2 = other.spawn(last, a);

        let mut success = true;
        for t in threads1.into_iter().chain(threads2.into_iter()) {
            success &= t.join().unwrap();
        }
        success
    }
}
