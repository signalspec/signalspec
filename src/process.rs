use std::thread;
use data::Shape;
use language::{Item, ModuleLoader};
use connection::Connection;

pub trait PrimitiveDef: Send {
    fn invoke_def(&self, &Shape, Item) -> Box<Process + 'static>;
}

pub trait Process: Send {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool;
    fn shape_up(&self) -> &Shape;
}



pub struct ProcessStack<'a> {
    loader: &'a ModuleLoader<'a>,
    processes: Vec<Box<Process>>,
}

impl<'a> ProcessStack<'a> {
    pub fn new(loader: &'a ModuleLoader<'a>) -> ProcessStack<'a> {
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

    pub fn run(self) -> bool {
        // TODO: should this take &self?
        let (_, mut connection) = Connection::new(&Shape::null());
        let threads = self.processes.into_iter().map(|p| {
            let (mut c2, c1) = Connection::new(p.shape_up());
            ::std::mem::swap(&mut c2, &mut connection);
            thread::spawn(move || {
                let mut downward = c2;
                let mut upward = c1;
                p.run(&mut downward, &mut upward)
            })
        }).collect::<Vec<_>>();

        let mut success = true;
        for t in threads {
            success &= t.join().unwrap();
        }
        success
    }
}
