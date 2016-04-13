use std::thread;
use data::Shape;
use language::Item;
use connection::Connection;

pub trait PrimitiveDef: Send {
    fn invoke_def(&self, &Shape, Item) -> Box<Process + 'static>;
}

pub trait Process: Send {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool;
    fn shape_up(&self) -> &Shape;
}

pub fn run_process_chain(processes: Vec<Box<Process>>) -> bool {
    let (_, mut connection) = Connection::new(&Shape::null());
    let threads = processes.into_iter().map(|p| {
        let (mut c2, c1) = Connection::new(p.shape_up());
        ::std::mem::swap(&mut c2, &mut connection);
        thread::spawn(move || {
            let mut downward = c2;
            let mut upward = c1;
            p.run(&mut downward, &mut upward)
        })
    }).collect::<Vec<_>>();

    threads.into_iter().all(|x| x.join().unwrap())
}
