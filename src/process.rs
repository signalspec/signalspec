use std::mem;
use std::fmt::Debug;
use protocol::{ Shape, Fields };
use language::{ Ctxt, Item, Step, StepInfo };
use connection::Connection;

pub trait Process: Debug + Send + Sync {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool;
}

pub struct FnProcess<T: Fn(&mut Connection, &mut Connection) -> bool>(pub T, pub &'static str);

impl<T: Sync + Send + Fn(&mut Connection, &mut Connection) -> bool> Process for FnProcess<T> {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        (self.0)(downwards, upwards)
    }
}

impl<T: Sync +Send + Fn(&mut Connection, &mut Connection) -> bool> Debug for FnProcess<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        write!(f, "{}", self.1)
    }
}

pub struct ProcessStack<'a> {
    loader: &'a Ctxt<'a>,
    step: Option<StepInfo>,
    top_shape: Shape,
    top_fields: Fields,
}

pub struct ProcessInfo {
    pub step: StepInfo,
    pub shape_up: Shape,
    pub fields_up: Fields,
}

impl<'a> ProcessStack<'a> {
    pub fn new(loader: &'a Ctxt<'a>) -> ProcessStack<'a> {
        let base_item = loader.prelude.borrow().get("Base").expect("No `Base` protocol found in prelude");
        let base_id = if let Item::Protocol(id) = base_item { id } else { panic!("`Base` is not a protocol")};
        let base_shape = Shape::Seq {
            def: base_id,
            param: Item::Tuple(vec![]),
            messages: vec![],
        };

        ProcessStack::with_shape(loader, base_shape, Fields::null())
    }

    pub fn with_shape(loader: &'a Ctxt<'a>, shape: Shape, fields: Fields) -> ProcessStack<'a> {
        ProcessStack {
            step: None,
            top_shape: shape,
            top_fields: fields,
            loader,
        }
    }

    pub fn top_shape(&self) -> &Shape { &self.top_shape }
    pub fn top_fields(&self) -> &Fields { &self.top_fields }

    pub fn add(&mut self, p: ProcessInfo) {
        let top_fields = mem::replace(&mut self.top_fields, p.fields_up);
        self.top_shape = p.shape_up;

        if let Some(existing_step) = self.step.take() {
            self.step = Some(StepInfo::fake(Step::Fork(Box::new(existing_step), top_fields, Box::new(p.step))));
        } else {
            self.step = Some(p.step);
        }
    }

    pub fn parse_add(&mut self, call: &str) -> Result<(), String> {
        let process = try!(self.loader.parse_process(call, self.top_shape(), self.top_fields())
            .map_err(|e| e.to_string()));
        self.add(process);
        Ok(())
    }

    pub fn run(&self, bottom: &mut Connection, top: &mut Connection) -> bool {
        if let Some(ref step) = self.step {
            ::language::run(&step, bottom, top)
        } else {
            true
        }
    }
}
