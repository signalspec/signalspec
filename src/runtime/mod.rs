mod test_runner;
mod primitives;
mod compile;
mod channel;

use std::sync::Arc;
use std::task::Poll;
use std::future::poll_fn;

use self::channel::SeqChannels;
pub use self::test_runner::run as run_tests;
pub use self::test_runner::run_file as run_tests_in_file;
pub use self::primitives::{ PrimitiveProcess, add_primitives };
use crate::diagnostic::{DiagnosticHandler, Collector};
use crate::{ Scope, ShapeMsg };
use crate::syntax::{ SourceFile, parse_process, ast };
pub use channel::{ Channel, ChannelMessage };
use futures_lite::ready;

use crate::core::{ Dir, Index, Item, Shape, compile_process, ShapeMsgParam};

pub struct Handle<'a> {
    shape: Option<Shape>,
    channels: SeqChannels,
    future: Option<compile::ProgramExec>,
    parent: Option<&'a mut Handle<'a>>,
}

impl<'a> Handle<'a> {
    pub fn base(index: &Index) -> Handle {
        Handle {
            shape: Some(base_shape(index)),
            channels: SeqChannels::null(),
            future: None,
            parent: None,
        }
    }

    pub fn seq_dn(index: &Index, ty: Item) -> (Channel, Handle) {
        let shape = seq_shape(index, ty, Dir::Dn);
        let channels = SeqChannels::for_shape(&shape);
        (channels.dn.as_ref().unwrap().clone(), Handle { shape: Some(shape), channels, future: None , parent: None})
    }

    pub fn seq_up(index: &Index, ty: Item) -> (Channel, Handle) {
        let shape = seq_shape(index, ty, Dir::Up);
        let channels = SeqChannels::for_shape(&shape);
        (channels.up.as_ref().unwrap().clone(), Handle { shape: Some(shape), channels, future: None , parent: None})
    }

    pub fn shape(&self) -> Option<&Shape> {
        self.shape.as_ref()
    }

    pub fn compile_run<'x: 'a>(&'x mut self, ui: &dyn DiagnosticHandler, index: &Index, scope: &Scope, process: &ast::Process) -> Result<Handle<'x>, ()> {
        let shape = self.shape.as_ref().expect("no shape");
        let errors = Collector::new();
        let program = compile_process(&errors, index, scope, shape.clone(), process);

        let errors = errors.diagnostics();
        if !errors.is_empty() {
            ui.report_all(errors);
            return Err(())
        }

        let compiled = Arc::new(compile::compile(&program));
        let channels_hi = program.shape_up.as_ref().map_or(SeqChannels::null(), SeqChannels::for_shape);
        let future = compile::ProgramExec::new(compiled, self.channels.clone(), channels_hi.clone() );
        Ok(Handle { shape: program.shape_up, channels: channels_hi, parent: Some(self), future: Some(future) })
    }

    pub fn parse_compile_run<'x: 'a>(&'x mut self, ui: &dyn DiagnosticHandler, index: &Index, call: &str) -> Result<Handle<'x>, ()> {
        let file = Arc::new(SourceFile::new("<process>".into(),  call.into()));
        let scope = Scope::new(file.clone());
        let ast = parse_process(file.source()).expect("parser failed");
        if crate::diagnostic::report_parse_errors(ui, &file, &ast) {
            return Err(())
        }
        self.compile_run(ui, index, &scope, &ast)
    }

    fn poll(&mut self, cx: &mut std::task::Context) -> Poll<Result<(), ()>> {
        if let Some(future) = &mut self.future {
            if let Poll::Ready(v) = future.poll_all(cx) {
                return Poll::Ready(v)
            }
        }
        if let Some(parent) = &mut self.parent {
            if let Poll::Ready(Result::Err(e)) = parent.poll(cx) {
                return Poll::Ready(Result::Err(e));
            }
        }
        Poll::Pending
    }

    pub fn finish(mut self) -> Result<(), ()> {
        if let Some(ref ch) = self.channels.dn {
            ch.set_closed(true);
        }
        futures_lite::future::block_on(poll_fn(|cx| { self.poll(cx) }))
    }

    pub fn receive(&mut self) -> Option<ChannelMessage> {
        futures_lite::future::block_on(poll_fn(|cx| {
            let _ = self.poll(cx);
            if let Some(ch) = self.channels.up.as_ref() {
                ready!(ch.poll_receive(cx));
                Poll::Ready(ch.read().pop())
            } else {
                Poll::Ready(None)
            }
        }))
    }

    pub fn send(&mut self, msg: ChannelMessage) {
        self.channels.dn.as_ref().expect("this handle does not have a downward channel").send(msg);
    }
}

fn base_shape(index: &Index) -> Shape {
    let base = index.find_protocol("Base").cloned().expect("No `Base` protocol found in prelude");
    Shape {
        def: base,
        param: Item::Tuple(vec![]),
        dir: Dir::Dn,
        messages: vec![],
    }
}

fn seq_shape(index: &Index, ty_item: Item, dir: Dir) -> Shape {
    let base = index.find_protocol("Seq").cloned().expect("No `Seq` protocol found in prelude");

    let ty = ty_item.as_type_tree().expect("not a type");

    Shape {
        def: base,
        param: Item::Tuple(vec![ty_item, Item::symbol(match dir { Dir::Dn => "dn", Dir::Up => "up"})]),
        dir,
        messages: vec![
            ShapeMsg {
                name: "val".into(),
                params: vec![ShapeMsgParam { ty, direction: dir }]
            }
        ],
    }
}
