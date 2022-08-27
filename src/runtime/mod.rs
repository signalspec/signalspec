mod test_runner;
mod primitives;
mod compile;
mod channel;

use std::sync::Arc;
use std::task::Poll;
use std::future::poll_fn;

use self::channel::SeqChannels;
pub use self::test_runner::run as run_tests_in_file;
pub use self::primitives::{ PrimitiveProcess, add_primitives };
use crate::diagnostic::{ DiagnosticHandler, DiagnosticKind, Label };
use crate::{ Scope };
use crate::syntax::{ SourceFile, FileSpan, parse_process_chain, ast };
pub use channel::{ Channel, ChannelMessage };
use futures_lite::ready;
use peg::error::ParseError;
use peg::str::LineCol;

use crate::core::{ Dir, Index, Item, Shape, compile_process_chain};

pub struct Handle<'a> {
    shape: Option<Shape>,
    channels: SeqChannels,
    future: Option<compile::ProgramExec>,
    parent: Option<&'a mut Handle<'a>>,
}

impl<'a> Handle<'a> {
    pub fn base(index: &Index) -> Handle {
        return Handle {
            shape: Some(base_shape(index)),
            channels: SeqChannels::null(),
            future: None,
            parent: None,
        }
    }

    pub fn shape(&self) -> Option<&Shape> {
        self.shape.as_ref()
    }

    pub fn compile_run<'x: 'a>(&'x mut self, ui: &dyn DiagnosticHandler, index: &Index, scope: &Scope, processes: &[ast::Process]) -> Result<Handle<'x>, ()> {
        let shape = self.shape.as_ref().expect("no shape");
        let program = compile_process_chain(ui, index, scope, shape.clone(), processes);
        let compiled = Arc::new(compile::compile(&program));
        let channels_hi = program.shape_up.as_ref().map_or(SeqChannels::null(), SeqChannels::for_shape);
        let future = compile::ProgramExec::new(compiled, self.channels.clone(), channels_hi.clone() );
        Ok(Handle { shape: program.shape_up, channels: channels_hi, parent: Some(self), future: Some(future) })
    }

    pub fn parse_compile_run<'x: 'a>(&'x mut self, ui: &dyn DiagnosticHandler, index: &Index, call: &str) -> Result<Handle<'x>, ()> {
        let file = Arc::new(SourceFile::new("<process>".into(),  call.into()));
        let scope = Scope::new(file.clone());
        let ast = match parse_process_chain(file.source()) {
            Ok(ast) => ast,
            Err(err) => {
                report_parse_error(ui, file, err);
                return Err(());
            }
        };

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

pub fn compile_run(ui: &dyn DiagnosticHandler, index: &Index, scope: &Scope, processes: &[ast::Process]) -> bool {
    let base_shape = base_shape(index);
    let p = compile_process_chain(ui, index, scope, base_shape, processes);

    let compiled = Arc::new(compile::compile(&p));

    futures_lite::future::block_on(compile::ProgramExec::new(compiled, SeqChannels::null(), SeqChannels::null())).is_ok()
}

fn report_parse_error(ui: &dyn DiagnosticHandler, file: Arc<SourceFile>, err: ParseError<LineCol>) {
    ui.report(
        DiagnosticKind::ParseError,
        vec![
            Label {
                file,
                span: FileSpan::at(err.location.offset),
                label: format!("expected {}", err.expected).into()
            }
        ]
    );
}
