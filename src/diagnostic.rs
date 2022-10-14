use std::{sync::Arc, borrow::Cow, fmt::Display, cell::RefCell};

use crate::{SourceFile, FileSpan, syntax::{AstNode, ast}};

pub trait DiagnosticHandler {
    fn report(&self,
        error: DiagnosticKind,
        labels: Vec<Label>,
    );
}

#[derive(Clone)]
pub struct Label {
    pub file: Arc<SourceFile>,
    pub span: FileSpan,
    pub label: Cow<'static, str>,
}

macro_rules! diagnostic_kinds {
    (
        $($variant:ident $({ $($n:ident : $t:ty),* })? => $msg:literal)*
    ) => {
        #[derive(Clone, Debug)]
        pub enum DiagnosticKind {
            $($variant $({ $($n: $t),* })?,)*
        }

        impl Display for DiagnosticKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$variant $({ $($n),* })? => write!(f, $msg),)*
                }
            }
        }
    };
}

diagnostic_kinds!{
    ParseError => "parse error"
    NoDefNamed { protocol_name: String, def_name: String } => "no definition named `{def_name}` found for protocol `{protocol_name}`"
}

pub struct SimplePrintHandler;

impl DiagnosticHandler for SimplePrintHandler {
    fn report(&self,
        error: DiagnosticKind,
        labels: Vec<Label>,
    ) {
        eprintln!("{error}");
        for l in &labels {
            let start_line = l.file.byte_to_line(l.span.start);
            eprintln!("\t{}:{} {}", l.file.name(), start_line + 1, l.label);
        }
    }
}

pub fn report_parse_errors(ui: &dyn DiagnosticHandler, file: &Arc<SourceFile>, ast: &dyn AstNode) -> bool{
    let mut has_errors = false;
    for err in ast.walk_preorder().filter_map(|n| n.downcast::<ast::Error>()) {
        ui.report(
            DiagnosticKind::ParseError,
            vec![
                Label {
                    file: file.clone(),
                    span: err.span,
                    label: format!("expected {}", err.expected).into()
                }
            ]
        );
        has_errors = true;
    }
    has_errors
}

/// Implementation of DiagnosticHandler that simply collects errors in a Vec
pub struct Collector {
    diagnostics: RefCell<Vec<(DiagnosticKind, Vec<Label>)>>
}

impl Collector {
    pub fn new() -> Self { Self { diagnostics: RefCell::new(Vec::new()) } }

    pub fn diagnostics(self) -> Vec<(DiagnosticKind, Vec<Label>)> {
        self.diagnostics.into_inner()
    }
}

impl DiagnosticHandler for Collector {
    fn report(&self,
        error: DiagnosticKind,
        labels: Vec<Label>,
    ) {
        self.diagnostics.borrow_mut().push((error, labels));
    }
}
