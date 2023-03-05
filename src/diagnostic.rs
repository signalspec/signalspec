use std::{sync::Arc, fmt::{Display, Debug}, cell::RefCell};
use crate::{SourceFile, FileSpan, syntax::{AstNode, ast}};

pub trait DiagnosticHandler {
    fn report(&self, error: Diagnostic);
    fn report_all(&self, errors: Vec<Diagnostic>) {
        for i in errors { self.report(i) }
    }
}

#[derive(Clone)]
pub struct Span {
    pub file: Arc<SourceFile>,
    pub span: FileSpan,
}

impl Span {
    pub fn new(file: &Arc<SourceFile>, span: FileSpan) -> Self { Self { file: file.clone(), span } }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start_line = self.file.byte_to_line(self.span.start);
        write!(f, "{}:{}", self.file.name(), start_line + 1, )
    }
}

#[derive(Clone)]
pub struct Label<'a> {
    pub span: &'a Span,
    pub label: String,
}

macro_rules! diagnostic_kinds {
    (
        $($variant:ident { $($n:ident : $t:ty),* } => $msg:literal {
            $(error $label:literal at $span:expr)*
        })*
    ) => {
        #[derive(Clone, Debug)]
        pub enum Diagnostic {
            $($variant { $($n: $t),* },)*
        }

        impl Diagnostic {
            pub fn kind(&self) -> &'static str {
                match self {
                    $(Self::$variant { .. } => stringify!($variant),)*
                }
            }

            pub fn message(&self) -> String {
                #[allow(unused_variables)]
                match self {
                    $(Self::$variant { $($n),* } => format!($msg),)*
                }
            }

            pub fn labels(&self) -> Vec<Label> {
                #[allow(unused_variables)]
                match self {
                    $(Self::$variant { $(ref $n),* } => {
                        vec![
                            $(
                                Label {
                                    label: format!($label),
                                    span: $span,
                                },
                            )*
                        ]
                    })*
                }
            }
        }

        impl Display for Diagnostic {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                #[allow(unused_variables)]
                match self {
                    $(Self::$variant { $($n),* } => write!(f, $msg),)*
                }
            }
        }
    };
}

diagnostic_kinds!{
    ParseError {
        span: Span,
        expected: &'static str
    } => "parse error" {
        error "expected {expected}" at span
    }

    NoDefNamed {
        span: Span,
        protocol_name: String,
        def_name: String
    } => "no definition `{def_name}` found for protocol `{protocol_name}`" {
        error "not found" at span
    }

    UndefinedVariable {
        span: Span,
        name: String
    } => "undefined variable `{name}`" {
        error "not found" at span
    }

    ArgsMismatchCount {
        span: Span,
        def_name: String,
        expected: usize,
        found: usize
    } => "wrong number of arguments to `{def_name}`" {
        error "expected {expected} arguments, but found {found}" at span
    }

    ArgMismatchType {
        span: Span,
        def_name: String,
        expected: String,
        found: String
    } => "incorrect argument on `{def_name}`" {
        error "expected `{expected}`, found `{found}`" at span
    }
}

pub struct SimplePrintHandler;

impl DiagnosticHandler for SimplePrintHandler {
    fn report(&self, d: Diagnostic) {
        eprintln!("{d}");
        for l in d.labels() {
            let start_line = l.span.file.byte_to_line(l.span.span.start);
            eprintln!("\t{}:{} {}", l.span.file.name(), start_line + 1, l.label);
        }
    }
}

pub fn report_parse_errors(ui: &dyn DiagnosticHandler, file: &Arc<SourceFile>, ast: &dyn AstNode) -> bool{
    let mut has_errors = false;
    for err in ast.walk_preorder().filter_map(|n| n.downcast::<ast::Error>()) {
        ui.report(
            Diagnostic::ParseError { span: Span::new(file, err.span), expected: err.expected },
        );
        has_errors = true;
    }
    has_errors
}

/// Implementation of DiagnosticHandler that simply collects errors in a Vec
pub struct Collector {
    diagnostics: RefCell<Vec<Diagnostic>>
}

impl Collector {
    pub fn new() -> Self { Self { diagnostics: RefCell::new(Vec::new()) } }

    pub fn diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics.into_inner()
    }
}

impl DiagnosticHandler for Collector {
    fn report(&self, error: Diagnostic) {
        self.diagnostics.borrow_mut().push(error);
    }
}
