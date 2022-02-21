use std::{sync::Arc, borrow::Cow, fmt::Display};

use crate::{SourceFile, FileSpan};

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