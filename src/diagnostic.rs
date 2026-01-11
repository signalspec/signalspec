use crate::{
    syntax::{ast, AstNode, Number},
    FileSpan, SourceFile, Type,
};
use std::{
    fmt::{Debug, Display},
    ptr,
    sync::Arc,
};

pub type Diagnostics = Vec<Diagnostic>;

pub fn print_diagnostics(diagnostics: Diagnostics) {
    for d in diagnostics {
        eprintln!("{d}");
        for l in d.labels() {
            let start_line = l.span.file.byte_to_line(l.span.span.start);
            eprintln!("\t{}:{} {}", l.span.file.name(), start_line + 1, l.label);
        }
    }
}

pub struct DiagnosticContext {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticContext {
    pub fn new() -> DiagnosticContext {
        DiagnosticContext {
            diagnostics: Vec::new(),
        }
    }

    pub fn report(&mut self, error: Diagnostic) -> ErrorReported {
        self.diagnostics.push(error);
        ErrorReported::error_reported()
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }
}

/// Sentinel value returned by `report` to serve as a type-system check that an error was already reported
#[derive(Clone, PartialEq, Eq)]
pub struct ErrorReported(());

impl ErrorReported {
    /// Provide evidence that an error has been reported
    pub fn error_reported() -> Self {
        ErrorReported(())
    }

    /// It can be assumed that AST error nodes have been reported
    pub fn from_ast(_: &ast::Error) -> Self {
        Self::error_reported()
    }
}

impl Debug for ErrorReported {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error previously reported")
    }
}

#[derive(Clone)]
pub struct Span {
    pub file: Arc<SourceFile>,
    pub span: FileSpan,
}

impl Span {
    pub fn new(file: &Arc<SourceFile>, span: FileSpan) -> Self {
        Self {
            file: file.clone(),
            span,
        }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start_line = self.file.byte_to_line(self.span.start);
        write!(f, "{}:{}", self.file.name(), start_line + 1,)
    }
}

#[derive(Clone)]
pub struct Label<'a> {
    pub span: &'a Span,
    pub label: String,
}

macro_rules! diagnostic_kinds {
    (
        $($variant:ident { $($n:ident : $t:ty),* } => $msg:literal at $labels:expr)*
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

            pub fn labels(&self) -> Vec<Label<'_>> {
                #[allow(unused_variables)]
                match self {
                    $(Self::$variant { $($n),* } => {
                        $labels.into_iter().collect()
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

diagnostic_kinds! {
    ParseError {
        span: Span,
        expected: &'static str
    } => "parse error" at [
        Label { label: format!("expected {expected}"), span },
    ]

    UnclosedDelimiter {
        span: Span,
        open: Span,
        expected: &'static str
    } => "unclosed delimiter" at [
        Label { label: "unclosed".to_string(), span: &open },
        Label { label: format!("expected {expected}"), span },
    ]

    NoDefNamed {
        span: Span,
        protocol_name: String,
        def_name: String
    } => "no definition `{def_name}` found for protocol `{protocol_name}`" at [
        Label { label: "not found".to_string(), span },
    ]

    NoVariantNamed {
        span: Span,
        protocol_name: String,
        name: String
    } => "no variant `{name}` exists on protocol `{protocol_name}`" at [
        Label { label: "not found".to_string(), span },
    ]

    NoChildNamed {
        span: Span,
        protocol_name: String,
        name: String
    } => "no child signal `{name}` exists on protocol `{protocol_name}`" at [
        Label { label: "not found".to_string(), span },
    ]

    NoProtocolNamed {
        span: Span,
        protocol_name: String
    } => "no protocol named `{protocol_name}`" at [
        Label { label: "not found".to_string(), span },
    ]

    ProtocolArgumentMismatch {
        span: Span,
        protocol_name: String,
        found: String,
        def: Span
    } => "invalid arguments to protocol `{protocol_name}`" at [
        Label { label: format!("found `{found}`"), span },
        Label { label: "definition site".to_string(), span: &def },
    ]

    ProtocolMessageWithArgsAndChild {
        span: Span
    } => "protocol message with child protocol cannot have arguments" at [
        Label { label: "has arguments and child protocol".to_string(), span },
    ]

    ProtocolDataModeMismatch {
        span: Span,
        mode: crate::core::ShapeMode,
        direction: crate::core::Dir
    } => "data direction not allowed by protocol mode" at [
        Label { label: format!("mode of this protocol is `{mode}` which can't have data with direction `{direction}`"), span },
    ]

    ProtocolChildModeMismatch {
        span: Span,
        child_name: String,
        mode: crate::core::ShapeMode,
        child_mode: crate::core::ShapeMode
    } => "mode mismatch on child protocol `{child_name}`" at [
        Label { label: format!("mode of this protocol is `{child_mode}` but the parent has `{mode}`"), span },
    ]

    OnBlockWithoutUpSignal {
        span: Span
    } => "`on` block used in a context without an upwards signal" at [
        Label { label: "requires a signal to act on".to_string(), span },
    ]

    StackWithoutBaseSignal {
        span: Span
    } => "stacked process without base signal" at [
        Label { label: "does not provide an upper signal".to_string(), span },
    ]

    UpValueNotProvided {
        span: Span
    } => "no value for up-direction variable provided" at [
        Label { label: "must be up-evaluated exactly once in this block".to_string(), span },
    ]

    UpValueMultiplyProvided {
        span: Span
    } => "value for up-direction variable provided multiple times" at [
        Label { label: "must be up-evaluated exactly once in this block".to_string(), span },
    ]   // TODO: spans for each of the up-evaluation sites

    UndefinedVariable {
        span: Span,
        name: String
        } => "undefined variable `{name}`" at [
        Label { label: "not found".to_string(), span },
    ]

    NotAFunction {
        span: Span,
        found: String
    } => "called an item that is not a function" at [
        Label { label: format!("expected a function, but found `{found}`"), span },
    ]

    ErrorInPrimitiveFunction {
        span: Span,
        msg: String
    } => "function error: {msg}" at [
        Label { label: "call site".to_string(), span },
    ]

    ErrorInPrimitiveProcess{
        span: Span,
        msg: String
    } => "primitive could not be instantiated: {msg}" at [
        Label { label: "call site".to_string(), span },
    ]

    FunctionArgumentMismatch {
        span: Span,
        def: Span
    } => "function argument mismatch" at [
        Label { label: "call site".to_string(), span },
        Label { label: "definition".to_string(), span: &def },
    ]

    ArgMismatchType {
        span: Span,
        def_name: String,
        expected: String,
        found: String
    } => "incorrect argument on `{def_name}`" at [
        Label { label: format!("expected `{expected}`, found `{found}`"), span },
    ]

    TupleTooManyPositional {
        span: Span,
        n: usize
    } => "too many positional fields" at [
        Label { label: format!("{n} unexpected positional fields"), span },
    ]

    TupleTooFewPositional {
        span: Span,
        n: usize
    } => "too few positional fields" at [
        Label { label: format!("expected {n} more positional fields"), span },
    ]

    TupleExtraNamed {
        span: Span,
        name: String
    } => "unexpected named field `{name}` passed" at [
        Label { label: "unexpected field".to_string(), span },
    ]

    TupleMissingNamed {
        span: Span,
        name: String
    } => "missing named field `{name}`" at [
        Label { label: "expected additional named field".to_string(), span },
    ]

    ExpectedTuple {
        span: Span,
        found: String
    } => "expected a tuple" at [
        Label { label: format!("passed a single value `{found}`"), span },
    ]

    InvalidItemForPattern {
        span: Span,
        found: String
    } => "invalid item not matched" at [
        Label { label: format!("found `{found}`, incompatible with pattern"), span },
    ]

    ExpectedConst {
        span: Span,
        found: String,
        expected: String
    } => "expected constant {expected}" at [
        Label { label: format!("found `{found}`"), span },
    ]

    ExpectedValue {
        span: Span,
        found: String
    } => "expected a single value" at [
        Label { label: format!("found `{found}`"), span },
    ]

    ExpectedType {
        span: Span,
        found: String
    } => "expected a value that can be resolved as a type" at [
        Label { label: format!("found `{found}`"), span },
    ]

    ExpectedSingleType {
        span: Span
    } => "expected a single type" at [
        Label { label: format!("found tuple"), span },
    ]

    NotAllowedInTypePosition {
        span: Span
    } => "invalid type expression" at [
        Label { label: format!("not allowed in type position"), span },
    ]

    InvalidRepeatCountType {
        span: Span,
        found: Type
    } => "repeat count must be a positive integer" at [
        Label { label: format!("found `{found}`"), span },
    ]

    InvalidRepeatCountPredicate {
        span: Span
    } => "repeat count predicate must be representable as an integer range" at [
        Label { label: "unsupported".to_string(), span },
    ]

    IncompatibleTypes {
        span: Span,
        t1: Type,
        t2: Type
    } => "incompatible types" at [
        Label { label: format!("`{t1}` and `{t2}` cannot be combined"), span },
    ]

    TypeConstraint {
        span: Span,
        found: Type,
        bound: Type
    } => "type error" at [
        Label { label: format!("`{found}` does not match type `{bound}`"), span },
    ]

    ChooseNotCovered {
        span: Span,
        found: Type
    } => "choices do not cover all values of type `{found}`" at [
        Label { label: "additional cases required".to_string(), span },
    ]

    BinaryOneSideMustBeConst {
        span: Span
    } => "one side of a binary operator must be constant for bidirectional expressions" at [
        Label { label: "both sides are runtime values".to_string(), span },
    ]

    DivisionMustBeConst {
        span: Span
    } => "denominator must be constant in division" at [
        Label { label: "denominator is a runtime value".to_string(), span },
    ]

    OperandNotMultipleOfScale {
        const_span: Span,
        const_val: Number,
        var_span: Span,
        var_scale: Number
    } => "constant operand is not a multiple of variable operand's step" at [
        Label { label: format!("found value {const_val}"), span: &const_span },
        Label { label: format!("found variable with step size of {var_scale}"), span: &var_span },
    ]

    BinaryInvalidType {
        span1: Span,
        ty1: Type,
        span2: Span,
        ty2: Type
    } => "invalid types for binary operator" at [
        Label { label: format!("found `{ty1}`"), span: &span1 },
        Label { label: format!("found `{ty2}`"), span: &span2 },
    ]

    RangeNotMultipleOfStep {
        min: Number,
        min_span: Span,
        max: Number,
        max_span: Span,
        step: Number
    } => "range bounds must be a multiple of step ({step})" at [
        Label { label: format!("found minimum {min}"), span: &min_span },
        Label { label: format!("found maximum {max}"), span: &max_span },
    ]

    RangeOrder {
        min: Number,
        min_span: Span,
        max: Number,
        max_span: Span
    } => "range upper bound must be greater than lower bound" at [
        Label { label: format!("found minimum {min}"), span: &min_span },
        Label { label: format!("found maximum {max}"), span: &max_span },
    ]

    RangeStepZero {
        step: Number,
        step_span: Span
    } => "range step must be nonzero" at [
        Label { label: format!("found step {step}"), span: &step_span },
    ]

    ExpectedVector {
        span: Span,
        found: Type
    } => "expected vector" at [
        Label { label: format!("found `{found}`"), span },
    ]

    PatternExpectedVector {
        span: Span,
        expected: Type,
        found_width: u32
    } => "expected type `{expected}`" at [
        Label { label: format!("can't destructure into vector of width {found_width}"), span },
    ]

    NotAllowedInPattern {
        span: Span
    } => "expression type not supported as a pattern" at [
        Label { label: "not allowed here".to_string(), span },
    ]

    AltZeroArms {
        span: Span
    } => "`alt` action must have at least one arm" at [
        Label { label: "no arms specified".to_string(), span },
    ]

    AltNoArmsMatched {
        span: Span
    } => "no arms matched constant alt" at [
        Label { label: "all patterns failed to match".to_string(), span },
    ]

    ForLoopVectorWidthMismatch {
        count_spans: Vec<(u32, Span)>
    } => "`for` loop vectors must be the same width" at count_spans.iter().map(|(width, span)| {
        Label { label: format!("vector of width {width}"), span }
    })

    RequiredDownValue {

        span: Span
    } => "expression cannot be evaluated as a value" at [
        Label { label: "not compatible with down-evaluation".to_string(), span },
    ]
}

/// If the error is an unclosed delimiter, get the span of the opening delimiter
fn find_unclosed_delimiter(parent: &dyn AstNode, err: &ast::Error) -> Option<FileSpan> {
    if let Some(parent) = parent.downcast::<ast::ExprTup>() {
        if parent.close.as_ref().is_err_and(|e| ptr::eq(err, e)) {
            return Some(parent.open.span);
        }
    } else if let Some(parent) = parent.downcast::<ast::Block>() {
        if parent.close.as_ref().is_err_and(|e| ptr::eq(err, e)) {
            return Some(parent.open.span);
        }
    } else if let Some(parent) = parent.downcast::<ast::Protocol>() {
        if parent.close.as_ref().is_err_and(|e| ptr::eq(err, e)) {
            return Some(parent.open.span);
        }
    }
    None
}

pub fn report_parse_errors(
    dcx: &mut DiagnosticContext,
    file: &Arc<SourceFile>,
    ast: &dyn AstNode,
) -> bool {
    let mut has_errors = false;
    let errors = ast
        .walk_preorder_with_parent()
        .filter_map(|(p, n)| n.downcast::<ast::Error>().map(|n| (p, n)));
    for (parent, err) in errors {
        if let Some(open) = find_unclosed_delimiter(parent, err) {
            dcx.report(Diagnostic::UnclosedDelimiter {
                span: Span::new(file, err.span),
                open: Span::new(file, open),
                expected: err.expected,
            });
        } else {
            dcx.report(Diagnostic::ParseError {
                span: Span::new(file, err.span),
                expected: err.expected,
            });
        }
        has_errors = true;
    }
    has_errors
}

/// Like iter.collect::<Result<..>>() but doesn't short-circuit so errors from
/// all subtrees are reported
pub fn collect_or_err<T, C: FromIterator<T>>(iter: impl Iterator<Item=Result<T, ErrorReported>>) -> Result<C, ErrorReported> {
    let mut error = None;
    let result = C::from_iter(iter.flat_map(|i| {
        match i {
            Ok(elem) => Some(elem),
            Err(err) => { error = Some(err); None }
        }
    }));
    if let Some(e) = error { Err(e) } else { Ok(result) }
}

/// Push an item into a fallible vector
pub fn try_push<T>(vec: &mut Result<Vec<T>, ErrorReported>, item: Result<T, ErrorReported>) {
    match (vec, item) {
        (Ok(vec), Ok(item)) => vec.push(item),
        (Err(_), _) => {}
        (vec, Err(e)) => *vec = Err(e),
    }
}
