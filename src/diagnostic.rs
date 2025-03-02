use std::{sync::Arc, fmt::{Display, Debug}, ptr};
use crate::{SourceFile, FileSpan, syntax::{AstNode, ast, Number}, Type};

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
        DiagnosticContext { diagnostics: Vec::new() }
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
#[derive(Clone)]
pub struct ErrorReported(());

impl ErrorReported {
    /// Provide evidence that an error has been reported
    pub fn error_reported() -> Self { ErrorReported(()) }

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
                    $(Self::$variant { $($n),* } => {
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

    UnclosedDelimiter {
        span: Span,
        open: Span,
        expected: &'static str
    } => "unclosed delimiter" {
        error "unclosed" at open
        error "expected {expected}" at span
    }

    NoDefNamed {
        span: Span,
        protocol_name: String,
        def_name: String
    } => "no definition `{def_name}` found for protocol `{protocol_name}`" {
        error "not found" at span
    }

    NoVariantNamed {
        span: Span,
        protocol_name: String,
        name: String
    } => "no variant `{name}` exists on protocol `{protocol_name}`" {
        error "not found" at span
    }

    NoChildNamed {
        span: Span,
        protocol_name: String,
        name: String
    } => "no child signal `{name}` exists on protocol `{protocol_name}`" {
        error "not found" at span
    }

    NoProtocolNamed {
        span: Span,
        protocol_name: String
    } => "no protocol named `{protocol_name}`" {
        error "not found" at span
    }

    ProtocolArgumentMismatch {
        span: Span,
        protocol_name: String,
        found: String,
        def: Span
    } => "invalid arguments to protocol `{protocol_name}`" {
        error "found `{found}`" at span
        error "definition site" at def
    }

    ProtocolMessageWithArgsAndChild {
        span: Span
    } => "protocol message with child protocol cannot have arguments" {
        error "has arguments and child protocol" at span
    }

    ProtocolDataModeMismatch {
        span: Span,
        mode: crate::core::ShapeMode,
        direction: crate::core::Dir
    } => "data direction not allowed by protocol mode" {
        error "mode of this protocol is `{mode}` which can't have data with direction `{direction}`" at span
    }

    ProtocolChildModeMismatch {
        span: Span,
        child_name: String,
        mode: crate::core::ShapeMode,
        child_mode: crate::core::ShapeMode
    } => "mode mismatch on child protocol `{child_name}`" {
        error "mode of this protocol is `{child_mode}` but the parent has `{mode}`" at span
    }

    OnBlockWithoutUpSignal {
        span: Span
    } => "`on` block used in a context without an upwards signal" {
        error "requires a signal to act on" at span
    }

    StackWithoutBaseSignal {
        span: Span
    } => "stacked process without base signal" {
        error "does not provide an upper signal" at span
    }

    UpValueNotProvided {
        span: Span
    } => "no value for up-direction variable provided" {
        error "must be up-evaluated exactly once in this block" at span
    }

    UpValueMultiplyProvided {
        span: Span
    } => "value for up-direction variable provided multiple times" {
        error "must be up-evaluated exactly once in this block" at span
        // TODO: spans for each of the up-evaluation sites
    }

    UndefinedVariable {
        span: Span,
        name: String
    } => "undefined variable `{name}`" {
        error "not found" at span
    }

    NotAFunction {
        span: Span,
        found: String
    } => "called an item that is not a function" {
        error "expected a function, but found `{found}`" at span
    }

    ErrorInPrimitiveFunction {
        span: Span,
        msg: String
    } => "function error: {msg}" {
        error "call site" at span
    }

    ErrorInPrimitiveProcess{
        span: Span,
        msg: String
    } => "primitive could not be instantiated: {msg}" {
        error "call site" at span
    }

    FunctionArgumentMismatch {
        span: Span,
        def: Span
    } => "function argument mismatch" {
        error "call site" at span
        error "definition" at def
    }

    ArgMismatchType {
        span: Span,
        def_name: String,
        expected: String,
        found: String
    } => "incorrect argument on `{def_name}`" {
        error "expected `{expected}`, found `{found}`" at span
    }

    TupleTooManyPositional {
        span: Span,
        n: usize
    } => "too many positional fields" {
        error "{n} unexpected positional fields" at span
    }

    TupleTooFewPositional {
        span: Span,
        n: usize
    } => "too few positional fields" {
        error "expected {n} more positional fields" at span
    }

    TupleExtraNamed {
        span: Span,
        name: String
    } => "unexpected named field `{name}` passed" {
        error "unexpected field" at span
    }

    TupleMissingNamed {
        span: Span,
        name: String
    } => "missing named field `{name}`" {
        error "expected additional named field" at span
    }

    ExpectedTuple {
        span: Span,
        found: String
    } => "expected a tuple" {
        error "passed a single value `{found}`" at span
    }

    InvalidItemForPattern {
        span: Span,
        found: String
    } => "invalid item not matched" {
        error "found `{found}`, incompatible with pattern" at span
    }

    ExpectedConst {
        span: Span,
        found: String,
        expected: String
    } => "expected constant {expected}" {
        error "found `{found}`" at span
    }

    ExpectedValue {
        span: Span,
        found: String
    } => "expected a single value" {
        error "found `{found}`" at span
    }

    ExpectedType {
        span: Span,
        found: String
    } => "expected a value that can be resolved as a type" {
        error "found `{found}`" at span
    }

    InvalidRepeatCountType {
        span: Span,
        found: Type
    } => "repeat count must be a positive integer" {
        error "found `{found}`" at span
    }

    InvalidRepeatCountPredicate {
        span: Span
    } => "repeat count predicate must be representable as an integer range" {
        error "unsupported" at span
    }

    IncompatibleTypes {
        span: Span,
        t1: Type,
        t2: Type
    } => "incompatible types" {
        error "`{t1}` and `{t2}` cannot be combined" at span
    }

    TypeConstraint {
        span: Span,
        found: Type,
        bound: Type
    } => "type error" {
        error "`{found}` does not match type `{bound}`" at span
    }

    ChooseNotCovered {
        span: Span,
        found: Type
    } => "choices do not cover all values of type `{found}`" {
        error "additional cases required" at span
    }

    BinaryOneSideMustBeConst {
        span: Span
    } => "one side of a binary operator must be constant for bidirectional expressions" {
        error "both sides are runtime values" at span
    }

    DivisionMustBeConst {
        span: Span
    } => "denominator must be constant in division" {
        error "denominator is a runtime value" at span
    }

    OperandNotMultipleOfScale {
        const_span: Span,
        const_val: Number,
        var_span: Span,
        var_scale: Number
    } => "constant operand is not a multiple of variable operand's step" {
        error "found value {const_val}" at const_span
        error "found variable with step size of {var_scale}" at var_span
    }

    BinaryInvalidType {
        span1: Span,
        ty1: Type,
        span2: Span,
        ty2: Type
    } => "invalid types for binary operator" {
        error "found `{ty1}`" at span1
        error "found `{ty2}`" at span2
    }

    RangeNotMultipleOfStep {
        min: Number,
        min_span: Span,
        max: Number,
        max_span: Span,
        step: Number
    } => "range bounds must be a multiple of step ({step})" {
        error "found minimum {min}" at min_span
        error "found maximum {max}" at max_span
    }

    RangeOrder {
        min: Number,
        min_span: Span,
        max: Number,
        max_span: Span
    } => "range upper bound must be greater than lower bound" {
        error "found minimum {min}" at min_span
        error "found maximum {max}" at max_span
    }

    RangeStepZero {
        step: Number,
        step_span: Span
    } => "range step must be nonzero" {
        error "found step {step}" at step_span
    }

    ExpectedVector {
        span: Span,
        found: Type
    } => "expected vector" {
        error "found `{found}`" at span
    }

    PatternExpectedVector {
        span: Span,
        expected: Type,
        found_width: u32
    } => "expected type `{expected}`" {
        error "can't destructure into vector of width {found_width}" at span
    }

    NotAllowedInPattern {
        span: Span
    } => "expression type not supported as a pattern" {
        error "not allowed here" at span
    }

    AltZeroArms {
        span: Span
    } => "`alt` action must have at least one arm" {
        error "no arms specified" at span
    }

    AltNoArmsMatched {
        span: Span
    } => "no arms matched constant alt" {
        error "all patterns failed to match" at span
    }

    ForLoopVectorWidthMismatch {
        span1: Span,
        width1: u32,
        span2: Span,
        width2: u32
    } => "`for` loop vectors must be the same width" {
        error "vector of width {width1}" at span1
        error "vector of width {width2}" at span2
    }

    RequiredDownValue {
        span: Span,
        found: String
    } => "expression cannot be evaluated as a value" {
        error "found `{found}`" at span
    }
}

/// If the error is an unclosed delimiter, get the span of the opening delimiter
fn find_unclosed_delimiter(parent: &dyn AstNode, err: &ast::Error) -> Option<FileSpan> {
    if let Some(parent) = parent.downcast::<ast::ExprTup>() {
        if parent.close.as_ref().is_err_and(|e| ptr::eq(err, e)) {
            return Some(parent.open.span)
        }
    } else if let Some(parent) = parent.downcast::<ast::Block>() {
        if parent.close.as_ref().is_err_and(|e| ptr::eq(err, e)) {
            return Some(parent.open.span)
        }
    } else if let Some(parent) = parent.downcast::<ast::Protocol>() {
        if parent.close.as_ref().is_err_and(|e| ptr::eq(err, e)) {
            return Some(parent.open.span)
        }
    }
    None
}

pub fn report_parse_errors(dcx: &mut DiagnosticContext, file: &Arc<SourceFile>, ast: &dyn AstNode) -> bool{
    let mut has_errors = false;
    let errors = ast.walk_preorder_with_parent()
        .filter_map(|(p, n)| {
            n.downcast::<ast::Error>().map(|n| (p, n))
        });
    for (parent, err) in errors {
        if let Some(open) = find_unclosed_delimiter(parent, err) {
            dcx.report(Diagnostic::UnclosedDelimiter {
                span: Span::new(file, err.span),
                open: Span::new(file, open),
                expected: err.expected
            });
        } else {
            dcx.report(Diagnostic::ParseError {
                span: Span::new(file, err.span),
                expected: err.expected
            });
        }
        has_errors = true;
    }
    has_errors
}
