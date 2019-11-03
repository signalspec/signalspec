use std::ops::Deref;

pub struct SourceFile {
    pub name: String,
    pub source: String,
}

impl SourceFile {
    pub fn span(&self) -> FileSpan {
        FileSpan::new(0, self.source.len())
    }
}

/// Associate a Span with a value of arbitrary type (e.g. an AST node).
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T> {
    pub node: T,
    pub span: FileSpan,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct FileSpan {
    pub start: u32,
    pub end: u32,
}

impl FileSpan {
    pub fn new(start: usize, end: usize) -> FileSpan {
        FileSpan { start: start as u32, end: end as u32 }
    }
}
