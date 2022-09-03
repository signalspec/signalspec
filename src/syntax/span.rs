use std::iter;
use std::{io, fs, path::Path};
use std::ops::Range;

/// Byte index into a source file
///
/// This wraps `u32` under the assumption that source code won't exceed 4 GiB.
#[derive(Clone, PartialEq, Eq,  PartialOrd, Ord, Hash, Debug, Copy)]
pub struct FilePos(pub u32);

impl From<FilePos> for usize {
    fn from(p: FilePos) -> Self {
        p.0 as usize
    }
}

impl From<usize> for FilePos {
    fn from(p: usize) -> Self {
        FilePos(p as u32)
    }
}

/// Byte span of a source file
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct FileSpan {
    pub start: FilePos,
    pub end: FilePos,
}

impl FileSpan {
    pub fn new(start: usize, end: usize) -> FileSpan {
        assert!(start <= end);
        FileSpan { start: start.into(), end: end.into() }
    }

    pub fn at(p: usize) -> FileSpan {
        Self::new(p, p)
    }

    pub fn contains(&self, p: FilePos) -> bool {
        p >= self.start && p <= self.end
    }

    pub fn to(self, other: FileSpan) -> FileSpan {
        FileSpan { start: self.start, end: other.end }
    }
}

impl From<Range<usize>> for FileSpan {
    fn from(s: Range<usize>) -> Self {
        FileSpan::new(s.start, s.end)
    }
}

impl From<FileSpan> for Range<usize> {
    fn from(s: FileSpan) -> Self {
        (s.start.into())..(s.end.into())
    }
}


/// Associate a Span with a value of arbitrary type (e.g. an AST node).
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T> {
    pub node: T,
    pub span: FileSpan,
}

struct LineIndex {
    line_starts: Vec<FilePos>,
    len: FilePos,
}

impl LineIndex {
    fn new(s: &str) -> LineIndex {
        let line_starts = iter::once(FilePos(0))
            .chain(s.match_indices('\n').map(|(p, _)| FilePos((p + 1) as u32)))
            .collect();
        LineIndex { line_starts, len: s.len().into() }
    }

    fn num_lines(&self) -> usize {
        self.line_starts.len()
    }

    fn byte_to_line(&self, pos: FilePos) -> usize {
        self.line_starts.binary_search(&pos).unwrap_or_else(|l| l - 1)
    }

    fn line_start(&self, line: usize) -> Option<FilePos> {
        self.line_starts.get(line).cloned()
    }

    fn line_span(&self, line: usize) -> Option<FileSpan> {
        let start = self.line_start(line)?;
        let end = self.line_start(line + 1).unwrap_or(self.len);
        Some(FileSpan { start, end})
    }
}

pub struct SourceFile {
    name: String,
    source: String,
    lines: LineIndex,
}

impl SourceFile {
    /// Create a SourceFile from a filename and string
    pub fn new(name: String, source: String) -> SourceFile {
        assert!(source.len() < u32::MAX as usize);
        let lines = LineIndex::new(&source);
        SourceFile { name, source, lines }
    }

    /// Create a SourceFile by loading a file from disk
    pub fn load(fname: &Path) -> Result<SourceFile, io::Error> {
        let source = fs::read_to_string(fname)?;
        Ok(Self::new(fname.to_string_lossy().into(), source))
    }

    /// Get a reference to the filename
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    /// Get a reference to the source text
    pub fn source(&self) -> &str {
        self.source.as_ref()
    }

    /// Get the 0-indexed line of a byte position
    pub fn byte_to_line(&self, pos: FilePos) -> usize {
        self.lines.byte_to_line(pos)
    }

    /// Get the byte span of a 0-indexed line number
    pub fn line_span(&self, line: usize) -> Option<FileSpan> {
        self.lines.line_span(line)
    }

    pub fn num_lines(&self) -> usize {
        self.lines.num_lines()
    }

    pub fn slice(&self, span: FileSpan) -> &str {
        &self.source[Range::<usize>::from(span)]
    }
}

impl PartialEq<SourceFile> for SourceFile {
    fn eq(&self, other: &SourceFile) -> bool {
        std::ptr::eq(self, other)
    }
}

#[test]
fn test_line_index() {
    let l1 = LineIndex::new("asdf");
    assert_eq!(l1.num_lines(), 1);
    assert_eq!(l1.byte_to_line(FilePos(0)), 0);
    assert_eq!(l1.byte_to_line(FilePos(3)), 0);
    assert_eq!(l1.byte_to_line(FilePos(4)), 0);
    assert_eq!(l1.line_span(0), Some(FileSpan::new(0, 4)));
    assert_eq!(l1.line_span(1), None);

    let l2 = LineIndex::new("abc\ndef\nghi\n");
    assert_eq!(l2.num_lines(), 4);
    assert_eq!(l2.byte_to_line(FilePos(2)), 0);
    assert_eq!(l2.byte_to_line(FilePos(5)), 1);
    assert_eq!(l2.byte_to_line(FilePos(11)), 2);
    assert_eq!(l2.line_span(0), Some(FileSpan::new(0, 4)));
    assert_eq!(l2.line_span(1), Some(FileSpan::new(4, 8)));
    assert_eq!(l2.line_span(2), Some(FileSpan::new(8, 12)));
    assert_eq!(l2.line_span(3), Some(FileSpan::new(12, 12)));
    assert_eq!(l2.line_span(4), None);
}
