use signalspec::{self, SourceFile, Diagnostic, FileSpan, Label, Index};
use env_logger;
use std::{process, fs, path::Path, sync::Arc, cell::RefCell};

fn main() {
    env_logger::init();

    let mut success = true;
    for entry in fs::read_dir("tests/check-fail").unwrap() {
        let path = entry.unwrap().path();
        if path.to_str().unwrap().ends_with(".signalspec") {
            success &= check_file(&path);
        }
    }

    process::exit( if success { 0 } else { 1 } );
}

fn check_file(fname: &Path) -> bool {
    println!("Running tests for {}", fname.to_string_lossy());
    
    let file = match SourceFile::load(fname) {
        Ok(file) => Arc::new(file),
        Err(err) => {
            eprintln!("\tCould not open '{}': {}", fname.display(), err);
            return false;
        }
    };

    let errors = RefCell::new(Vec::new());

    let mut index = Index::new();
    index.load(Path::new("tests/min-prelude.signalspec")).unwrap();
    let index = index.validate().unwrap();

    signalspec::runtime::run_tests_in_file(&|d| errors.borrow_mut().extend(d), &index, file.clone(), true);

    check_errors(&file, &errors.into_inner())
}

fn check_errors(f: &SourceFile, errors: &[Diagnostic]) -> bool {
    let mut messages: Vec<_> = errors.iter().map(|d| d.message()).collect();
    let mut labels: Vec<_> = errors.iter().flat_map(|d| d.labels()).collect();
    let mut success = true;

    let mut non_comment_line_span: Option<FileSpan> = None;

    for line_num in 0..f.num_lines() {
        let line_span = f.line_span(line_num).unwrap();
        let line_text = f.slice(line_span);
        
        match check_line::line(line_text).expect("comment line parse failed") {
            Some(CheckLine::Message(s)) => {
                if let Some(p) = messages.iter().position(|m| m.contains(s)) {
                    messages.swap_remove(p);
                } else {
                    eprintln!("\tExpected error with message `{s}`");
                    success = false;
                }
            },
            Some(CheckLine::Label(p1, p2, s)) => {
                let span = non_comment_line_span.unwrap().subspan(p1 as u32, p2 as u32);
                if let Some(p) = labels.iter().position(|l| l.span.span == span && l.label.contains(s)) {
                    labels.swap_remove(p);
                } else {
                    eprintln!("\tExpected label at line {line} with message `{s}`", line=line_num + 1);
                    success = false;
                }
            }
            Some(CheckLine::LabelStart(p1, s)) => {
                let start = non_comment_line_span.unwrap().start.0 + p1 as u32;
                if let Some(p) = labels.iter().position(|l| l.span.span.start.0 == start && l.label.contains(s)) {
                    labels.swap_remove(p);
                } else {
                    eprintln!("\tExpected label starting at {start} with message `{s}`");
                    success = false;
                }
            }
            Some(CheckLine::LabelAny(s)) => {
                if let Some(p) = labels.iter().position(|l| l.label.contains(s)) {
                    labels.swap_remove(p);
                } else {
                    eprintln!("\tExpected label with message `{s}`");
                    success = false;
                }
            }
            None => {
                non_comment_line_span = Some(line_span);
            }
        }
    }

    for message in &messages {
        eprintln!("\tUnmatched message reported. Add: //# {message}");
    }

    for Label { span, label, .. } in &labels {
        let span = span.span;
        let start_line = f.byte_to_line(span.start);
        let start_line_span = f.line_span(start_line).unwrap();
        let end_line = f.byte_to_line(span.end);

        let start_pos = span.start - start_line_span.start;
        let spacing = " ".repeat(start_pos.saturating_sub(3) as usize);

        let marker = if start_line == end_line && span.start != span.end {
            "^".repeat((span.end - span.start) as usize)
        } else {
            "^-".into()
        };

        eprintln!("\tUnmatched label reported at line {line}. Add: // {spacing}{marker} {label}", line = start_line+1);
    }

    success && messages.is_empty() && labels.is_empty()
}

pub(crate) enum CheckLine<'s> {
    Message(&'s str),
    Label(usize, usize, &'s str),
    LabelStart(usize, &'s str),
    LabelAny(&'s str),
}

peg::parser! { grammar check_line() for str {
    pub(crate) rule line() -> Option<CheckLine<'input>> = [' ']* "//" [' ']* r:(
        "#" s:$([_]*) { CheckLine::Message(s.trim()) }
        / [' ']* p1:position!() ['^']+ p2:position!() !['-' | '*'] s:$([_]*) { CheckLine::Label(p1, p2, s.trim()) }
        / [' ']* p1:position!() "^-" s:$([_]*) { CheckLine::LabelStart(p1, s.trim()) }
        / [' ']* "^*" s:$([_]*) { CheckLine::LabelAny(s.trim()) }
    ) { Some(r) } / [_]* { None }
}}
