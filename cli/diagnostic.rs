use codespan_reporting::{
    diagnostic::{
        Diagnostic as ReportingDiagnostic,
        Label as ReportingLabel,
    },
    term::termcolor::{ColorChoice, StandardStream}
};
use signalspec::{SourceFile, diagnostic::Diagnostics};

pub fn report_diagnostics<'a>(diagnostics: Diagnostics) {
    for d in diagnostics {
        let d = ReportingDiagnostic::error()
            .with_message(d.message())
            .with_labels(d.labels().into_iter().map(|l|{
                ReportingLabel::primary(&*l.span.file, l.span.span).with_message(l.label)
            }).collect());

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        codespan_reporting::term::emit(&mut writer.lock(), &config, &Files, &d).unwrap();
    }
}

struct Files;

impl<'a> codespan_reporting::files::Files<'a> for Files {
    type FileId = &'a SourceFile;

    type Name = &'a str;

    type Source = &'a str;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok(&id.name())
    }

    fn source(&'a self, id: Self::FileId) -> Result<Self::Source, codespan_reporting::files::Error> {
        Ok(&id.source())
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Result<usize, codespan_reporting::files::Error> {
        Ok(id.byte_to_line(byte_index.into()))
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        Ok(id.line_span(line_index)
            .ok_or(codespan_reporting::files::Error::LineTooLarge{ given: line_index, max: id.num_lines() - 1})?
            .into()
        )
    }
}