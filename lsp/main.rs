use std::{collections::HashMap, sync::Arc};
use crossbeam_channel::Sender;

use lsp_server::{ Connection, Message };
use lsp_types::{ ServerCapabilities, InitializeParams, TextDocumentSyncCapability, TextDocumentSyncOptions, TextDocumentSyncKind, OneOf, Url };
use signalspec::{ SourceFile, syntax::FilePos, diagnostic::Span, DiagnosticHandler };

mod dispatch;
use dispatch::DispatchNotification;

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::FULL),
            will_save: None,
            will_save_wait_until: None,
            save: None,
        })),
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

fn main() {
    let (connection, io_threads) = Connection::stdio();
    let server_capabilities = serde_json::to_value(&server_capabilities()).unwrap();
    let initialization_params = connection.initialize(server_capabilities).unwrap();
    let _params: InitializeParams = serde_json::from_value(initialization_params).unwrap();

    let mut state = LspState::new(connection.sender.clone());

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req).unwrap() {
                    break;
                }
                eprintln!("got request: {:?}", req);
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(n) => {
                DispatchNotification::new(&mut state, n)
                    .on::<lsp_types::notification::DidOpenTextDocument>(did_open_text_document)
                    .on::<lsp_types::notification::DidCloseTextDocument>(did_close_text_document)
                    .on::<lsp_types::notification::DidChangeTextDocument>(did_change_text_document)
                    .finish();
            }
        }
    }
    io_threads.join().unwrap();

    eprintln!("shutting down server");
}

fn did_open_text_document(state: &mut LspState, params: lsp_types::DidOpenTextDocumentParams) {
    let source = Arc::new(signalspec::SourceFile::new(params.text_document.uri.path().to_owned(), params.text_document.text));
    let file = state.index.parse_module(source);

    if let Some(already_open) = state.open_documents.insert(params.text_document.uri.clone(), Document { file }) {
        eprintln!("duplicate didOpenTextDocument for {}", params.text_document.uri);
        state.index.remove_file(&already_open.file);
    }

    state.document_modified(params.text_document.uri);
}

fn did_close_text_document(state: &mut LspState, params: lsp_types::DidCloseTextDocumentParams) {
    if let Some(was_open) = state.open_documents.remove(&params.text_document.uri) {
        state.index.remove_file(&was_open.file);
        state.document_removed(params.text_document.uri);
    } else {
        eprintln!("didCloseTextDocument for {} when not open", params.text_document.uri);
    }
}

fn did_change_text_document(state: &mut LspState, params: lsp_types::DidChangeTextDocumentParams) {
    if let Some(doc) = state.open_documents.get_mut(&params.text_document.uri) {
        state.index.remove_file(&doc.file);
        let name = params.text_document.uri.path().to_owned();
        let text = params.content_changes.into_iter().next().unwrap().text;
        let source = Arc::new(signalspec::SourceFile::new(name, text));
        doc.file = state.index.parse_module(source);
        eprintln!("Updated {}", doc.file.source().name());
        state.document_modified(params.text_document.uri);
    } else {
        eprintln!("didChangeTextDocument for {} but not open", params.text_document.uri);
    }
}

fn pos_to_lsp(file: &Arc<SourceFile>, pos: FilePos) -> lsp_types::Position {
    let line = file.byte_to_line(pos);
    let line_span = file.line_span(line).unwrap();
    let line_text = file.slice(line_span);
    let utf8_line_offset = (pos.0 - line_span.start.0) as usize;
    let col: usize = line_text[0..utf8_line_offset].chars().map(char::len_utf16).sum();
    lsp_types::Position { line: line as u32, character: col as u32 }
}

fn span_to_lsp(span: &Span) -> lsp_types::Range {
    lsp_types::Range {
        start: pos_to_lsp(&span.file, span.span.start),
        end: pos_to_lsp(&span.file, span.span.end),
    }
}

struct LspState {
    lsp_sender: Sender<Message>,
    open_documents: HashMap<lsp_types::Url, Document>,
    index: signalspec::Index,
}

impl LspState {
    fn new(lsp_sender: Sender<Message>) -> Self {
        LspState { 
            lsp_sender,
            open_documents: HashMap::new(),
            index: signalspec::Index::new(),
        }
    }

    fn send_notification<N: lsp_types::notification::Notification>(&mut self, params: N::Params) {
        self.lsp_sender.send(lsp_server::Message::Notification(
            lsp_server::Notification::new(N::METHOD.to_string(), params)
        )).unwrap()
    }

    fn document_modified(&mut self, uri: Url) {
        let doc = self.open_documents.get(&uri).unwrap();

        let collector = signalspec::diagnostic::Collector::new();
        collector.report_all(doc.file.errors.iter().cloned());

        let diagnostics = collector.diagnostics().into_iter().map(|d| {
            let labels = d.labels();
            let primary_label = labels.first().unwrap();
            lsp_types::Diagnostic {
                range: span_to_lsp(&primary_label.span),
                severity: None,
                code: None,
                code_description: None,
                source: Some("signalspec".to_owned()),
                message: d.message(),
                related_information: None,
                tags: None,
                data: None,
            }
        }).collect();

        let version = None;

        self.send_notification::<lsp_types::notification::PublishDiagnostics>(
            lsp_types::PublishDiagnosticsParams { uri, diagnostics, version }
        );
    }

    fn document_removed(&mut self, uri: Url) {
        self.send_notification::<lsp_types::notification::PublishDiagnostics>(
            lsp_types::PublishDiagnosticsParams { uri, diagnostics: vec![], version: None }
        );
    }
}

struct Document {
    file: Arc<signalspec::FileScope>,
}
