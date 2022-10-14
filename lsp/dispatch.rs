use lsp_types;

use lsp_server;

pub(crate) struct DispatchNotification<'a, State> {
    pub(crate) state: &'a mut State,
    pub(crate) msg: Option<lsp_server::Notification>,
}

impl<'a, State> DispatchNotification<'a, State> {
    pub(crate) fn new(state: &'a mut State, msg: lsp_server::Notification) -> Self { 
        Self { state, msg: Some(msg) }
    }

    pub(crate) fn on<T: lsp_types::notification::Notification>(self, f: fn(&mut State, T::Params)) -> Self {
        match self.msg {
            Some(msg) if &msg.method == T::METHOD => {
                let params = serde_json::from_value(msg.params).expect("failed to deserialize notification params");
                f(self.state, params);
                Self { state: self.state, msg: None }
            }
            _ => self
        }
    }

    pub(crate) fn finish(self) {
        if let Some(msg) = self.msg {
            eprintln!("Unknown notification {}", msg.method);
        }
    }
}
