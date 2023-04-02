use std::sync::Arc;

use super::Scope;
use crate::runtime::PrimitiveProcess;

pub struct PrimitiveDef {
    pub id: &'static str,
    pub instantiate: Box<dyn Fn(&Scope) -> Result<Arc<dyn PrimitiveProcess>, String>>,
}

impl PrimitiveDef {
    pub fn instantiate(&self, scope: &Scope) -> Result<Arc<dyn PrimitiveProcess>, String> {
        (self.instantiate)(scope)
    }
}
