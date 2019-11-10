use super::Scope;
use crate::runtime::PrimitiveProcess;

pub struct PrimitiveDef {
    pub id: &'static str,
    pub instantiate: Box<dyn Fn(&Scope) -> Result<Box<dyn PrimitiveProcess>, ()>>,
}

impl PrimitiveDef {
    pub fn instantiate(&self, scope: &Scope) -> Box<dyn PrimitiveProcess> {
        (self.instantiate)(scope).expect("Failed to instantiate primitive")
    }
}
