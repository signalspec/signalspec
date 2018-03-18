use super::{ Ctxt, Scope };
use data::DataMode;
use protocol::{ Fields, Shape };
use process::PrimitiveProcess;

pub enum PrimitiveDefFields {
    Explicit(Fields),
    Auto(DataMode)
}

pub struct PrimitiveDef {
    pub id: &'static str,
    pub fields_down: Fields,
    pub fields_up: PrimitiveDefFields,
    pub instantiate: Box<Fn(&Scope) -> Result<Box<PrimitiveProcess>, ()>>,
}

pub fn call_primitive(_ctx: &Ctxt,
                  scope: &Scope,
                  fields_down: &Fields,
                  shape_up: &Shape,
                  primitive_impls: &[PrimitiveDef],
                  name: &str) -> (Box<PrimitiveProcess>, Fields) {
    for def in primitive_impls {
        if fields_down == &def.fields_down {
            info!("Using {} for primitive at {}", def.id, name);
            let fields_up = match def.fields_up {
                PrimitiveDefFields::Explicit(ref fields) => fields.clone(),
                PrimitiveDefFields::Auto(dir) => shape_up.fields(dir),
            };

            let implementation = (def.instantiate)(scope).expect("Failed to instantiate primitive");

            return (implementation, fields_up);
        }
    }

    panic!("No matching call for primitive {} for {:?}", name, fields_down);
}
