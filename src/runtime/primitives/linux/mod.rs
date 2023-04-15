use crate::Index;

mod spidev;

pub fn add_primitives(index: &mut Index) {
    spidev::add_primitives(index);
}