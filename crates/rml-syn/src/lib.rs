#[macro_use]
pub(crate) mod macros;
pub(crate) mod print;

pub mod locset;
pub mod spec;
pub mod term;

pub use spec::*;
pub use term::*;

pub fn l() -> bool {
    true
}

pub trait Encode {
    type Target;
    fn encode(self) -> Self::Target;
}
