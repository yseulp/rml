extern crate self as rml_contracts;

pub use crate::macros::*;

#[cfg(not(rml))]
mod macros {
    pub use rml_proc::spec;

    pub use rml_proc::requires;
}

#[cfg(rml)]
mod macros {
    pub use rml_proc_dummy::spec;

    pub use rml_proc_dummy::requires;
}
