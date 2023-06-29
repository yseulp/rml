extern crate self as rml_contracts;

pub use crate::macros::*;

#[cfg(rml)]
mod macros {
    pub use rml_proc::spec;

    pub use rml_proc::requires;
}

#[cfg(not(rml))]
mod macros {
    pub use rml_proc_dummy::spec;

    pub use rml_proc_dummy::requires;
}
