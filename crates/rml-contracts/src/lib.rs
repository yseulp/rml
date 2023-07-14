extern crate self as rml_contracts;

pub use crate::macros::*;

#[cfg(rml)]
mod macros {
    pub use rml_proc::spec;

    pub use rml_proc::pure;

    pub use rml_proc::invariant;

    pub use rml_proc::modifies;

    pub use rml_proc::variant;
}

#[cfg(not(rml))]
mod macros {
    pub use rml_proc_dummy::spec;

    pub use rml_proc_dummy::pure;

    pub use rml_proc_dummy::invariant;

    pub use rml_proc_dummy::modifies;

    pub use rml_proc_dummy::variant;
}
