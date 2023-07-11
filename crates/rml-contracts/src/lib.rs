#![cfg_attr(not(creusot), feature(rustc_attrs))]

extern crate self as rml_contracts;

pub use crate::macros::*;

#[cfg(not(rml))]
mod macros {
    pub use rml_proc::spec;

    pub use rml_proc::pure;

    pub use rml_proc::invariant;

    pub use rml_proc::modifies;

    pub use rml_proc::variant;
}

#[cfg(rml)]
mod macros {
    pub use rml_proc_dummy::spec;

    pub use rml_proc_dummy::pure;

    pub use rml_proc_dummy::invariant;

    pub use rml_proc_dummy::modifies;

    pub use rml_proc_dummy::variant;
}
