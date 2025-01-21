#![feature(allocator_api)]

//! Procedural (attribute) macros to specify Rust code. Either generates
//! additional specification functions, methods, and closures, or changes
//! nothing at all, depending on whether `--cfg rml` is set. Do not set this
//! flag yourself, use `cargo-rml` instead.
//!
//! # Usage
//!
//! To specify contracts, import this crate like this:
//! ```
//! extern crate rml_contracts;
//! use rml_contracts::*;
//! ```
//! This will add the necessary attributes, definition of logic-only types, and
//! add some specification to standard library items.
//!
//! If you want to add attributes to loops or closures to specify them, you must
//! add the following features to your crate:
//! ```
//! #![feature(stmt_expr_attributes)]
//! #![feature(proc_macro_hygiene)]
//! ```

extern crate self as rml_contracts;

pub use crate::macros::*;

pub mod logic;
mod model;
pub mod std;
pub mod well_founded;

pub use ghost::Ghost;
pub use logic::{int::Int, ops::IndexLogic, seq::Seq};
pub use model::{DeepModel, ShallowModel};
pub use well_founded::{well_founded_check, WellFounded};

#[cfg(rml)]
pub mod ghost;

#[cfg(not(rml))]
pub mod ghost {
    pub struct Ghost<T>(std::marker::PhantomData<T>)
    where
        T: ?Sized;

    impl<T> Ghost<T> {
        pub fn new() -> Ghost<T> {
            Ghost(std::marker::PhantomData)
        }

        pub fn from_fn<F: Fn() -> Ghost<T>>(_: F) -> Ghost<T> {
            Ghost(std::marker::PhantomData)
        }
    }
}

#[cfg(rml)]
mod macros {
    pub use rml_proc::{
        extern_spec, invariant, logic, modifies, proof_assert, pure, rml, spec, strictly_pure,
        trusted, variant,
    };

    pub mod stubs {
        #[rml::decl::logic]
        #[rml::decl::internal]
        #[rustc_diagnostic_item = "rml_exists"]
        pub fn exists<T, F: Fn(T) -> bool>(_: F) -> bool {
            panic!()
        }

        #[rml::decl::logic]
        #[rml::decl::internal]
        #[rustc_diagnostic_item = "rml_forall"]
        pub fn forall<T, F: Fn(T) -> bool>(_: F) -> bool {
            panic!()
        }

        #[rml::decl::logic]
        #[rml::decl::internal]
        #[rustc_diagnostic_item = "rml_equiv"]
        pub fn equiv<T>(_: T, _: T) -> bool {
            true
        }

        #[rml::decl::logic]
        #[rml::decl::internal]
        #[rustc_diagnostic_item = "rml_old"]
        pub fn old<T>(t: T) -> T {
            t
        }

        #[rml::decl::logic]
        #[rml::decl::internal]
        #[rustc_diagnostic_item = "rml_final_value"]
        pub fn final_value<T>(t: T) -> T {
            t
        }
    }
}

#[cfg(not(rml))]
mod macros {
    pub use rml_proc_dummy::{
        extern_spec, invariant, logic, modifies, proof_assert, pure, rml, spec, strictly_pure,
        trusted, variant,
    };
}
