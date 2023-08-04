#![feature(allocator_api)]

extern crate self as rml_contracts;

pub use crate::macros::*;

pub mod logic;
mod model;
pub mod std;
pub mod well_founded;

pub use ghost::Ghost;
pub use logic::{int::Int, ops::IndexLogic, seq::Seq};
pub use model::{DeepModel, ShallowModel};
pub use well_founded::WellFounded;

#[cfg(creusot)]
pub mod ghost;

#[cfg(not(creusot))]
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
    pub use rml_proc::spec;

    pub use rml_proc::pure;

    pub use rml_proc::strictly_pure;

    pub use rml_proc::invariant;

    pub use rml_proc::modifies;

    pub use rml_proc::variant;

    pub use rml_proc::logic;

    pub use rml_proc::rml;

    pub use rml_proc::trusted;

    pub use rml_proc::proof_assert;

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
        #[rustc_diagnostic_item = "rml_equiv"]
        pub fn old<T>(t: T) -> T {
            t
        }
    }
}

#[cfg(not(rml))]
mod macros {
    pub use rml_proc_dummy::spec;

    pub use rml_proc_dummy::pure;

    pub use rml_proc_dummy::strictly_pure;

    pub use rml_proc_dummy::invariant;

    pub use rml_proc_dummy::modifies;

    pub use rml_proc_dummy::variant;

    pub use rml_proc_dummy::logic;

    pub use rml_proc_dummy::rml;

    pub use rml_proc_dummy::trusted;

    pub use rml_proc_dummy::proof_assert;
}
