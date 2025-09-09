#![feature(allocator_api)]

//! Procedural (attribute) macros to specify Rust code. Either generates
//! additional specification functions, methods, and closures, or changes
//! nothing at all, depending on whether --cfg rml is set. Do not set this
//! flag yourself, use cargo-rml instead.
//!
//! # Usage
//!
//! To specify contracts, import this crate like this:
//! 
//! extern crate rml_contracts;
//! use rml_contracts::*;
//!

//! This will add the necessary attributes, definition of logic-only types, and
//! add some specification to standard library items.
//!
//! If you want to add attributes to loops or closures to specify them, you must
//! add the following features to your crate:
//! 
//! #![feature(stmt_expr_attributes)]
//! #![feature(proc_macro_hygiene)]
//!


extern crate self as rml_contracts;

pub use crate::macros::*;

pub mod logic;
mod model;
pub mod std;
pub mod well_founded;

pub use ghost::Ghost;
pub use logic::{int::Int, ops::IndexLogic, seq::Seq};
pub use model::{DeepModel, ShallowModel};
pub use well_founded::{WellFounded, well_founded_check};

#[cfg(rml)]
pub mod snapshot;
#[cfg(not(rml))]
pub mod snapshot {
    use std::{marker::PhantomData, ops::{Deref, DerefMut}};
    pub struct Snapshot<T: ?Sized>(PhantomData<T>);

    impl<T> Snapshot<T> {

        pub fn new(_: T) -> Self {
            Self(PhantomData)
        }

        pub fn phantom() -> Self {
            Self(PhantomData)
        }

        pub fn from_fn(_: fn() -> T) -> Self {
            Self(PhantomData)
        }
    }

    pub fn snapshot<T>(_v: T) -> T {
        panic!()
    } 

    impl<T: ?Sized> Deref for Snapshot<T> {
        type Target = T;
        fn deref(&self) -> &Self::Target {
            panic!()
        }
    }

    impl<T: ?Sized> DerefMut for Snapshot<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            panic!()
        }
    }
}




#[cfg(rml)]
pub mod ghost;

#[cfg(not(rml))]
pub mod ghost {
    use std::{
        marker::PhantomData,
        ops::{Deref, DerefMut},
    };

    pub struct Ghost<T>(PhantomData<T>)
    where
        T: ?Sized;

    impl<T> Ghost<T> {
        pub fn new(_: T) -> Ghost<T> {
            Self(PhantomData)
        }

        pub fn phantom() -> Ghost<T> {
            Self(PhantomData)
        }

        pub fn from_fn<F: Fn() -> Ghost<T>>(_: F) -> Ghost<T> {
            Ghost(PhantomData)
        }
    }

    impl<T: ?Sized> DerefMut for Ghost<T> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            panic!()
        }
    }

    impl<T: ?Sized> Deref for Ghost<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            panic!()
        }
    }
}

#[cfg(rml)]
mod macros {
    pub use rml_proc::{
        extern_spec, ghost, invariant, logic, modifies, proof_assert, pure, rml, spec,
        strictly_pure, trusted, variant, snapshot, 
    };

    pub mod stubs {
        use crate::snapshot::Snapshot;

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

        #[rml::decl::logic]
        #[rml::decl::internal]
        #[rustc_diagnostic_item = "rml_snapshot_from_fn"]
        fn snapshot_from_fn<T, F>(f: F) -> Snapshot<T> where F: Fn() -> Snapshot<T> {
            f()
        }
    }
}

#[cfg(not(rml))]
mod macros {
    pub use rml_proc_dummy::{
        extern_spec, ghost, invariant, logic, modifies, proof_assert, pure, rml, spec,
        strictly_pure, trusted, variant, snapshot
    };
}