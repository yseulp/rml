#![feature(box_patterns, doc_cfg)]

//! RML-Syn is a library for the syntax of an RML specification, based on `syn`.
//!
//! It is geared towards use in procedural (attribute) macros, but contains a
//! general API.

#[macro_use]
pub(crate) mod macros;
pub(crate) mod print;

pub mod visit;

pub mod attrs;
pub mod extern_spec;
pub mod locset;
pub mod pat;
pub mod spec;
pub mod subject;
pub mod term;

use std::iter;

pub use locset::*;
pub use pat::*;
pub use spec::*;
use syn::{AttrStyle, Attribute};
pub use term::*;

/// Encoding trait for translating an rml-syn element into a syn-element
pub trait Encode {
    type Target;

    /// Encode the value to [Self::Target].
    fn encode(self) -> Self::Target;
}

/// Allow filtering attributes to outer/inner attributes.
pub trait FilterAttrs<'a> {
    type Ret: Iterator<Item = &'a Attribute>;

    /// Get outer attributes.
    fn outer(self) -> Self::Ret;
    /// Get inner attributes.
    fn inner(self) -> Self::Ret;
}

impl<'a> FilterAttrs<'a> for &'a [Attribute] {
    type Ret = iter::Filter<std::slice::Iter<'a, Attribute>, fn(&&Attribute) -> bool>;

    fn outer(self) -> Self::Ret {
        fn is_outer(attr: &&Attribute) -> bool {
            match attr.style {
                AttrStyle::Outer => true,
                AttrStyle::Inner(_) => false,
            }
        }
        self.iter().filter(is_outer)
    }

    fn inner(self) -> Self::Ret {
        fn is_inner(attr: &&Attribute) -> bool {
            match attr.style {
                AttrStyle::Inner(_) => true,
                AttrStyle::Outer => false,
            }
        }
        self.iter().filter(is_inner)
    }
}
