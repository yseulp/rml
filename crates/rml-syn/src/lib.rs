#![feature(box_patterns)]

#[macro_use]
pub(crate) mod macros;
pub(crate) mod print;

pub mod locset;
pub mod spec;
pub mod subject;
pub mod term;

use std::iter;

pub use spec::*;
use syn::{AttrStyle, Attribute};
pub use term::*;

pub fn l() -> bool {
    true
}

pub trait Encode {
    type Target;
    fn encode(self) -> Self::Target;
}

pub trait FilterAttrs<'a> {
    type Ret: Iterator<Item = &'a Attribute>;

    fn outer(self) -> Self::Ret;
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
