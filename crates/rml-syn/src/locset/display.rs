use std::fmt;

use quote::ToTokens;

use super::{
    LocSetField, LocSetFieldWildcard, LocSetGroup, LocSetIndex, LocSetNothing, LocSetPath,
    LocSetTerm,
};

impl fmt::Display for LocSetTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LocSetTerm::Field(l) => fmt::Display::fmt(l, f),
            LocSetTerm::FieldWildcard(l) => fmt::Display::fmt(l, f),
            LocSetTerm::Index(l) => fmt::Display::fmt(l, f),
            LocSetTerm::Path(l) => fmt::Display::fmt(l, f),
            LocSetTerm::Group(l) => fmt::Display::fmt(l, f),
            LocSetTerm::Nothing(l) => fmt::Display::fmt(l, f),
        }
    }
}

impl fmt::Display for LocSetField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.base, self.member.to_token_stream())
    }
}

impl fmt::Display for LocSetFieldWildcard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.*", self.base)
    }
}

impl fmt::Display for LocSetIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.term, self.index)
    }
}

impl fmt::Display for LocSetPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner.to_token_stream())
    }
}

impl fmt::Display for LocSetGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.items.to_token_stream())
    }
}

impl fmt::Display for LocSetNothing {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "nothing")
    }
}
