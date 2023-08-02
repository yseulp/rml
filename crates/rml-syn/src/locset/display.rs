use std::fmt;

use quote::ToTokens;

use super::{
    LocSet, LocSetField, LocSetFieldWildcard, LocSetGroup, LocSetIndex, LocSetNothing, LocSetPath,
};

impl fmt::Display for LocSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LocSet::Field(l) => fmt::Display::fmt(l, f),
            LocSet::FieldWildcard(l) => fmt::Display::fmt(l, f),
            LocSet::Index(l) => fmt::Display::fmt(l, f),
            LocSet::Path(l) => fmt::Display::fmt(l, f),
            LocSet::Group(l) => fmt::Display::fmt(l, f),
            LocSet::Nothing(l) => fmt::Display::fmt(l, f),
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
