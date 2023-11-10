use ::std::fmt;
use quote::ToTokens;
use syn::punctuated::Punctuated;

use super::{
    FieldPat, Pat, PatIdent, PatOr, PatParen, PatRest, PatSlice, PatStruct, PatTuple,
    PatTupleStruct, PatType, PatWild,
};

#[inline]
fn punctuated_to_str<T, P>(punct: &Punctuated<T, P>, join: &'static str) -> String
where
    T: fmt::Display,
{
    punct
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(join)
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Ident(i) => fmt::Display::fmt(i, f),
            Pat::Lit(l) => fmt::Display::fmt(l, f),
            Pat::Macro(m) => fmt::Display::fmt(&m.to_token_stream(), f),
            Pat::Or(o) => fmt::Display::fmt(o, f),
            Pat::Paren(p) => fmt::Display::fmt(p, f),
            Pat::Path(p) => fmt::Display::fmt(p, f),
            Pat::Range(r) => fmt::Display::fmt(r, f),
            Pat::Rest(r) => fmt::Display::fmt(r, f),
            Pat::Slice(s) => fmt::Display::fmt(s, f),
            Pat::Struct(s) => fmt::Display::fmt(s, f),
            Pat::Tuple(t) => fmt::Display::fmt(t, f),
            Pat::TupleStruct(t) => fmt::Display::fmt(t, f),
            Pat::Type(t) => fmt::Display::fmt(t, f),
            Pat::Verbatim(v) => fmt::Display::fmt(v, f),
            Pat::Wild(w) => fmt::Display::fmt(w, f),
        }
    }
}

impl fmt::Display for PatIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((_, sub)) = &self.subpat {
            write!(f, "{} @ {}", self.ident, sub)
        } else {
            write!(f, "{}", self.ident)
        }
    }
}

impl fmt::Display for PatOr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.leading_vert.is_some() {
            write!(f, "| {}", punctuated_to_str(&self.cases, " | "))
        } else {
            write!(f, "")
        }
    }
}

impl fmt::Display for PatParen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.pat)
    }
}

impl fmt::Display for PatRest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "..")
    }
}

impl fmt::Display for PatSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", punctuated_to_str(&self.elems, ", "))
    }
}

impl fmt::Display for PatStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(_qself) = &self.qself {
            todo!("Struct patterns with qself")
        } else {
            write!(
                f,
                "{} {{ {} {} }}",
                &self.path.to_token_stream(),
                punctuated_to_str(&self.fields, ", "),
                if self.rest.is_some() { ", .." } else { "" }
            )
        }
    }
}

impl fmt::Display for PatTuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", punctuated_to_str(&self.elems, ", "))
    }
}

impl fmt::Display for PatTupleStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(_qself) = &self.qself {
            todo!("Tuple Struct patterns with qself")
        } else {
            write!(
                f,
                "{} ({})",
                self.path.to_token_stream(),
                punctuated_to_str(&self.elems, ", ")
            )
        }
    }
}

impl fmt::Display for PatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.pat, self.ty.to_token_stream())
    }
}

impl fmt::Display for PatWild {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_")
    }
}

impl fmt::Display for FieldPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.colon_token.is_some() {
            write!(f, "{}: {}", self.member.to_token_stream(), self.pat)
        } else {
            write!(f, "{}", self.member.to_token_stream())
        }
    }
}
