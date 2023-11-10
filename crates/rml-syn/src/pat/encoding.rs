use super::*;
use crate::{term::encoding::encode_punctuated, Encode};

impl Encode for Pat {
    type Target = syn::Pat;

    fn encode(self) -> Self::Target {
        match self {
            Pat::Ident(PatIdent { ident, subpat }) => syn::Pat::Ident(syn::PatIdent {
                attrs: Vec::new(),
                by_ref: None,
                mutability: None,
                ident,
                subpat: subpat.map(|(at, sub)| (at, sub.encode().into())),
            }),
            Pat::Lit(PatLit { lit }) => syn::Pat::Lit(syn::PatLit {
                attrs: Vec::new(),
                lit,
            }),
            Pat::Macro(m) => syn::Pat::Macro(m),
            Pat::Or(PatOr {
                leading_vert,
                cases,
            }) => syn::Pat::Or(syn::PatOr {
                attrs: Vec::new(),
                leading_vert,
                cases: encode_punctuated(cases),
            }),
            Pat::Paren(PatParen { paren_token, pat }) => syn::Pat::Paren(syn::PatParen {
                attrs: Vec::new(),
                paren_token,
                pat: pat.encode().into(),
            }),
            Pat::Path(PatPath { inner }) => syn::Pat::Path(inner),
            Pat::Range(PatRange { start, limits, end }) => syn::Pat::Range(syn::PatRange {
                attrs: Vec::new(),
                start: start.map(|s| s.encode().into()),
                limits,
                end: end.map(|e| e.encode().into()),
            }),
            Pat::Rest(PatRest { dot2_token }) => syn::Pat::Rest(syn::PatRest {
                attrs: Vec::new(),
                dot2_token,
            }),
            Pat::Slice(PatSlice {
                bracket_token,
                elems,
            }) => syn::Pat::Slice(syn::PatSlice {
                attrs: Vec::new(),
                bracket_token,
                elems: encode_punctuated(elems),
            }),
            Pat::Struct(PatStruct {
                qself,
                path,
                brace_token,
                fields,
                rest,
            }) => syn::Pat::Struct(syn::PatStruct {
                attrs: Vec::new(),
                qself,
                path,
                brace_token,
                fields: encode_punctuated(fields),
                rest: rest.map(|r| r.encode()),
            }),
            Pat::Tuple(PatTuple { paren_token, elems }) => syn::Pat::Tuple(syn::PatTuple {
                attrs: Vec::new(),
                paren_token,
                elems: encode_punctuated(elems),
            }),
            Pat::TupleStruct(PatTupleStruct {
                qself,
                path,
                paren_token,
                elems,
            }) => syn::Pat::TupleStruct(syn::PatTupleStruct {
                attrs: Vec::new(),
                qself,
                path,
                paren_token,
                elems: encode_punctuated(elems),
            }),
            Pat::Type(PatType {
                pat,
                colon_token,
                ty,
            }) => syn::Pat::Type(syn::PatType {
                attrs: Vec::new(),
                pat: pat.encode().into(),
                colon_token,
                ty,
            }),
            Pat::Verbatim(ts) => syn::Pat::Verbatim(ts),
            Pat::Wild(PatWild { underscore_token }) => syn::Pat::Wild(syn::PatWild {
                attrs: Vec::new(),
                underscore_token,
            }),
        }
    }
}

impl Encode for FieldPat {
    type Target = syn::FieldPat;

    fn encode(self) -> Self::Target {
        syn::FieldPat {
            attrs: Vec::new(),
            member: self.member,
            colon_token: self.colon_token,
            pat: self.pat.encode().into(),
        }
    }
}

impl Encode for PatRest {
    type Target = syn::PatRest;

    fn encode(self) -> Self::Target {
        syn::PatRest {
            attrs: Vec::new(),
            dot2_token: self.dot2_token,
        }
    }
}
