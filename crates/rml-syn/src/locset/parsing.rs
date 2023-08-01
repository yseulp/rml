use syn::{bracketed, parse::Parse, punctuated::Punctuated, token, Token};

use crate::Term;

use super::{
    kw, LocSetField, LocSetFieldWildcard, LocSetGroup, LocSetIndex, LocSetNothing, LocSetPath,
    LocSetTerm,
};

impl Parse for LocSetTerm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(kw::nothing) {
            return Ok(LocSetTerm::Nothing(input.parse()?));
        }

        let term: Term = input.parse()?;

        let locset = if let Term::Path(p) = term {
            LocSetTerm::Path(LocSetPath { inner: p })
        } else if input.peek(Token![.]) && input.peek2(Token![*]) {
            LocSetTerm::FieldWildcard(LocSetFieldWildcard {
                base: term.into(),
                dot_token: input.parse()?,
                star_token: input.parse()?,
            })
        } else if input.peek(Token![.]) {
            LocSetTerm::Field(LocSetField {
                base: term.into(),
                dot_token: input.parse()?,
                member: input.parse()?,
            })
        } else if input.peek(token::Bracket) {
            let content;
            let bracket_token = bracketed!(content in input);
            let index: Box<_> = content.parse::<Term>()?.into();

            LocSetTerm::Index(super::LocSetIndex {
                term: term.into(),
                bracket_token,
                index,
            })
        } else {
            return Err(input.error("Expected `.` or `[`"));
        };

        if input.peek(Token![,]) {
            let mut items = Punctuated::new();
            items.push_value(locset);
            items.push_punct(input.parse()?);

            while input.peek(Token![,]) {
                items.push_value(input.parse()?);
                items.push_punct(input.parse()?);
            }

            return Ok(LocSetTerm::Group(LocSetGroup { items }));
        }

        Ok(locset)
    }
}

impl Parse for LocSetField {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            base: input.parse::<Term>()?.into(),
            dot_token: input.parse()?,
            member: input.parse()?,
        })
    }
}

impl Parse for LocSetFieldWildcard {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            base: input.parse::<Term>()?.into(),
            dot_token: input.parse()?,
            star_token: input.parse()?,
        })
    }
}

impl Parse for LocSetPath {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            inner: input.parse()?,
        })
    }
}

impl Parse for LocSetIndex {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            term: input.parse::<Term>()?.into(),
            bracket_token: bracketed!(content in input),
            index: content.parse()?,
        })
    }
}

impl Parse for LocSetGroup {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut items = Punctuated::new();
        items.push_value(input.parse()?);

        while input.peek(Token![,]) {
            items.push_punct(input.parse()?);
            items.push_value(input.parse()?);
        }

        Ok(Self { items })
    }
}

impl Parse for LocSetNothing {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            nothing_token: input.parse()?,
        })
    }
}
