use syn::{bracketed, parse::Parse, punctuated::Punctuated, token, Token};

use super::{
    kw, LocSet, LocSetField, LocSetFieldWildcard, LocSetGroup, LocSetIndex, LocSetNothing,
    LocSetPath,
};
use crate::{Term, TermPath};

impl Parse for LocSet {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(kw::nothing) {
            return Ok(LocSet::Nothing(input.parse()?));
        }

        let path: TermPath = input.parse()?;

        let locset = if input.peek(Token![.]) && input.peek2(Token![*]) {
            LocSet::FieldWildcard(LocSetFieldWildcard {
                base: Term::Path(path).into(),
                dot_token: input.parse()?,
                star_token: input.parse()?,
            })
        } else if input.peek(Token![.]) {
            LocSet::Field(LocSetField {
                base: Term::Path(path).into(),
                dot_token: input.parse()?,
                member: input.parse()?,
            })
        } else if input.peek(token::Bracket) {
            let content;
            let bracket_token = bracketed!(content in input);
            let index: Box<_> = content.parse::<Term>()?.into();

            LocSet::Index(super::LocSetIndex {
                term: Term::Path(path).into(),
                bracket_token,
                index,
            })
        } else {
            LocSet::Path(LocSetPath { inner: path })
        };

        if input.peek(Token![,]) {
            let mut items = Punctuated::new();
            items.push_value(locset);
            items.push_punct(input.parse()?);

            while input.peek(Token![,]) {
                items.push_value(input.parse()?);
                items.push_punct(input.parse()?);
            }

            return Ok(LocSet::Group(LocSetGroup { items }));
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
        let items = Punctuated::parse_separated_nonempty(input)?;

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
