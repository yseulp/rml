//! RML Attributes for functions or data structures.

use quote::ToTokens;
use syn::{
    bracketed, parenthesized,
    parse::{Parse, ParseStream},
    token, MacroDelimiter, Token,
};

use crate::{parse_delimiter, surround_delim, SpecContent, Term};

mod kw {
    syn::custom_keyword!(pure);
    syn::custom_keyword!(strictly_pure);
    syn::custom_keyword!(invariant);
    syn::custom_keyword!(spec);
}

/// Permissable attributes for functions.
/// Used for extern specs.
pub enum FnAttribute {
    Pure(AttributePure),
    StrictlyPure(AttributeStrictlyPure),
    Spec(AttributeSpec),
}

/// Attribute declaring pure functions/methods.
pub struct AttributePure {
    pub pound_token: Token![#],
    pub bracket_token: token::Bracket,
    pub pure_token: kw::pure,
}

/// Attribute declaring strictly pure functions/methods.
pub struct AttributeStrictlyPure {
    pub pound_token: Token![#],
    pub bracket_token: token::Bracket,
    pub strictly_pure_token: kw::strictly_pure,
}

/// Attribute declaring invariants for data structures and traits..
pub struct AttributeInvariant {
    pub pound_token: Token![#],
    pub bracket_token: token::Bracket,
    pub inv_token: kw::invariant,
    pub paren_token: token::Paren,
    pub term: Term,
}

/// Attribute declaring specification case for functions.
pub struct AttributeSpec {
    pub pound_token: Token![#],
    pub bracket_token: token::Bracket,
    pub spec_token: kw::spec,
    pub delimiter: MacroDelimiter,
    pub spec: SpecContent,
}

impl Parse for FnAttribute {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let pound_token = input.parse()?;
        let content;
        let bracket_token = bracketed!(content in input);
        let a = if content.peek(kw::pure) {
            let pure_token = content.parse()?;
            Self::Pure(AttributePure {
                pound_token,
                bracket_token,
                pure_token,
            })
        } else if content.peek(kw::strictly_pure) {
            let strictly_pure_token = content.parse()?;
            Self::StrictlyPure(AttributeStrictlyPure {
                pound_token,
                bracket_token,
                strictly_pure_token,
            })
        } else if content.peek(kw::spec) {
            let spec_token = content.parse()?;
            let (delimiter, tokens) = parse_delimiter(&content)?;
            let spec = syn::parse(tokens.into())?;

            Self::Spec(AttributeSpec {
                pound_token,
                bracket_token,
                spec_token,
                delimiter,
                spec,
            })
        } else {
            return Err(content.error("Expected pure, strictly_pure, or spec"));
        };
        if !content.is_empty() {
            return Err(content.error("Expected end of input"));
        }
        Ok(a)
    }
}

impl Parse for AttributeInvariant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pound_token = input.parse()?;
        let content;
        let bracket_token = bracketed!(content in input);
        let inv_token = content.parse()?;
        let inner;
        let paren_token = parenthesized!(inner in content);
        let term = inner.parse()?;

        Ok(Self {
            pound_token,
            bracket_token,
            inv_token,
            paren_token,
            term,
        })
    }
}

impl ToTokens for FnAttribute {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            FnAttribute::Pure(a) => a.to_tokens(tokens),
            FnAttribute::StrictlyPure(a) => a.to_tokens(tokens),
            FnAttribute::Spec(a) => a.to_tokens(tokens),
        }
    }
}

impl ToTokens for AttributePure {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.pound_token.to_tokens(tokens);
        self.bracket_token.surround(tokens, |tokens| {
            self.pure_token.to_tokens(tokens);
        })
    }
}

impl ToTokens for AttributeStrictlyPure {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.pound_token.to_tokens(tokens);
        self.bracket_token.surround(tokens, |tokens| {
            self.strictly_pure_token.to_tokens(tokens);
        })
    }
}

impl ToTokens for AttributeSpec {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.pound_token.to_tokens(tokens);
        self.bracket_token.surround(tokens, |tokens| {
            self.spec_token.to_tokens(tokens);
            surround_delim(&self.delimiter, tokens, |tokens| {
                self.spec.to_tokens(tokens)
            });
        })
    }
}

impl ToTokens for AttributeInvariant {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.pound_token.to_tokens(tokens);
        self.bracket_token.surround(tokens, |tokens| {
            self.inv_token.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.term.to_tokens(tokens);
            })
        })
    }
}

/// Greedily parse a series of attributes.
pub fn parse_attrs<T>(input: ParseStream) -> syn::Result<Vec<T>>
where
    T: Parse,
{
    let mut attrs = Vec::new();
    while input.peek(Token![#]) {
        attrs.push(input.parse()?);
    }
    Ok(attrs)
}
