//! Contains the datatype for RML subjects, i.e., functions, methods, loops,
//! etc.
//!
//! Adapted from creusot.

use proc_macro2::TokenStream as TS2;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse::{self, Parse},
    Attribute, ItemFn, Result, Signature, Token, Type, Visibility,
};

use crate::FilterAttrs;

/// The subject of a `logic` attribute.
#[derive(Debug)]
pub enum LogicSubject {
    /// A function with a body.
    WithBody(ItemFn),
    /// A trait function with no body.
    WithoutBody(TraitFnWithoutBody),
}

/// A function in a trait declaration without a body.
#[derive(Debug)]
pub struct TraitFnWithoutBody {
    pub attrs: Vec<Attribute>,
    pub sig: Signature,
    pub semi_token: Token![;],
}

impl Parse for LogicSubject {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis: Visibility = input.parse()?;
        let sig: Signature = input.parse()?;
        match sig.output {
            syn::ReturnType::Default => {
                return Err(input.error("logic functions must have a return type"))
            }
            syn::ReturnType::Type(_, box Type::Never(_)) => {
                return Err(input.error("logic functions cannot return `!`"))
            }
            syn::ReturnType::Type(_, box Type::Tuple(t)) if t.elems.is_empty() => {
                return Err(input.error("logic functions must have a return type other than `()`"))
            }
            _ => {}
        };
        if input.peek(Token![;]) {
            if !matches!(vis, Visibility::Inherited) {
                return Err(input.error("Trait methods must not have a visibility modifier"));
            }
            Ok(Self::WithoutBody(TraitFnWithoutBody {
                attrs,
                sig,
                semi_token: input.parse()?,
            }))
        } else {
            Ok(Self::WithBody(ItemFn {
                attrs,
                vis,
                sig,
                block: input.parse()?,
            }))
        }
    }
}

impl ToTokens for TraitFnWithoutBody {
    fn to_tokens(&self, tokens: &mut TS2) {
        tokens.append_all(self.attrs.outer());
        self.sig.to_tokens(tokens);
        self.semi_token.to_tokens(tokens);
    }
}
