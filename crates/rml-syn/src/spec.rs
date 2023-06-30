use proc_macro2::{Delimiter, Span, TokenStream as TS2, TokenTree};
use quote::quote_spanned;
use syn::{
    parse::Parse,
    spanned::Spanned,
    token::{Brace, Bracket, Paren},
    FnArg, LitStr, MacroDelimiter, Token,
};

use crate::Term;

#[derive(Debug, Clone, Copy)]
pub enum SpecKind {
    Normal,
    Panic,
}

#[derive(Debug)]
pub enum SpecPart {
    Requires(SpecPartRequires),
    Ensures(SpecPartEnsures),
    Panics(SpecPartPanics),
}

#[derive(Debug)]
pub struct SpecPartRequires {
    pub requires_token: kw::requires,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

#[derive(Debug)]
pub struct SpecPartEnsures {
    pub ensures_token: kw::ensures,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

#[derive(Debug)]
pub struct SpecPartPanics {
    pub panics_token: kw::panics,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

/// A function specification.
///
/// # Grammar
/// spec ::= (name ',')? ((requires-part | ensures-part)* | (requires-part | panics-part)*)
///
/// name ::= STRING_LIT
///
/// requires-part ::= 'requires' ('(' term ')')?
///
/// ensures-part ::= 'ensures' ('(' term ')')?
///
/// panics-part ::= 'panics' ('(' term ')')?
///
/// term ::= _see [`Term`]_
#[derive(Debug)]
pub struct Spec {
    pub name: Option<String>,
    pub pre_conds: Vec<Term>,
    pub post_conds: Vec<Term>,
    pub kind: SpecKind,
}

impl Spec {
    pub fn encode(&self, result: FnArg, sp: Span) -> TS2 {
        let pre: Vec<_> = self
            .pre_conds
            .iter()
            .map(|t| {
                let sp = t.span();
                let e = t.encode();
                quote_spanned! {
                    sp => || { #e }
                }
            })
            .collect();
        let post: Vec<_> = self
            .post_conds
            .iter()
            .map(|t| {
                let sp = t.span();
                let e = t.encode();
                quote_spanned! {
                    sp => |#result| { #e }
                }
            })
            .collect();
        let s = match self.kind {
            SpecKind::Normal => {
                quote_spanned! {
                    sp => rml::SpecificationNormal {
                        pre: vec![#(#pre,)*],
                        post: vec![#(#post,)*]
                    }
                }
            }
            SpecKind::Panic => {
                quote_spanned! {
                    sp => rml::SpecificationPanic {
                        pre: vec![#(#pre,)*],
                        post: vec![#(#post,)*]
                    }
                }
            }
        };
        syn::parse(s.into()).unwrap()
    }
}

impl Parse for SpecPartRequires {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let t = input.parse()?;
        let (delimiter, term) = if input.peek(Paren) || input.peek(Bracket) || input.peek(Brace) {
            let (del, tokens) = parse_delimiter(input)?;
            if tokens.is_empty() {
                (Some(del), None)
            } else {
                let term = syn::parse(tokens.into())?;
                (Some(del), Some(term))
            }
        } else {
            (None, None)
        };
        Ok(Self {
            requires_token: t,
            delimiter,
            term,
        })
    }
}

impl Parse for SpecPartEnsures {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let t = input.parse()?;
        let (delimiter, term) = if input.peek(Paren) || input.peek(Bracket) || input.peek(Brace) {
            let (del, tokens) = parse_delimiter(input)?;
            if tokens.is_empty() {
                (Some(del), None)
            } else {
                let term = syn::parse(tokens.into())?;
                (Some(del), Some(term))
            }
        } else {
            (None, None)
        };
        Ok(Self {
            ensures_token: t,
            delimiter,
            term,
        })
    }
}

impl Parse for SpecPartPanics {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let t = input.parse()?;
        let (delimiter, term) = if input.peek(Paren) || input.peek(Bracket) || input.peek(Brace) {
            let (del, tokens) = parse_delimiter(input)?;
            if tokens.is_empty() {
                (Some(del), None)
            } else {
                let term = syn::parse(tokens.into())?;
                (Some(del), Some(term))
            }
        } else {
            (None, None)
        };
        Ok(Self {
            panics_token: t,
            delimiter,
            term,
        })
    }
}

mod kw {
    syn::custom_keyword!(requires);
    syn::custom_keyword!(ensures);
    syn::custom_keyword!(panics);
}

impl Parse for SpecPart {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(kw::requires) {
            Ok(Self::Requires(input.parse()?))
        } else if input.peek(kw::ensures) {
            Ok(Self::Ensures(input.parse()?))
        } else if input.peek(kw::panics) {
            Ok(Self::Panics(input.parse()?))
        } else {
            Err(input.error("expected one of requires, ensures, or panics"))
        }
    }
}

pub(crate) fn parse_delimiter(
    input: syn::parse::ParseStream,
) -> syn::Result<(MacroDelimiter, proc_macro2::TokenStream)> {
    input.step(|cursor| {
        if let Some((TokenTree::Group(g), rest)) = cursor.token_tree() {
            let span = g.delim_span();
            let delimiter = match g.delimiter() {
                Delimiter::Parenthesis => MacroDelimiter::Paren(Paren(span)),
                Delimiter::Brace => MacroDelimiter::Brace(Brace(span)),
                Delimiter::Bracket => MacroDelimiter::Bracket(Bracket(span)),
                Delimiter::None => {
                    return Err(cursor.error("expected delimiter"));
                }
            };
            Ok(((delimiter, g.stream()), rest))
        } else {
            Err(cursor.error("expected delimiter"))
        }
    })
}

impl Parse for Spec {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut pre_conds = Vec::new();
        let mut post_conds = Vec::new();
        let mut kind = None;

        let name = input.parse::<Option<LitStr>>()?.map(|l| l.value());
        if name.is_some() {
            let _: Token![,] = input.parse()?;
        }

        loop {
            let part: SpecPart = input.parse()?;
            match part {
                SpecPart::Requires(SpecPartRequires { term, .. }) => {
                    if let Some(t) = term {
                        pre_conds.push(t)
                    }
                }
                SpecPart::Ensures(SpecPartEnsures { term, .. }) => {
                    match kind {
                        Some(SpecKind::Normal) => {}
                        Some(SpecKind::Panic) => {
                            return Err(syn::Error::new(
                                input.span(),
                                "The specification has both ensures and panics clauses; it may only have post conditions of one type",
                            ));
                        }
                        None => kind = Some(SpecKind::Normal),
                    }
                    if let Some(t) = term {
                        post_conds.push(t)
                    }
                }
                SpecPart::Panics(SpecPartPanics { term, .. }) => {
                    match kind {
                        Some(SpecKind::Panic) => {}
                        Some(SpecKind::Normal) => {
                            return Err(syn::Error::new(
                                input.span(),
                                "The specification has both ensures and panics clauses; it may only have post conditions of one type",
                            ));
                        }
                        None => kind = Some(SpecKind::Panic),
                    }
                    if let Some(t) = term {
                        post_conds.push(t)
                    }
                }
            }
            if !input.peek(Token![,]) {
                break;
            }
            let _: Token![,] = input.parse()?;
        }

        if !input.is_empty() {
            return Err(input.error("expected `,` or end of input"));
        }

        let kind = match kind {
            Some(k) => k,
            None => {
                return Err(syn::Error::new(
                    input.span(),
                    "Specification has no post condition of any kind",
                ))
            }
        };

        Ok(Self {
            name,
            pre_conds,
            post_conds,
            kind,
        })
    }
}
