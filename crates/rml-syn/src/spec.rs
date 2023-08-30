//! RML specification.

use std::fmt;

use proc_macro2::{Delimiter, TokenTree};
use syn::{
    braced, bracketed, parenthesized,
    parse::{Parse, ParseStream},
    token::{self, Brace, Bracket, Paren},
    LitStr, MacroDelimiter, Token,
};

use crate::{locset::LocSet, Term};

/// The kind of a specification, i.e., whether it specifies normal
/// or panic behavior.
#[derive(Debug, Clone, Copy)]
pub enum SpecKind {
    /// Normal behavior.
    Normal,
    /// Panic behavior.
    Panic,
}
impl SpecKind {
    fn is_normal(&self) -> bool {
        matches!(self, Self::Normal)
    }
}

/// Part of a spec.
#[derive(Debug)]
pub enum SpecPart {
    /// `requires` takes a boolean term and specifies the pre-condition.
    /// Defaults to `true`.
    Requires(SpecPartRequires),
    /// `ensures` takes a boolean term and specifies the post-condition in normal behavior.
    /// Defaults to `true`.
    Ensures(SpecPartEnsures),
    /// `panics` takes a boolean term and specifies the post-condition in panic behavior.
    /// Defaults to `true`.
    Panics(SpecPartPanics),
    /// `modifies` takes a [LocSet] and specifies the locations the function may change.
    /// Defaults to ???.
    Modifies(SpecPartModifies),
    /// `variant` takes any term implementing the `WellFounded` trait defined in `rml-contracts`.
    /// It specifies the variant for a recursive function.
    /// Default is no variant.
    Variant(SpecPartVariant),
    /// `diverges` takes a boolean term and specifies under what condition
    /// Defaults to `false` if no `diverges` part exists and `true` if it exists but is empty.
    Diverges(SpecPartDiverges),
}

/// `requires` takes a boolean term and specifies the pre-condition.
/// Defaults to `true`.
#[derive(Debug)]
pub struct SpecPartRequires {
    pub requires_token: kw::requires,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

/// `ensures` takes a boolean term and specifies the post-condition in normal behavior.
/// Defaults to `true`.
#[derive(Debug)]
pub struct SpecPartEnsures {
    pub ensures_token: kw::ensures,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

/// `panics` takes a boolean term and specifies the post-condition in panic behavior.
/// Defaults to `true`.
#[derive(Debug)]
pub struct SpecPartPanics {
    pub panics_token: kw::panics,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

/// `modifies` takes a [LocSet] and specifies the locations the function may change.
/// Defaults to ???.
#[derive(Debug)]
pub struct SpecPartModifies {
    pub modifies_token: kw::modifies,
    pub delimiter: MacroDelimiter,
    pub locset: LocSet,
}

/// `variant` takes any term implementing the `WellFounded` trait defined in `rml-contracts`.
/// It specifies the variant for a recursive function.
/// Default is no variant.
#[derive(Debug)]
pub struct SpecPartVariant {
    pub variant_token: kw::variant,
    pub delimiter: MacroDelimiter,
    pub term: Term,
}

/// `diverges` takes a boolean term and specifies under what condition
/// Defaults to `false` if no `diverges` part exists and `true` if it exists but is empty.
#[derive(Debug)]
pub struct SpecPartDiverges {
    pub diverges_token: kw::diverges,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

/// A function specification.
///
/// # Grammar
/// spec ::= (name ',')? (normal-spec-part | panics-spec-part)*
///
/// name ::= STRING_LIT
///
/// normal-spec-part ::= requires-part
///                     | ensures-part
///                     | modifies-part
///                     | variant-part
///                     | diverges-part
///
/// panics-spec-part ::= requires-part
///                     | panics-part
///                     | modifies-part
///                     | variant-part
///                     | diverges-part
///
/// requires-part ::= 'requires' ('(' term ')')?
///
/// ensures-part ::= 'ensures' ('(' term ')')?
///
/// panics-part ::= 'panics' ('(' term ')')?
///
/// modifies-part ::= 'modifies' ('(' term-loc-set ')')
///
/// variant-part ::= 'variant' ('(' term ')')
///
/// diverges-part ::= 'diverges' ('(' term ')')?
///
/// term ::= _see [`Term`]_
#[derive(Debug)]
pub struct Spec {
    pub name: Option<String>,
    pub kind: SpecKind,
    pub pre_conds: Vec<Term>,
    pub post_conds: Vec<Term>,
    pub modifies: Option<LocSet>,
    pub variant: Option<Term>,
    pub diverges: Option<Option<Term>>,
}

impl Spec {
    pub fn is_normal(&self) -> bool {
        self.kind.is_normal()
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

impl Parse for SpecPartModifies {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let modifies_token = input.parse()?;
        let content;
        let delimiter = if input.peek(token::Paren) {
            MacroDelimiter::Paren(parenthesized!(content in input))
        } else if input.peek(token::Brace) {
            MacroDelimiter::Brace(braced!(content in input))
        } else if input.peek(token::Bracket) {
            MacroDelimiter::Bracket(bracketed!(content in input))
        } else {
            return Err(syn::Error::new(input.span(), "Expeted delimiter"));
        };

        Ok(Self {
            modifies_token,
            delimiter,
            locset: content.parse()?,
        })
    }
}

impl Parse for SpecPartVariant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let variant_token = input.parse()?;
        let (delimiter, tokens) = parse_delimiter(input)?;
        let term = syn::parse(tokens.into())?;

        Ok(Self {
            variant_token,
            delimiter,
            term,
        })
    }
}

impl Parse for SpecPartDiverges {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let diverges_token = input.parse()?;
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
            diverges_token,
            delimiter,
            term,
        })
    }
}

mod kw {
    syn::custom_keyword!(requires);
    syn::custom_keyword!(ensures);
    syn::custom_keyword!(panics);
    syn::custom_keyword!(modifies);
    syn::custom_keyword!(variant);
    syn::custom_keyword!(diverges);
}

impl Parse for SpecPart {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(kw::requires) {
            Ok(Self::Requires(input.parse()?))
        } else if input.peek(kw::ensures) {
            Ok(Self::Ensures(input.parse()?))
        } else if input.peek(kw::panics) {
            Ok(Self::Panics(input.parse()?))
        } else if input.peek(kw::modifies) {
            Ok(Self::Modifies(input.parse()?))
        } else if input.peek(kw::variant) {
            Ok(Self::Variant(input.parse()?))
        } else if input.peek(kw::diverges) {
            Ok(Self::Diverges(input.parse()?))
        } else {
            Err(input
                .error("expected one of requires, ensures, panics, modifies, variant, or diverges"))
        }
    }
}

/// Parse a macro delimiter (parentheses, brackets, braces) and returns the tokens as well as
/// the tokenstream between the delimiters.
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
        let mut modifies = None;
        let mut variant = None;
        let mut diverges = None;
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
                SpecPart::Modifies(SpecPartModifies { locset, .. }) => {
                    if modifies.is_some() {
                        return Err(syn::Error::new(
                            input.span(),
                            "The specification has multiple declarations of 'modifies'",
                        ));
                    } else {
                        modifies = Some(locset)
                    }
                }
                SpecPart::Variant(SpecPartVariant { term, .. }) => {
                    if variant.is_some() {
                        return Err(syn::Error::new(
                            input.span(),
                            "The specification has multiple declarations of 'variant'",
                        ));
                    } else {
                        variant = Some(term)
                    }
                }
                SpecPart::Diverges(SpecPartDiverges { term, .. }) => {
                    if diverges.is_some() {
                        return Err(syn::Error::new(
                            input.span(),
                            "The specification has multiple declarations of 'diverges'",
                        ));
                    } else {
                        diverges = Some(term)
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
            modifies,
            variant,
            diverges,
            kind,
        })
    }
}

impl fmt::Display for Spec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match &self.name {
            Some(n) => n,
            None => "<UNNAMED>",
        };
        let mut r = writeln!(f, "Spec({name}) {{").and(writeln!(f, "\tpre: ["));
        for p in &self.pre_conds {
            r = r.and(writeln!(f, "\t\t{},", p));
        }
        r = r.and(writeln!(f, "\t],")).and(match &self.kind {
            SpecKind::Normal => writeln!(f, "\tpost: ["),
            SpecKind::Panic => writeln!(f, "\tpanics: ["),
        });
        for p in &self.post_conds {
            r = r.and(writeln!(f, "\t\t{p},"))
        }

        r.and(writeln!(f, "\t]")).and(writeln!(f, "}}"))
    }
}
