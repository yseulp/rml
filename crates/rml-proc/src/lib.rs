use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Ident, TokenTree};
use rml_syn::{Term, TermClosure};
use syn::{
    parse::Parse,
    parse_macro_input,
    token::{self, Brace, Bracket, Paren},
    LitStr, MacroDelimiter, Token,
};

#[derive(Debug, Clone, Copy)]
enum SpecKind {
    Normal,
    Panic,
}

#[derive(Debug)]
enum SpecPart {
    Requires(SpecPartRequires),
    Ensures(SpecPartEnsures),
    Panics(SpecPartPanics),
}

#[derive(Debug)]
struct SpecPartRequires {
    pub requires_token: kw::requires,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

#[derive(Debug)]
struct SpecPartEnsures {
    pub ensures_token: kw::ensures,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

#[derive(Debug)]
struct SpecPartPanics {
    pub panics_token: kw::panics,
    pub delimiter: Option<MacroDelimiter>,
    pub term: Option<Term>,
}

#[derive(Debug)]
struct Spec {
    name: Option<String>,
    pre_conds: Vec<Term>,
    post_conds: Vec<Term>,
    kind: SpecKind,
}

impl Parse for SpecPartRequires {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let t = input.parse()?;
        let (delimiter, term) =
            if input.peek(Paren) || input.peek(token::Bracket) || input.peek(token::Brace) {
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
        let (delimiter, term) =
            if input.peek(Paren) || input.peek(token::Bracket) || input.peek(token::Brace) {
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
        let (delimiter, term) =
            if input.peek(Paren) || input.peek(token::Bracket) || input.peek(token::Brace) {
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

        let name = input.parse::<LitStr>()?.value();

        while input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;
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
            name: Some(name),
            pre_conds,
            post_conds,
            kind,
        })
    }
}

#[proc_macro_attribute]
pub fn spec(attr: TokenStream, item: TokenStream) -> TokenStream {
    let sp = parse_macro_input!(attr as Spec);
    println!("{:#?}", sp);
    item
}

#[proc_macro_attribute]
pub fn requires(attr: TokenStream, item: TokenStream) -> TokenStream {
    todo!()
}
