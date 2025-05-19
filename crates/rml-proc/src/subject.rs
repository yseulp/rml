use std::iter;

use proc_macro2::TokenStream as TS2;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    braced,
    parse::{self, Parse},
    spanned::Spanned,
    token, AttrStyle, Attribute, Block, Expr, ExprClosure, ExprForLoop, ExprLoop, ExprWhile,
    ItemEnum, ItemStruct, ItemTrait, Label, Lifetime, Result, Signature, Token, Visibility,
};

/// A function or method, which may either have a body or a semicolon.
pub(crate) struct FnOrMethod {
    pub defaultness: Option<Token![default]>,
    pub visibility: Visibility,
    pub attrs: Vec<Attribute>,
    pub sig: Signature,
    pub body: Option<Block>,
    pub semi_token: Option<Token![;]>,
}

impl FnOrMethod {
    pub fn is_trait_signature(&self) -> bool {
        self.semi_token.is_some()
    }
}

/// The subject or target of a specification case. Either a function/method, or
/// a closure.
pub(crate) enum ContractSubject {
    FnOrMethod(Box<FnOrMethod>),
    Closure(ExprClosure),
}

impl ToTokens for FnOrMethod {
    fn to_tokens(&self, tokens: &mut TS2) {
        tokens.append_all(self.attrs.outer());
        self.defaultness.to_tokens(tokens);
        self.visibility.to_tokens(tokens);
        self.sig.to_tokens(tokens);
        self.body.to_tokens(tokens);
        self.semi_token.to_tokens(tokens);
    }
}

impl ContractSubject {
    /// Name of the subject. If called on a function or method, returns the
    /// name. For closures, it defaults to `"closure"`.
    pub fn name(&self) -> String {
        match self {
            ContractSubject::FnOrMethod(tr) => tr.sig.ident.to_string(),
            ContractSubject::Closure(_) => "closure".to_string(),
        }
    }
}

impl Parse for ContractSubject {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        if input.peek(Token![|])
            || input.peek(Token![async]) && (input.peek2(Token![|]) || input.peek2(Token![move]))
            || input.peek(Token![static])
            || input.peek(Token![move])
        {
            let mut closure: ExprClosure = input.parse()?;
            let _: Option<Token![,]> = input.parse()?;
            closure.attrs.extend(attrs);
            return Ok(ContractSubject::Closure(closure));
        }

        let defaultness: Option<_> = input.parse()?;
        // Infalliable, no visibility = inherited
        let vis: Visibility = input.parse()?;
        let sig: Signature = input.parse()?;
        let lookahead = input.lookahead1();

        let (brace_token, stmts, semi_token) = if lookahead.peek(token::Brace) {
            let content;
            let brace_token = braced!(content in input);

            let stmts = content.call(Block::parse_within)?;
            (Some(brace_token), stmts, None)
        } else if lookahead.peek(Token![;]) {
            let semi_token: Token![;] = input.parse()?;
            (None, Vec::new(), Some(semi_token))
        } else {
            return Err(lookahead.error());
        };

        Ok(ContractSubject::FnOrMethod(Box::new(FnOrMethod {
            defaultness,
            visibility: vis,
            attrs,
            sig,
            body: brace_token.map(|brace_token| Block { brace_token, stmts }),
            semi_token,
        })))
    }
}

trait FilterAttrs<'a> {
    type Ret: Iterator<Item = &'a Attribute>;

    fn outer(self) -> Self::Ret;
    fn inner(self) -> Self::Ret;
}

impl<'a> FilterAttrs<'a> for &'a [Attribute] {
    type Ret = iter::Filter<std::slice::Iter<'a, Attribute>, fn(&&Attribute) -> bool>;

    fn outer(self) -> Self::Ret {
        fn is_outer(attr: &&Attribute) -> bool {
            match attr.style {
                AttrStyle::Outer => true,
                AttrStyle::Inner(_) => false,
            }
        }
        self.iter().filter(is_outer)
    }

    fn inner(self) -> Self::Ret {
        fn is_inner(attr: &&Attribute) -> bool {
            match attr.style {
                AttrStyle::Inner(_) => true,
                AttrStyle::Outer => false,
            }
        }
        self.iter().filter(is_inner)
    }
}

/// The three kinds of Rust loop.
#[derive(Debug)]
pub enum LoopKind {
    /// A `for pat in expr { ... }` loop.
    ForLoop(ExprForLoop),
    /// A `loop { ... }`.
    Loop(ExprLoop),
    /// A `while expr` loop.
    While(ExprWhile),
}

impl LoopKind {
    /// The span of the loop.
    pub fn span(&self) -> proc_macro2::Span {
        match self {
            LoopKind::ForLoop(l) => l.span(),
            LoopKind::Loop(l) => l.span(),
            LoopKind::While(l) => l.span(),
        }
    }

    /// A mutable reference to the attributes.
    pub fn attrs_mut(&mut self) -> &mut Vec<Attribute> {
        match self {
            LoopKind::ForLoop(l) => &mut l.attrs,
            LoopKind::Loop(l) => &mut l.attrs,
            LoopKind::While(l) => &mut l.attrs,
        }
    }
}

/// The kind of items that can have invariants.
#[derive(Debug)]
pub enum ItemKind {
    /// A trait, e.g., `trait T { ... }`.
    Trait(ItemTrait),
    /// A struct, e.g., `struct S { ... }`.
    Struct(ItemStruct),
    /// An enum, e.g., `enum E { ... }`.
    Enum(ItemEnum),
}

impl ToTokens for ItemKind {
    fn to_tokens(&self, tokens: &mut TS2) {
        match self {
            ItemKind::Trait(i) => i.to_tokens(tokens),
            ItemKind::Struct(i) => i.to_tokens(tokens),
            ItemKind::Enum(i) => i.to_tokens(tokens),
        }
    }
}

/// Subject or target for invariant attributes. Either a loop (loop invariant)
/// or struct/enum/trait ("object" invariant).
#[derive(Debug)]
pub enum InvariantSubject {
    Loop(LoopKind),
    Item(ItemKind),
}

impl InvariantSubject {
    /// Span of the subject.
    pub fn span(&self) -> proc_macro2::Span {
        match self {
            InvariantSubject::Loop(l) => l.span(),
            InvariantSubject::Item(i) => i.span(),
        }
    }
}

impl Parse for InvariantSubject {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let ahead = input.fork();
        ahead.call(Attribute::parse_outer)?;
        let vis = ahead.parse::<Visibility>()?;

        let lookahead = ahead.lookahead1();

        if lookahead.peek(Token![struct]) {
            Ok(Self::Item(ItemKind::Struct(input.parse()?)))
        } else if lookahead.peek(Token![enum]) {
            Ok(Self::Item(ItemKind::Enum(input.parse()?)))
        } else if lookahead.peek(Token![unsafe]) {
            ahead.parse::<Token![unsafe]>()?;
            let lookahead = ahead.lookahead1();
            if lookahead.peek(Token![trait])
                || lookahead.peek(Token![auto]) && ahead.peek2(Token![trait])
            {
                Ok(Self::Item(ItemKind::Trait(input.parse()?)))
            } else {
                Err(lookahead.error())
            }
        } else if lookahead.peek(Token![trait]) {
            Ok(Self::Item(ItemKind::Trait(input.parse()?)))
        } else {
            if !matches!(vis, Visibility::Inherited) {
                return Err(syn::Error::new(
                    input.span(),
                    "Loop constructs must not have a visibility",
                ));
            }
            if lookahead.peek(Token![for]) {
                Ok(Self::Loop(LoopKind::ForLoop(input.parse()?)))
            } else if lookahead.peek(Token![loop]) {
                Ok(Self::Loop(LoopKind::Loop(input.parse()?)))
            } else if lookahead.peek(Token![while]) {
                Ok(Self::Loop(LoopKind::While(input.parse()?)))
            } else if lookahead.peek(Lifetime) {
                let the_label: Label = input.parse()?;
                let lookahead = input.lookahead1();
                let mut expr = if lookahead.peek(Token![while]) {
                    Expr::While(input.parse()?)
                } else if lookahead.peek(Token![for]) {
                    Expr::ForLoop(input.parse()?)
                } else if lookahead.peek(Token![loop]) {
                    Expr::Loop(input.parse()?)
                } else {
                    return Err(lookahead.error());
                };
                match &mut expr {
                    Expr::While(ExprWhile { label, .. })
                    | Expr::ForLoop(ExprForLoop { label, .. })
                    | Expr::Loop(ExprLoop { label, .. }) => *label = Some(the_label),
                    _ => unreachable!(),
                }
                Ok(match expr {
                    Expr::While(w) => Self::Loop(LoopKind::While(w)),
                    Expr::ForLoop(f) => Self::Loop(LoopKind::ForLoop(f)),
                    Expr::Loop(l) => Self::Loop(LoopKind::Loop(l)),
                    _ => unreachable!(),
                })
            } else {
                Err(lookahead.error())
            }
        }
    }
}
