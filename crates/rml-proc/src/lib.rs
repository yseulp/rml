use proc_macro::TokenStream as TS1;
use proc_macro2::{Span, TokenStream as TS2};
use quote::{quote, ToTokens, TokenStreamExt};
use rml_syn::Spec;
use std::iter;
use syn::{
    braced,
    parse::{self, Parse},
    parse_macro_input, token, AttrStyle, Attribute, Block, ExprClosure, Ident, Result, Signature,
    Token, Visibility,
};

#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let sp = parse_macro_input!(attr as Spec);
    println!("{:#?}", sp);

    let item = parse_macro_input!(item as ContractSubject);

    let spec_name = generate_unique_ident(&item.name());
    let name_tag = format!("{}", quote! { #spec_name });

    match item {
        ContractSubject::FnOrMethod(fn_or_meth) if fn_or_meth.is_trait_signature() => {
            TS1::from(quote! {
                #fn_or_meth
            })
        }
        ContractSubject::FnOrMethod(fn_or_meth) => TS1::from(quote! {
            #[rml::specification::spec=#name_tag]
            #fn_or_meth
        }),
        ContractSubject::Closure(c) => TS1::from(quote! {
            #c
        }),
    }
}

#[proc_macro_attribute]
pub fn requires(_attr: TS1, _item: TS1) -> TS1 {
    todo!()
}

fn generate_unique_ident(prefix: &str) -> Ident {
    let uuid = uuid::Uuid::new_v4();
    let ident = format!("{}_{}", prefix, uuid).replace('-', "_");

    Ident::new(&ident, Span::call_site())
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

struct FnOrMethod {
    defaultness: Option<Token![default]>,
    visibility: Visibility,
    attrs: Vec<Attribute>,
    sig: Signature,
    body: Option<Block>,
    semi_token: Option<Token![;]>,
}

impl FnOrMethod {
    fn is_trait_signature(&self) -> bool {
        self.semi_token.is_some()
    }
}

enum ContractSubject {
    FnOrMethod(FnOrMethod),
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
    fn name(&self) -> String {
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

        return Ok(ContractSubject::FnOrMethod(FnOrMethod {
            defaultness,
            visibility: vis,
            attrs,
            sig,
            body: brace_token.map(|brace_token| Block { brace_token, stmts }),
            semi_token,
        }));
    }
}
