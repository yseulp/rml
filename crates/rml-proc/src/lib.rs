use proc_macro::TokenStream as TS1;
use proc_macro2::{Span, TokenStream as TS2};
use quote::quote;
use rml_syn::{Spec, Term};

use syn::{parse_macro_input, parse_quote, FnArg, Ident, Item, ReturnType, Stmt};

mod subject;

use subject::{ContractSubject, InvariantSubject};

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
        ContractSubject::FnOrMethod(mut fn_or_meth) => {
            let result = match fn_or_meth.sig.output {
                ReturnType::Default => parse_quote! { result : () },
                ReturnType::Type(_, ref ty) => parse_quote! { result : #ty },
            };
            let spec_tokens = fn_spec_item(spec_name, result, sp, Span::call_site());
            if let Some(b) = fn_or_meth.body.as_mut() {
                b.stmts.insert(0, Stmt::Item(Item::Verbatim(spec_tokens)))
            }
            TS1::from(quote! {
                #[rml::specification::spec=#name_tag]
                #fn_or_meth
            })
        }
        ContractSubject::Closure(c) => TS1::from(quote! {
            #c
        }),
    }
}

#[proc_macro_attribute]
pub fn pure(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty());
    let toks = TS2::from(item);
    TS1::from(quote! {
        #[rml::decl::pure]
        #toks
    })
}

#[proc_macro_attribute]
pub fn invariant(attr: TS1, item: TS1) -> TS1 {
    let term = parse_macro_input!(attr as Term);
    let subject = parse_macro_input!(item as InvariantSubject);
    let ts = match subject {
        InvariantSubject::ForLoop(l) => {
            quote! {#l}
        }
        InvariantSubject::Loop(l) => {
            let inv = term.encode();
            quote! {
                #inv
                #l
            }
        }
        InvariantSubject::While(l) => quote! {
            #l
        },
        InvariantSubject::Trait(i) => quote! {#i},
        InvariantSubject::Struct(i) => quote! {#i},
        InvariantSubject::Enum(i) => quote! {#i},
    };

    TS1::from(ts)
}

#[proc_macro_attribute]
pub fn variant(attr: TS1, item: TS1) -> TS1 {
    item
}

#[proc_macro_attribute]
pub fn modifies(attr: TS1, item: TS1) -> TS1 {
    item
}

fn generate_unique_ident(prefix: &str) -> Ident {
    let uuid = uuid::Uuid::new_v4();
    let ident = format!("{}_{}", prefix, uuid).replace('-', "_");

    Ident::new(&ident, Span::call_site())
}

fn fn_spec_item(spec_name: Ident, result: FnArg, spec: Spec, span: Span) -> TS2 {
    let spec_term = spec.encode(result, span);
    quote! {
        #[allow(unused_must_use)]
        let _ = #[rml::item=#spec_name] #spec_term
    }
}
