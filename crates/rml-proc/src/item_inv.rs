use proc_macro2::{Ident, Span, TokenStream as TS2};
use quote::quote;
use rml_syn::{Encode, Term};
use syn::{
    parse_quote_spanned, spanned::Spanned, Attribute, Generics, ItemFn, ItemImpl, ItemTrait, Meta,
    Result, TraitItemFn,
};

use crate::{
    subject::ItemKind,
    util::{extract_attrs, generate_unique_ident},
};

fn item_inv_fn(term: Term, ident: &Ident, inv_kind: &Ident) -> ItemFn {
    let tsp = term.span();
    let e = term.encode();
    let name = ident.to_string();
    parse_quote_spanned! { tsp =>
        #[allow(unused_variables, dead_code)]
        #[rml::spec::#inv_kind = #name]
        fn #ident(self) -> bool {
            let b: bool = !!(#e);
            b
        }
    }
}

fn item_inv_impl(span: Span, ident: Ident, generics: &Generics, fns: Vec<ItemFn>) -> ItemImpl {
    let mut i: ItemImpl = parse_quote_spanned! { span =>
        impl #ident {
            #(#fns)*
        }
    };

    i.generics = generics.clone();
    i
}

fn struct_or_enum(inv_kind: &'static str, term: Term, mut item: ItemKind) -> Result<TS2> {
    let tsp = term.span();
    let ref_ident = Ident::new(&format!("{inv_kind}_ref"), tsp);
    let first_ident = generate_unique_ident(inv_kind);
    let first_str = first_ident.to_string();
    let inv_kind_ident = Ident::new(inv_kind, tsp);

    let mut attrs: Vec<Attribute> =
        vec![parse_quote_spanned! {tsp => #[rml::#ref_ident = #first_str]}];
    let mut fns: Vec<ItemFn> = vec![item_inv_fn(term, &first_ident, &inv_kind_ident)];

    let (mut old_attrs, ident, gens) = match &mut item {
        ItemKind::Trait(_) => unreachable!(),
        ItemKind::Struct(i) => (&mut i.attrs, i.ident.clone(), i.generics.clone()),
        ItemKind::Enum(i) => (&mut i.attrs, i.ident.clone(), i.generics.clone()),
    };

    let inv_attrs = extract_attrs(&mut old_attrs, "invariant");

    for inv in inv_attrs {
        let sp = inv.span();
        if let Meta::List(l) = inv.meta {
            let t: Term = syn::parse2(l.tokens)?;
            let name = generate_unique_ident(inv_kind);
            fns.push(item_inv_fn(t, &name, &inv_kind_ident));
            attrs.push(parse_quote_spanned! {sp => #[rml::#ref_ident = #name]});
        } else {
            panic!()
        }
    }

    // TODO: more concrete span?
    let r#impl = item_inv_impl(Span::call_site(), ident, &gens, fns);

    Ok(quote! {
        #(#attrs)*
        #item

        #r#impl
    })
}

fn trait_inv(term: Term, mut i: ItemTrait) -> Result<TS2> {
    let tsp = term.span();
    let first_ident = generate_unique_ident("trait_inv");
    let first_str = first_ident.to_string();
    let inv_kind_ident = Ident::new("trait_inv", tsp);

    let mut attrs: Vec<Attribute> =
        vec![parse_quote_spanned! {tsp => #[rml::trait_inv_ref = #first_str]}];
    let mut fns: Vec<ItemFn> = vec![item_inv_fn(term, &first_ident, &inv_kind_ident)];

    let inv_attrs = extract_attrs(&mut i.attrs, "invariant");

    for inv in inv_attrs {
        let sp = inv.span();
        if let Meta::List(l) = inv.meta {
            let t: Term = syn::parse2(l.tokens)?;
            let name = generate_unique_ident("trait_inv");
            fns.push(item_inv_fn(t, &name, &inv_kind_ident));
            attrs.push(parse_quote_spanned! {sp => #[rml::trait_inv_ref = #name]});
        } else {
            panic!()
        }
    }

    // We simply add the invariants as spec functions nto the trait declaration.
    // Technically, this permits developers to override the contents of the invariants,
    // leading to breaking of behavioral subtyping.
    // But in practice, this is impossible: On every compilation the names of the
    // spec functions is randomly generated anew with a new UUID. If a developer manages to
    // name their functions in accordance to this random generation---and does so for any
    // new compilation---they deserve to break whatever they want!
    i.items.extend(fns.into_iter().map(
        |ItemFn {
             attrs, sig, block, ..
         }| {
            syn::TraitItem::Fn(TraitItemFn {
                attrs,
                sig,
                default: Some(*block),
                semi_token: None,
            })
        },
    ));

    Ok(quote! {
        #(#attrs)*
        #i
    })
}

pub(crate) fn item_inv(term: Term, item: ItemKind) -> Result<TS2> {
    match item {
        ItemKind::Trait(i) => trait_inv(term, i),
        ItemKind::Struct(i) => struct_or_enum("struct_inv", term, ItemKind::Struct(i)),
        ItemKind::Enum(i) => struct_or_enum("enum_inv", term, ItemKind::Enum(i)),
    }
}
