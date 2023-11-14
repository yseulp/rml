use proc_macro2::{Ident, Span, TokenStream as TS2};
use quote::quote;
use rml_syn::Term;
use syn::{
    parse_quote, parse_quote_spanned, spanned::Spanned, Attribute, Generics, ItemFn, ItemImpl,
    ItemTrait, Meta, Path, Result, TraitItemFn,
};

use crate::{
    subject::ItemKind,
    util::{extract_attrs, gen_bool_spec_fn, gen_self_params, gen_unique_ident, EMPTY_GENERICS},
};

/// Generate an `impl` with the methods `fns`.
fn item_inv_impl(span: Span, ident: Ident, generics: &Generics, fns: Vec<ItemFn>) -> ItemImpl {
    let mut i: ItemImpl = parse_quote_spanned! { span =>
        impl #ident {
            #(#fns)*
        }
    };

    i.generics = generics.clone();
    i
}

/// Generates spec code for data structure invariants
///
/// Adds a new impl for with invariant spec methods.
fn struct_or_enum(inv_kind: &'static str, term: Term, mut item: ItemKind) -> Result<TS2> {
    let tsp = term.span();
    let ref_ident = Ident::new(&format!("{inv_kind}_ref"), tsp);
    let first_ident = gen_unique_ident(inv_kind);
    let first_str = first_ident.to_string();
    let inv_kind_ident = Ident::new(inv_kind, tsp);
    let attr_path: Path = parse_quote!(rml::spec::#inv_kind_ident);

    let mut attrs: Vec<Attribute> =
        vec![parse_quote_spanned! {tsp => #[rml::#ref_ident = #first_str]}];
    let mut fns: Vec<ItemFn> = vec![gen_bool_spec_fn(
        &first_ident,
        term.span(),
        term,
        &attr_path,
        gen_self_params().pairs(),
        &EMPTY_GENERICS,
    )];

    let (old_attrs, ident, gens) = match &mut item {
        ItemKind::Trait(_) => unreachable!(),
        ItemKind::Struct(i) => (&mut i.attrs, i.ident.clone(), i.generics.clone()),
        ItemKind::Enum(i) => (&mut i.attrs, i.ident.clone(), i.generics.clone()),
    };

    let inv_attrs = extract_attrs(old_attrs, "invariant");

    for inv in inv_attrs {
        let sp = inv.span();
        if let Meta::List(l) = inv.meta {
            let t: Term = syn::parse2(l.tokens)?;
            let name = gen_unique_ident(inv_kind);
            fns.push(gen_bool_spec_fn(
                &name,
                t.span(),
                t,
                &attr_path,
                gen_self_params().pairs(),
                &EMPTY_GENERICS,
            ));
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

/// Generate spec code for traits. Adds a method to the trait for each invariant
/// attribute.
fn trait_inv(term: Term, mut i: ItemTrait) -> Result<TS2> {
    let tsp = term.span();
    let first_ident = gen_unique_ident("trait_inv");
    let first_str = first_ident.to_string();
    let attr_path: Path = parse_quote!(rml::spec::trait_inv);

    let mut attrs: Vec<Attribute> =
        vec![parse_quote_spanned! {tsp => #[rml::trait_inv_ref = #first_str]}];
    let mut fns: Vec<ItemFn> = vec![gen_bool_spec_fn(
        &first_ident,
        term.span(),
        term,
        &attr_path,
        gen_self_params().pairs(),
        &EMPTY_GENERICS,
    )];

    let inv_attrs = extract_attrs(&mut i.attrs, "invariant");

    for inv in inv_attrs {
        let sp = inv.span();
        if let Meta::List(l) = inv.meta {
            let t: Term = syn::parse2(l.tokens)?;
            let name = gen_unique_ident("trait_inv");
            fns.push(gen_bool_spec_fn(
                &name,
                t.span(),
                t,
                &attr_path,
                gen_self_params().pairs(),
                &EMPTY_GENERICS,
            ));
            attrs.push(parse_quote_spanned! {sp => #[rml::trait_inv_ref = #name]});
        } else {
            panic!()
        }
    }

    // We simply add the invariants as spec functions to the trait declaration.
    // Technically, this permits developers to override the contents of the
    // invariants, leading to breaking of behavioral subtyping.
    // But in practice, this is impossible: On every compilation the names of the
    // spec functions is randomly generated anew with a new UUID. If a developer
    // manages to name their functions in accordance to this random
    // generation---and does so for any new compilation---they deserve to break
    // whatever they want!
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

/// Generate spec code for invariants.
pub(crate) fn item_inv(term: Term, item: ItemKind) -> Result<TS2> {
    match item {
        ItemKind::Trait(i) => trait_inv(term, i),
        ItemKind::Struct(i) => struct_or_enum("struct_inv", term, ItemKind::Struct(i)),
        ItemKind::Enum(i) => struct_or_enum("enum_inv", term, ItemKind::Enum(i)),
    }
}
