use proc_macro2::{Span, TokenStream as TS2};
use quote::quote_spanned;
use rml_syn::{LocSet, LocSetGroup, Spec};
use syn::{
    parse_quote, parse_quote_spanned, punctuated::Punctuated, spanned::Spanned, Attribute, FnArg,
    Ident, Signature, Token,
};

use crate::util::{
    gen_bool_spec_fn, gen_locset_spec_fn, gen_unique_ident, gen_wf_spec_fn, get_mut_ref_params,
};

pub(crate) fn fn_spec_item(
    spec_id: Ident,
    sig: Signature,
    result: FnArg,
    mut spec: Spec,
    span: Span,
) -> TS2 {
    let is_normal = spec.is_normal();
    let spec_attr = if is_normal {
        "spec_normal"
    } else {
        "spec_panic"
    };
    let spec_attr = Ident::new(spec_attr, Span::call_site());
    let mut post_sig = sig.clone();
    post_sig.inputs.push(result);

    // pre
    if spec.pre_conds.is_empty() {
        spec.pre_conds.push(parse_quote!(true));
    }
    let pre_idents: Vec<_> = (0..spec.pre_conds.len())
        .map(|_| gen_unique_ident("spec_part_pre"))
        .collect();
    let pre: Vec<_> = spec
        .pre_conds
        .into_iter()
        .enumerate()
        .map(|(i, p)| {
            let id = &pre_idents[i];
            let span = p.span();
            gen_bool_spec_fn(
                id,
                span,
                p,
                &parse_quote!(rml::spec::pre),
                sig.inputs.pairs(),
                &sig.generics,
            )
        })
        .collect();
    let pre_strs = pre_idents.iter().map(|i| i.to_string());

    // post
    if spec.post_conds.is_empty() {
        spec.post_conds.push(parse_quote!(true))
    }
    let post_idents: Vec<_> = (0..spec.post_conds.len())
        .map(|_| gen_unique_ident("spec_part_post"))
        .collect();
    let post = spec.post_conds.into_iter().enumerate().map(|(i, p)| {
        let id = &post_idents[i];
        let span = p.span();

        gen_bool_spec_fn(
            id,
            span,
            p,
            &parse_quote!(rml::spec::post),
            post_sig.inputs.pairs(),
            &post_sig.generics,
        )
    });
    let post_strs = post_idents.iter().map(|i| i.to_string());

    // modifies
    let locset = if let Some(ls) = spec.modifies {
        ls
    } else {
        let mut_params = get_mut_ref_params(&sig);
        let mut locsets: Vec<LocSet> = mut_params
            .map(|a| {
                let asp = a.span();
                match a {
                    FnArg::Receiver(_) => {
                        parse_quote_spanned! { asp => self.* }
                    }
                    FnArg::Typed(t) => {
                        let base = &t.pat;
                        parse_quote_spanned! { asp => #base.* }
                    }
                }
            })
            .collect();
        if locsets.is_empty() {
            LocSet::Nothing(parse_quote_spanned! { span => nothing })
        } else {
            let first = locsets.remove(0);
            let mut p = Punctuated::new();
            p.push_value(first);
            p.push_punct(Token![,](span));

            for ls in locsets {
                p.push(ls);
            }

            LocSet::Group(LocSetGroup { items: p })
        }
    };
    let modi_id = gen_unique_ident("spec_part_modi");
    let modi_id_str = modi_id.to_string();
    let modi_attr: Attribute =
        parse_quote_spanned! { span => #[rml::spec_part_modi_ref=#modi_id_str] };
    let modi = gen_locset_spec_fn(
        &modi_id,
        span,
        locset,
        &parse_quote!(rml::spec::modi),
        sig.inputs.pairs(),
        &sig.generics,
    );

    // variant
    let (var_attr, var) = if let Some(v) = spec.variant {
        let span = v.span();
        let id = gen_unique_ident("spec_part_var");
        let id_str = id.to_string();
        let item = gen_wf_spec_fn(
            &id,
            span,
            v,
            &parse_quote!(rml::spec::var),
            sig.inputs.pairs(),
            &sig.generics,
        );
        let var_attr: Attribute =
            parse_quote_spanned! { span => #[rml::spec_part_var_ref=#id_str] };
        (Some(var_attr), Some(item))
    } else {
        (None, None)
    };

    // diverges
    let diverges = spec
        .diverges
        .map(|d| d.unwrap_or_else(|| parse_quote_spanned! { span => true }))
        .unwrap_or_else(|| parse_quote_spanned! { span => false });
    let (div, div_attr) = {
        let id = gen_unique_ident("spec_part_div");
        let id_str = id.to_string();
        let span = diverges.span();

        let item = gen_bool_spec_fn(
            &id,
            span,
            diverges,
            &parse_quote!(rml::spec::div),
            sig.inputs.pairs(),
            &sig.generics,
        );
        let attr: Attribute = parse_quote_spanned! { span => #[rml::spec_part_div_ref=#id_str] };
        (item, attr)
    };

    // name
    let spec_name: Option<Attribute> = spec
        .name
        .map(|n| parse_quote_spanned! { span => #[rml::spec_name=#n] });

    let spec_name_str = spec_id.to_string();
    quote_spanned! { span =>
        #(#pre)*
        #(#post)*
        #div
        #modi
        #var
        #[allow(unused_must_use, unused_variables, dead_code)]
        #[rml::#spec_attr=#spec_name_str]
        #(#[rml::spec_part_pre_ref=#pre_strs])*
        #(#[rml::spec_part_post_ref=#post_strs])*
        #var_attr
        #modi_attr
        #div_attr
        #spec_name
        const #spec_id: bool = false;
    }
}
