use core::fmt;

use proc_macro2::{Ident, Span, TokenStream as TS2};
use quote::{ToTokens, quote, quote_spanned};
use rml_syn::{
    attrs::{AttributeInvariant, AttributeSpec},
    extern_spec::{
        ExternSpecItem, FlattenedEnumSpec, FlattenedExternSpec, FlattenedFnSpec,
        FlattenedStructSpec, FlattenedTraitSpec, FnContext,
    },
};
use syn::{
    Attribute, ExprPath, FnArg, Generics, PatType, Path, Result, ReturnType, Token, parse_quote,
    spanned::Spanned,
};

use crate::{
    func::fn_spec_item,
    util::{gen_bool_spec_fn, gen_self_params, gen_unique_ident, replace_self},
};

enum InvSubject {
    Struct,
    Enum,
    Trait,
}

impl fmt::Display for InvSubject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            InvSubject::Struct => "struct",
            InvSubject::Enum => "enum",
            InvSubject::Trait => "trait",
        };
        fmt::Display::fmt(s, f)
    }
}

/// Generate Rust code that enocdes the specification of an [ExternSpecItem] as
/// a constant with attributes and spec functions.
///
/// - `subject`: The subject to generate code for.
/// - `path`: An optional path that describes the path to the item.
pub(crate) fn extern_spec(subject: ExternSpecItem, path: Option<Path>) -> Result<TS2> {
    let subject_span = subject.span();
    let specs = subject.flatten(path)?;

    specs
        .into_iter()
        .map(|spec| match spec {
            FlattenedExternSpec::Struct(FlattenedStructSpec {
                attrs,
                prefix,
                ident,
                generics,
                ..
            }) => handle_inv(
                subject_span,
                attrs,
                prefix,
                ident,
                generics,
                InvSubject::Struct,
            ),
            FlattenedExternSpec::Enum(FlattenedEnumSpec {
                attrs,
                prefix,
                ident,
                generics,
                ..
            }) => handle_inv(
                subject_span,
                attrs,
                prefix,
                ident,
                generics,
                InvSubject::Enum,
            ),
            FlattenedExternSpec::Trait(FlattenedTraitSpec {
                attrs,
                prefix,
                ident,
                generics,
                ..
            }) => handle_inv(
                subject_span,
                attrs,
                prefix,
                ident,
                generics,
                InvSubject::Trait,
            ),
            FlattenedExternSpec::Fn(f) => handle_fn(subject_span, *f),
        })
        .try_collect()
}

/// Generate code for an extern function (after flattening).
fn handle_fn(
    subject_span: Span,
    FlattenedFnSpec {
        span,
        attrs,
        prefix,
        mut sig,
        ctxt,
    }: FlattenedFnSpec,
) -> Result<TS2> {
    let mut is_pure = false;
    let mut is_strictly_pure = false;
    let mut fn_spec_items = vec![];
    let mut spec_case_refs = vec![];

    for p in &mut sig.inputs {
        if let FnArg::Receiver(s) = p.clone() {
            let span = s.span();
            *p = FnArg::Typed(PatType {
                attrs: s.attrs,
                pat: Box::new(parse_quote!(rml_self)),
                colon_token: Token![:](span),
                ty: ctxt.self_ty().unwrap().into(),
            })
        }
    }

    for attr in attrs {
        let span = attr.span();
        match attr {
            rml_syn::attrs::FnAttribute::Pure(a) => {
                if is_pure {
                    return Err(syn::Error::new_spanned(a, "Duplicate `pure` attribute"));
                }
                is_pure = true;
            }
            rml_syn::attrs::FnAttribute::StrictlyPure(a) => {
                if is_strictly_pure {
                    return Err(syn::Error::new_spanned(
                        a,
                        "Duplicate `strictly_pure` attribute",
                    ));
                }
                is_strictly_pure = true;
            }
            rml_syn::attrs::FnAttribute::Spec(AttributeSpec { spec, .. }) => {
                let spec_id = gen_unique_ident(&sig.ident.to_string());
                let result = match &sig.output {
                    ReturnType::Default => parse_quote! { result : () },
                    ReturnType::Type(_, ty) => parse_quote! { result : #ty },
                };
                let mut spec = spec.validate()?;
                spec_case_refs.push(spec_id.to_string());
                replace_self(&mut spec, &parse_quote!(rml_self));
                fn_spec_items.push(fn_spec_item(spec_id, sig.clone(), result, spec, span));
            }
        }
    }

    if is_pure && is_strictly_pure {
        return Err(syn::Error::new(
            span,
            "Function cannot be declared `pure` and `strictly_pure`",
        ));
    }

    let purity_attr: Option<Attribute> = if is_strictly_pure {
        Some(parse_quote!(#[rml::decl::strictly_pure]))
    } else if is_pure {
        Some(parse_quote!(#[rml::decl::pure]))
    } else {
        None
    };

    let ctxt = match ctxt {
        FnContext::None => None,
        FnContext::Trait(ident, generics) => {
            let s = quote!(#ident #generics).to_string();
            Some(quote!(#[rml::extern_spec_trait=#s]))
        }
        FnContext::Impl(target_ty) => {
            let ty_str = target_ty.to_token_stream().to_string();
            Some(quote!(#[rml::extern_spec_impl=#ty_str]))
        }
        FnContext::TraitImpl { target_ty, trait_ } => {
            let ty_str = target_ty.to_token_stream().to_string();
            let trait_str = trait_.to_token_stream().to_string();
            Some(quote! {

                #[rml::extern_spec_impl=#ty_str]
                #[rml::extern_spec_trait_impl=#trait_str]
            })
        }
    };

    let spec_case_attrs = spec_case_refs
        .into_iter()
        .map(|name| quote!(#[rml::extern_spec_case_ref=#name]))
        .collect::<Vec<_>>();

    let const_name = gen_unique_ident(&format!("extern_spec_{}", sig.ident));

    let f_ident = sig.ident;

    let path = quote!(#prefix::#f_ident).to_string();

    Ok(quote_spanned! {
        subject_span =>
        #(#fn_spec_items)*

        #purity_attr
        #(#spec_case_attrs)*
        #ctxt
        #[rml::extern_spec_path=#path]
        const #const_name: bool = false;
    })
}

/// Generate code for an extern data structure or trait (after flattening).
fn handle_inv(
    subject_span: Span,
    attrs: Vec<AttributeInvariant>,
    prefix: ExprPath,
    ident: Ident,
    generics: Generics,
    subject: InvSubject,
) -> Result<TS2> {
    let mut inv_fns = vec![];
    let mut inv_idents = vec![];
    let ident_prefix = format!("inv_{subject}_{ident}");

    for attr in attrs {
        let inv = attr.term;
        let ident = gen_unique_ident(&ident_prefix);
        let inputs = gen_self_params();
        let f = gen_bool_spec_fn(
            &ident,
            subject_span,
            inv,
            &parse_quote!(rml::extern_spec_inv),
            inputs.pairs(),
            &generics.clone(),
        );
        inv_fns.push(f);
        inv_idents.push(ident);
    }

    let inv_attrs = inv_idents
        .into_iter()
        .map(|name| quote!(#[rml::extern_spec_inv_ref=#name]))
        .collect::<Vec<_>>();

    let const_name = gen_unique_ident(&format!("extern_spec_inv_{subject}_{ident}"));

    let path = quote!(#prefix::#ident).to_string();

    Ok(quote_spanned! {
        subject_span =>
        #(#inv_fns)*

        #(#inv_attrs)*
        #[rml::extern_spec_path=#path]
        const #const_name: bool = false;
    })
}
