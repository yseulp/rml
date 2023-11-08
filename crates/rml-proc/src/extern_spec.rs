use proc_macro2::{Ident, Span, TokenStream as TS2};
use quote::{quote, quote_spanned};
use rml_syn::{
    attrs::{AttributeInvariant, AttributeSpec},
    extern_spec::{
        ExternSpecItem, FlattenedEnumSpec, FlattenedExternSpec, FlattenedFnSpec,
        FlattenedStructSpec, FlattenedTraitSpec, FnContext,
    },
};

use syn::{parse_quote, spanned::Spanned, Attribute, ExprPath, Generics, Path, Result, ReturnType};

use crate::{func::fn_spec_item, util::generate_unique_ident};

enum InvSubject {
    Struct,
    Enum,
    Trait,
}

pub(crate) fn extern_spec(subject: ExternSpecItem, path: Option<Path>) -> Result<TS2> {
    let subject_span = subject.span();
    let specs = subject.flatten(path)?;

    specs
        .into_iter()
        .map(|spec| match spec {
            FlattenedExternSpec::Struct(FlattenedStructSpec {
                span,
                attrs,
                prefix,
                ident,
                generics,
            }) => handle_inv(
                subject_span,
                span,
                attrs,
                prefix,
                ident,
                generics,
                InvSubject::Struct,
            ),
            FlattenedExternSpec::Enum(FlattenedEnumSpec {
                span,
                attrs,
                prefix,
                ident,
                generics,
            }) => handle_inv(
                subject_span,
                span,
                attrs,
                prefix,
                ident,
                generics,
                InvSubject::Enum,
            ),
            FlattenedExternSpec::Trait(FlattenedTraitSpec {
                span,
                attrs,
                prefix,
                ident,
                generics,
            }) => handle_inv(
                subject_span,
                span,
                attrs,
                prefix,
                ident,
                generics,
                InvSubject::Trait,
            ),
            FlattenedExternSpec::Fn(f) => handle_fn(subject_span, f),
        })
        .try_collect()
}

fn handle_fn(
    subject_span: Span,
    FlattenedFnSpec {
        span,
        attrs,
        prefix,
        sig,
        ctxt,
    }: FlattenedFnSpec,
) -> Result<TS2> {
    let mut is_pure = false;
    let mut is_strictly_pure = false;
    let mut fn_spec_items = vec![];
    let mut spec_case_refs = vec![];

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
                let spec_id = generate_unique_ident(&sig.ident.to_string());
                let result = match &sig.output {
                    ReturnType::Default => parse_quote! { result : () },
                    ReturnType::Type(_, ref ty) => parse_quote! { result : #ty },
                };
                let spec = spec.validate()?;
                spec_case_refs.push(spec_id.to_string());
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
            Some(quote!(#[rml::extern_spec_trait=#ident #generics]))
        }
        FnContext::Impl(target_ty) => Some(quote!(#[rml::extern_spec_impl=#target_ty])),
        FnContext::TraitImpl { target_ty, trait_ } => Some(quote! {
            #[rml::extern_spec_impl=#target_ty]
            #[rml::extern_spec_trait_impl=#trait_]
        }),
    };

    let spec_case_attrs = spec_case_refs
        .into_iter()
        .map(|name| quote!(rml::extern_spec_case_ref=#name))
        .collect::<Vec<_>>();

    let const_name = generate_unique_ident(&format!("extern_spec_{}", sig.ident));

    Ok(quote_spanned! {
        subject_span =>
        #purity_attr
        #(#spec_case_attrs)*
        #ctxt
        #[extern_spec_path=#prefix]
        const #const_name: bool = false;
    })
}

fn handle_inv(
    subject_span: Span,
    span: Span,
    attrs: Vec<AttributeInvariant>,
    prefix: ExprPath,
    ident: Ident,
    generics: Generics,
    subject: InvSubject,
) -> Result<TS2> {
    Ok(quote_spanned! {
        subject_span =>

    })
}
