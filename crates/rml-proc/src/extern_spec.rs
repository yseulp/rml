use core::fmt;

use proc_macro2::{Ident, Span, TokenStream as TS2};
use quote::{quote, quote_spanned};
use rml_syn::{
    attrs::{AttributeInvariant, AttributeSpec},
    extern_spec::{
        ExternSpecItem, FlattenedEnumSpec, FlattenedExternSpec, FlattenedFnSpec,
        FlattenedStructSpec, FlattenedTraitSpec, FnContext,
    },
};

use syn::{
    parse_quote, punctuated::Punctuated, spanned::Spanned, token, Attribute, ExprPath, Generics,
    Path, Result, ReturnType, Signature, Token,
};

use crate::{
    func::fn_spec_item,
    util::{gen_bool_spec_fn, generate_unique_ident},
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
        .map(|name| quote!(#[rml::extern_spec_case_ref=#name]))
        .collect::<Vec<_>>();

    let const_name = generate_unique_ident(&format!("extern_spec_{}", sig.ident));

    let f_ident = sig.ident;

    Ok(quote_spanned! {
        subject_span =>
        #(#fn_spec_items)*

        #purity_attr
        #(#spec_case_attrs)*
        #ctxt
        #[rml::extern_spec_path=#prefix::#f_ident]
        const #const_name: bool = false;
    })
}

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
        let ident = generate_unique_ident(&ident_prefix);
        let mut inputs = Punctuated::new();
        inputs.push(parse_quote!(self));
        let sig = Signature {
            constness: None,
            asyncness: None,
            unsafety: None,
            abi: None,
            fn_token: Token![fn](subject_span),
            ident: ident.clone(),
            generics: generics.clone(),
            paren_token: token::Paren(subject_span),
            inputs,
            variadic: None,
            output: ReturnType::Default,
        };
        let f = gen_bool_spec_fn(
            ident.clone(),
            subject_span,
            inv,
            parse_quote!(rml::extern_spec_inv),
            &sig,
        );
        inv_fns.push(f);
        inv_idents.push(ident);
    }

    let inv_attrs = inv_idents
        .into_iter()
        .map(|name| quote!(#[rml::extern_spec_inv_ref=#name]))
        .collect::<Vec<_>>();

    let const_name = generate_unique_ident(&format!("extern_spec_inv_{subject}_{ident}"));

    Ok(quote_spanned! {
        subject_span =>
        #(#inv_fns)*

        #(#inv_attrs)*
        #[rml::extern_spec_path=#prefix::#ident]
        const #const_name: bool = false;
    })
}
