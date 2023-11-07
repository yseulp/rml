use proc_macro2::{Span, TokenStream as TS2};
use quote::{quote, quote_spanned};
use rml_syn::{
    attrs::AttributeSpec,
    extern_spec::{ExternSpecFn, ExternSpecItem},
};

use syn::{
    parse_quote, punctuated::Punctuated, spanned::Spanned, Attribute, Path, PathSegment, Result,
    ReturnType,
};

use crate::{func::fn_spec_item, util::generate_unique_ident};

pub(crate) fn extern_spec(subject: ExternSpecItem, path: Option<Path>) -> Result<TS2> {
    let subject_span = subject.span();
    let ts2 = match subject {
        ExternSpecItem::Enum(_) => todo!(),
        ExternSpecItem::Fn(ExternSpecFn { attrs, sig, .. }) => {
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
                    Span::call_site(),
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

            let spec_case_attrs = spec_case_refs
                .into_iter()
                .map(|name| quote!(rml::extern_spec_case_ref=#name))
                .collect::<Vec<_>>();

            let const_name = generate_unique_ident(&format!("extern_spec_{}", sig.ident));
            let mut path = path.unwrap_or(Path {
                leading_colon: None,
                segments: Punctuated::new(),
            });
            path.segments.push(PathSegment {
                ident: sig.ident,
                arguments: syn::PathArguments::None,
            });

            quote_spanned! {
                subject_span =>
                #[#purity_attr]
                #(#spec_case_attrs)*
                #[extern_spec_path=#path]
                const #const_name: bool = false;
            }
        }
        ExternSpecItem::Impl(_) => todo!(),
        ExternSpecItem::Mod(_) => todo!(),
        ExternSpecItem::Struct(_) => todo!(),
        ExternSpecItem::Trait(_) => todo!(),
    };

    Ok(ts2)
}
