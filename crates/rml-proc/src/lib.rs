#![feature(box_patterns)]
#![feature(iterator_try_collect)]

//! Procedural (attribute) macros to specify Rust code.
//!
//! **Do not directly depend on this!** Use `rml-contracts` instead.

use func::fn_spec_item;
use proc_macro::TokenStream as TS1;
use proc_macro2::{Span, TokenStream as TS2};
use quote::{quote, quote_spanned};
use rml_syn::{
    extern_spec::ExternSpecItem, subject::LogicSubject, Encode, SpecContent, TBlock, Term,
};
use syn::{parse_macro_input, parse_quote, spanned::Spanned, Path, ReturnType};

mod extern_spec;
mod func;
mod item_inv;
mod loop_inv;
mod subject;
mod util;

use subject::{ContractSubject, InvariantSubject};
use util::gen_unique_ident;

/// A specification case for a function. The attribute takes a [SpecContent]
/// and must be attached to a function or method, which need not have a body.
#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let sp = parse_macro_input!(attr as SpecContent);
    let sp = match sp.validate() {
        Ok(s) => s,
        Err(e) => return TS1::from(e.into_compile_error()),
    };

    let item = parse_macro_input!(item as ContractSubject);

    let spec_name = gen_unique_ident(&item.name());
    let name_tag = format!("{}", quote! { #spec_name });

    match item {
        ContractSubject::FnOrMethod(fn_or_meth) if fn_or_meth.is_trait_signature() => {
            TS1::from(quote! {
                #fn_or_meth
            })
        }
        ContractSubject::FnOrMethod(fn_or_meth) => {
            let result = match fn_or_meth.sig.output {
                ReturnType::Default => parse_quote! { result : () },
                ReturnType::Type(_, ref ty) => parse_quote! { result : #ty },
            };
            let spec_tokens = fn_spec_item(
                spec_name,
                fn_or_meth.sig.clone(),
                result,
                sp,
                Span::call_site(),
            );
            TS1::from(quote! {
                #spec_tokens
                #[rml::spec_case_ref=#name_tag]
                #fn_or_meth
            })
        }
        ContractSubject::Closure(c) => TS1::from(quote! {
            #c
        }),
    }
}

/// Declares a function as strictly pure, i.e., that it has *no* side effects.
///
/// The attribute takes no arguments and must be attched to a function or
/// method.
#[proc_macro_attribute]
pub fn strictly_pure(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("strictly_pure");
    }
    let toks = TS2::from(item);
    TS1::from(quote! {
        #[rml::decl::strictly_pure]
        #toks
    })
}

/// Declares a function as strictly pure, i.e., that it has *no* side effects on
/// *existing data*.
///
/// The attribute takes no arguments and must be attched to a function or
/// method.
#[proc_macro_attribute]
pub fn pure(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("pure");
    }
    let toks = TS2::from(item);
    TS1::from(quote! {
        #[rml::decl::pure]
        #toks
    })
}

/// Declare an invariant for a data structure or loop.
/// Takes a boolean [Term]. The target must be a loop, enum, struct, or trait.
#[proc_macro_attribute]
pub fn invariant(attr: TS1, item: TS1) -> TS1 {
    let term = parse_macro_input!(attr as Term);
    let subject = parse_macro_input!(item as InvariantSubject);
    let sp = subject.span();
    let ts = match subject {
        InvariantSubject::Loop(l) => {
            let (attrs, stmts, l) = match loop_inv::loop_inv(term, l) {
                Ok(r) => r,
                Err(err) => return TS1::from(err.to_compile_error()),
            };
            quote_spanned! { sp=>
                {
                    #stmts
                    #attrs
                    #l
                }
            }
        }
        InvariantSubject::Item(i) => match item_inv::item_inv(term, i) {
            Ok(ts) => ts,
            Err(e) => return TS1::from(e.to_compile_error()),
        },
    };

    TS1::from(ts)
}

/// Declare a variant for a loop. Takes a [Term], which must have a strict order
/// defined.
#[proc_macro_attribute]
pub fn variant(_: TS1, _: TS1) -> TS1 {
    TS1::from(
        syn::Error::new(
            Span::call_site(),
            "The `variant` attribute must be after an `invariant` attribute",
        )
        .to_compile_error(),
    )
}

/// Declare which fields, parameters, etc. may be modified by the function or
/// loop. Takes a [LocSet].
///
/// [LocSet]: rml_syn::LocSet
#[proc_macro_attribute]
pub fn modifies(_: TS1, _: TS1) -> TS1 {
    TS1::from(
        syn::Error::new(
            Span::call_site(),
            "The `modifies` attribute must be after an `invariant` attribute",
        )
        .to_compile_error(),
    )
}

/// Declares a function as a logic function, which can only be called from
/// within specifications.
#[proc_macro_attribute]
pub fn logic(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("logic");
    }
    let subject = parse_macro_input!(item as LogicSubject);
    match subject {
        LogicSubject::WithBody(f) => {
            let sp = f.span();
            TS1::from(quote_spanned! { sp =>
                #[rml::decl::logic]
                #f
            })
        }
        LogicSubject::WithoutBody(t) => {
            let sp = t.span();
            TS1::from(quote_spanned! { sp =>
                #[rml::decl::logic]
                #t
            })
        }
    }
}

/// Declares a function as trusted, which means it need not be verified.
#[proc_macro_attribute]
pub fn trusted(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("trusted");
    }
    let toks = TS2::from(item);
    TS1::from(quote! {
        #[rml::decl::trusted]
        #toks
    })
}

/// Parse a series of RML statements.
#[proc_macro]
pub fn rml(tokens: TS1) -> TS1 {
    let block = parse_macro_input!(tokens with TBlock::parse_within);
    TS1::from(
        block
            .into_iter()
            .map(|ts| {
                let sp = ts.span();
                let stmt = ts.encode();
                quote_spanned! { sp => #stmt }
            })
            .collect::<TS2>(),
    )
}

#[proc_macro]
pub fn proof_assert(assertion: TS1) -> TS1 {
    let assertion = parse_macro_input!(assertion with TBlock::parse_within);
    let body = assertion
        .into_iter()
        .map(|ts| {
            let sp = ts.span();
            let stmt = ts.encode();
            quote_spanned! { sp => #stmt }
        })
        .collect::<TS2>();
    TS1::from(quote! {
        {
            #[allow(unused_must_use, unused_variables)]
            let _ = {
                #[rml::spec::assert]
                || {
                    let b: bool = {
                        #body
                    }
                    b
                }
            }
        }
    })
}

/// Specifies external data structures or functions. Takes an optional [Path] to
/// the items.
///
/// The target must be an [ExternSpecItem].
///
/// External specification refers to specification for items _external_ to the
/// current crate. Since, the source code is not available, specification must
/// be made available in a different way.
///
/// Specifying can be done by using the `extern_spec` attribute before other
/// RML attributes.
///
/// ## Functions
/// Assume `div` is a function defined in some external crate.
/// ```
/// #[extern_spec]
/// #[spec {
///     requires(b > 0),
///     ensures(result a / b)
/// }]
/// fn div(a: u32, b: u32) -> u32;
/// ```
/// The above specification will be used by tools using RML as though it was
/// attached to a local function.
#[proc_macro_attribute]
pub fn extern_spec(attr: TS1, item: TS1) -> TS1 {
    let path = if attr.is_empty() {
        None
    } else {
        let p = parse_macro_input!(attr as Path);
        Some(p)
    };

    let subject = parse_macro_input!(item as ExternSpecItem);
    let ts = match extern_spec::extern_spec(subject, path) {
        Ok(ts) => ts,
        Err(e) => return TS1::from(e.to_compile_error()),
    };

    TS1::from(ts)
}

/// Generate a compile error for an attribute that takes no arguments but
/// was given some.
fn takes_no_args(name: &str) -> TS1 {
    TS1::from(
        syn::Error::new(Span::call_site(), format!("`{name}` takes no arguments"))
            .to_compile_error(),
    )
}
