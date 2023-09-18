//! Procedural (attribute) macros that do not change the annoated code at all.
//! Does, however, syntax check all attributes. Type-checking not included.
//!
//! **Do not directly depend on this!** Use `rml-contracts` instead.

extern crate proc_macro;
use proc_macro::TokenStream as TS1;
use proc_macro2::Span;

use rml_syn::{
    extern_spec::ExternSpecItem, locset::LocSet, subject::LogicSubject, SpecContent, TBlock, Term,
};
use syn::{parse_macro_input, Path};

#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as SpecContent);
    item
}

#[proc_macro_attribute]
pub fn strictly_pure(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("strictly_pure");
    }
    item
}

#[proc_macro_attribute]
pub fn pure(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("pure");
    }
    item
}

#[proc_macro_attribute]
pub fn invariant(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as Term);
    item
}

#[proc_macro_attribute]
pub fn variant(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as Term);
    item
}

#[proc_macro_attribute]
pub fn modifies(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as LocSet);
    item
}

#[proc_macro_attribute]
pub fn logic(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("logic");
    }
    // Make sure that the attribute is always attached to the correct "thing",
    // even though we don't output it
    let _ = parse_macro_input!(item as LogicSubject);
    TS1::new()
}

#[proc_macro_attribute]
pub fn trusted(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("trusted");
    }
    item
}

#[proc_macro]
pub fn rml(tokens: TS1) -> TS1 {
    let _ = parse_macro_input!(tokens with TBlock::parse_within);
    TS1::new()
}

#[proc_macro]
pub fn proof_assert(assertion: TS1) -> TS1 {
    let _ = parse_macro_input!(assertion with TBlock::parse_within);
    TS1::new()
}

#[proc_macro_attribute]
pub fn extern_spec(attr: TS1, item: TS1) -> TS1 {
    let _ = if attr.is_empty() {
        None
    } else {
        let p = parse_macro_input!(attr as Path);
        Some(p)
    };

    let _ = parse_macro_input!(item as ExternSpecItem);

    TS1::new()
}

fn takes_no_args(name: &str) -> TS1 {
    TS1::from(
        syn::Error::new(Span::call_site(), format!("`{name}` takes no arguments"))
            .to_compile_error(),
    )
}
