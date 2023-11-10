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

/// A specification case for a function. The attribute takes a [SpecContent]
/// and must be attached to a function or method, which need not have a body.
#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as SpecContent);
    item
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
    item
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
    item
}

/// Declare an invariant for a data structure or loop.
/// Takes a boolean [Term].
#[proc_macro_attribute]
pub fn invariant(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as Term);
    item
}

/// Declare a variant for a loop. Takes a [Term], which must have a strict order
/// defined.
#[proc_macro_attribute]
pub fn variant(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as Term);
    item
}

/// Declare which fields, parameters, etc. may be modified by the function or
/// loop. Takes a [LocSet].
#[proc_macro_attribute]
pub fn modifies(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as LocSet);
    item
}

/// Declares a function as a logic function, which can only be called from
/// within specifications.
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

/// Declares a function as trusted, which means it need not be verified.
#[proc_macro_attribute]
pub fn trusted(attr: TS1, item: TS1) -> TS1 {
    if !attr.is_empty() {
        return takes_no_args("trusted");
    }
    item
}

/// Parse a series of RML statements.
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

/// Specifies external data structures or functions. Takes an optional [Path] to
/// the items.
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

/// Generate a compile error for an attribute that takes no arguments but
/// was given some.
fn takes_no_args(name: &str) -> TS1 {
    TS1::from(
        syn::Error::new(Span::call_site(), format!("`{name}` takes no arguments"))
            .to_compile_error(),
    )
}
