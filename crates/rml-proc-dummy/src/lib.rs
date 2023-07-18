extern crate proc_macro;
use proc_macro::TokenStream as TS1;

use rml_syn::{subject::LogicSubject, Spec, Term};
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let _ = parse_macro_input!(attr as Spec);
    item
}

#[proc_macro_attribute]
pub fn strictly_pure(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`strictly_pure` takes no arguments");
    item
}

#[proc_macro_attribute]
pub fn pure(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`pure` takes no arguments");
    item
}

#[proc_macro_attribute]
pub fn invariant(attr: TS1, item: TS1) -> TS1 {
    let _: Term = parse_macro_input!(attr as Term);
    item
}

#[proc_macro_attribute]
pub fn variant(attr: TS1, item: TS1) -> TS1 {
    let _: Term = parse_macro_input!(attr as Term);
    item
}

#[proc_macro_attribute]
pub fn modifies(_attr: TS1, item: TS1) -> TS1 {
    item
}

#[proc_macro_attribute]
pub fn logic(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`logic` takes no arguments");
    // Make sure that the attribute is always attached to the correct "thing",
    // even though we don't output it
    let _ = parse_macro_input!(item as LogicSubject);
    TS1::new()
}

#[proc_macro_attribute]
pub fn trusted(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty(), "`trusted` takes no arguments");
    item
}

#[proc_macro]
pub fn rml(_: TS1) -> TS1 {
    TS1::new()
}

#[proc_macro]
pub fn proof_assert(_: TS1) -> TS1 {
    TS1::new()
}
