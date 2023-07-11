extern crate proc_macro;
use proc_macro::TokenStream as TS1;

use rml_syn::{Spec, Term};
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    let s = parse_macro_input!(attr as Spec);
    println!("{s}");
    item
}

#[proc_macro_attribute]
pub fn pure(attr: TS1, item: TS1) -> TS1 {
    assert!(attr.is_empty());
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
pub fn modifies(attr: TS1, item: TS1) -> TS1 {
    item
}
