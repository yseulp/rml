extern crate proc_macro;
use proc_macro::TokenStream as TS1;

use rml_syn::Spec;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn spec(attr: TS1, item: TS1) -> TS1 {
    println!("Dummy spec called");
    let s = parse_macro_input!(attr as Spec);
    println!("{s}");
    item
}

#[proc_macro_attribute]
pub fn requires(_: TS1, item: TS1) -> TS1 {
    item
}
