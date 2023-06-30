extern crate proc_macro;
use proc_macro::TokenStream as TS1;

#[proc_macro_attribute]
pub fn spec(_: TS1, item: TS1) -> TS1 {
    println!("Dummy spec called");
    item
}

#[proc_macro_attribute]
pub fn requires(_: TS1, item: TS1) -> TS1 {
    item
}
