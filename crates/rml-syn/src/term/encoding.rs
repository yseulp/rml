use super::Term;
use proc_macro2::TokenStream as TS2;
use quote::quote;

impl Term {
    pub fn encode(&self) -> TS2 {
        quote! { true }
    }
}
