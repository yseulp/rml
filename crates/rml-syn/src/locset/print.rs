use quote::ToTokens;

use super::{LocSetField, LocSetFieldWildcard, LocSetIndex, LocSetIndexWildcard, LocSetPath};

impl ToTokens for LocSetField {
    fn to_tokens(&self, _tokens: &mut proc_macro2::TokenStream) {
        todo!()
    }
}

impl ToTokens for LocSetFieldWildcard {
    fn to_tokens(&self, _tokens: &mut proc_macro2::TokenStream) {
        todo!()
    }
}

impl ToTokens for LocSetIndex {
    fn to_tokens(&self, _tokens: &mut proc_macro2::TokenStream) {
        todo!()
    }
}

impl ToTokens for LocSetIndexWildcard {
    fn to_tokens(&self, _tokens: &mut proc_macro2::TokenStream) {
        todo!()
    }
}

impl ToTokens for LocSetPath {
    fn to_tokens(&self, _tokens: &mut proc_macro2::TokenStream) {
        todo!()
    }
}
