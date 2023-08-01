use quote::ToTokens;

use super::{
    LocSetField, LocSetFieldWildcard, LocSetGroup, LocSetIndex, LocSetNothing, LocSetPath,
};

impl ToTokens for LocSetField {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.base.to_tokens(tokens);
        self.dot_token.to_tokens(tokens);
        self.member.to_tokens(tokens);
    }
}

impl ToTokens for LocSetFieldWildcard {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.base.to_tokens(tokens);
        self.dot_token.to_tokens(tokens);
        self.star_token.to_tokens(tokens);
    }
}

impl ToTokens for LocSetIndex {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.term.to_tokens(tokens);
        self.bracket_token.surround(tokens, |tokens| {
            self.index.to_tokens(tokens);
        })
    }
}

impl ToTokens for LocSetPath {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.inner.to_tokens(tokens);
    }
}

impl ToTokens for LocSetGroup {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.items.to_tokens(tokens);
    }
}

impl ToTokens for LocSetNothing {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.nothing_token.to_tokens(tokens)
    }
}
