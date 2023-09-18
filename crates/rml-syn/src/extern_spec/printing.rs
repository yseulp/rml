use quote::ToTokens;

use super::{
    ExternSpecEnum, ExternSpecFn, ExternSpecImpl, ExternSpecItem, ExternSpecMod, ExternSpecStruct,
    ExternSpecTrait,
};

impl ToTokens for ExternSpecItem {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            ExternSpecItem::Enum(i) => i.to_tokens(tokens),
            ExternSpecItem::Fn(i) => i.to_tokens(tokens),
            ExternSpecItem::Impl(i) => i.to_tokens(tokens),
            ExternSpecItem::Mod(i) => i.to_tokens(tokens),
            ExternSpecItem::Struct(i) => i.to_tokens(tokens),
            ExternSpecItem::Trait(i) => i.to_tokens(tokens),
        }
    }
}

impl ToTokens for ExternSpecEnum {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for a in &self.attrs {
            a.to_tokens(tokens);
        }
        self.enum_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        self.generics.where_clause.to_tokens(tokens);
        self.brace_token.surround(tokens, |_| {});
    }
}

impl ToTokens for ExternSpecFn {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for a in &self.attrs {
            a.to_tokens(tokens);
        }
        self.sig.to_tokens(tokens);
        self.semi_token.to_tokens(tokens);
    }
}

impl ToTokens for ExternSpecImpl {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.impl_token.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        if let Some((not, path, f)) = &self.trait_ {
            not.to_tokens(tokens);
            path.to_tokens(tokens);
            f.to_tokens(tokens);
        }
        self.self_ty.to_tokens(tokens);
        self.generics.where_clause.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            for i in &self.items {
                i.to_tokens(tokens);
            }
        })
    }
}

impl ToTokens for ExternSpecMod {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.mod_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            for i in &self.content {
                i.to_tokens(tokens);
            }
        })
    }
}

impl ToTokens for ExternSpecStruct {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for a in &self.attrs {
            a.to_tokens(tokens);
        }
        self.struct_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        self.generics.where_clause.to_tokens(tokens);
        self.brace_token.surround(tokens, |_| {})
    }
}

impl ToTokens for ExternSpecTrait {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for a in &self.attrs {
            a.to_tokens(tokens);
        }
        self.trait_token.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.supertraits.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            for i in &self.items {
                i.to_tokens(tokens);
            }
        })
    }
}
