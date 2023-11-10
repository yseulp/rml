use std::cmp;

use quote::ToTokens;

use super::*;
use crate::print::TokensOrDefault;

impl ToTokens for PatIdent {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
        if let Some((at, pat)) = &self.subpat {
            at.to_tokens(tokens);
            pat.to_tokens(tokens);
        }
    }
}

impl ToTokens for PatOr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.leading_vert.to_tokens(tokens);
        self.cases.to_tokens(tokens);
    }
}

impl ToTokens for PatParen {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.paren_token
            .surround(tokens, |tokens| self.pat.to_tokens(tokens))
    }
}

impl ToTokens for PatRest {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.dot2_token.to_tokens(tokens)
    }
}

impl ToTokens for PatSlice {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.bracket_token
            .surround(tokens, |tokens| self.elems.to_tokens(tokens))
    }
}

impl ToTokens for PatStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        print_path(tokens, &self.qself, &self.path);
        self.brace_token.surround(tokens, |tokens| {
            self.fields.to_tokens(tokens);

            if !self.fields.empty_or_trailing() && self.rest.is_some() {
                <Token![,]>::default().to_tokens(tokens);
            }
            self.rest.to_tokens(tokens);
        })
    }
}

impl ToTokens for PatTuple {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.paren_token
            .surround(tokens, |tokens| self.elems.to_tokens(tokens))
    }
}

impl ToTokens for PatTupleStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        print_path(tokens, &self.qself, &self.path);
        self.paren_token
            .surround(tokens, |tokens| self.elems.to_tokens(tokens))
    }
}

impl ToTokens for PatType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pat.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToTokens for PatWild {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.underscore_token.to_tokens(tokens)
    }
}

impl ToTokens for FieldPat {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(colon_token) = &self.colon_token {
            self.member.to_tokens(tokens);
            colon_token.to_tokens(tokens);
        }
        self.pat.to_tokens(tokens);
    }
}

pub(crate) fn print_path(tokens: &mut TokenStream, qself: &Option<QSelf>, path: &Path) {
    let qself = match qself {
        Some(qself) => qself,
        None => {
            path.to_tokens(tokens);
            return;
        }
    };

    qself.lt_token.to_tokens(tokens);
    qself.ty.to_tokens(tokens);

    let pos = cmp::min(qself.position, path.segments.len());
    let mut segments = path.segments.pairs();
    if pos > 0 {
        TokensOrDefault(&qself.as_token).to_tokens(tokens);
        path.leading_colon.to_tokens(tokens);
        for (i, segment) in segments.by_ref().take(pos).enumerate() {
            if i + 1 == pos {
                segment.value().to_tokens(tokens);
                qself.gt_token.to_tokens(tokens);
                segment.punct().to_tokens(tokens);
            } else {
                segment.to_tokens(tokens);
            }
        }
    } else {
        qself.gt_token.to_tokens(tokens);
        path.leading_colon.to_tokens(tokens);
    }
    for segment in segments {
        segment.to_tokens(tokens);
    }
}
