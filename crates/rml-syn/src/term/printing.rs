use quote::{ToTokens, TokenStreamExt};

use super::*;
use crate::print::TokensOrDefault;

// If the given term is a bare `TermStruct`, wraps it in parenthesis
// before appending it to `TokenStream`.
fn wrap_bare_struct(tokens: &mut TokenStream, e: &Term) {
    if let Term::Struct(_) = *e {
        token::Paren::default().surround(tokens, |tokens| {
            e.to_tokens(tokens);
        });
    } else {
        e.to_tokens(tokens);
    }
}

fn maybe_wrap_else(tokens: &mut TokenStream, else_: &Option<(Token![else], Box<Term>)>) {
    if let Some((else_token, else_)) = else_ {
        else_token.to_tokens(tokens);

        // If we are not one of the valid expressions to exist in an else
        // clause, wrap ourselves in a block.
        match **else_ {
            Term::If(_) | Term::Block(_) => {
                else_.to_tokens(tokens);
            }
            _ => {
                token::Brace::default().surround(tokens, |tokens| {
                    else_.to_tokens(tokens);
                });
            }
        }
    }
}

impl ToTokens for TBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.brace_token.surround(tokens, |tokens| {
            tokens.append_all(&self.stmts);
        });
    }
}

impl ToTokens for TermStmt {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TermStmt::Local(local) => local.to_tokens(tokens),
            TermStmt::Term(term) => term.to_tokens(tokens),
            TermStmt::Semi(term, semi) => {
                term.to_tokens(tokens);
                semi.to_tokens(tokens);
            }
            TermStmt::Item(i) => i.to_tokens(tokens),
        }
    }
}

impl ToTokens for TLocal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.let_token.to_tokens(tokens);
        self.pat.to_tokens(tokens);
        if let Some((eq_token, init)) = &self.init {
            eq_token.to_tokens(tokens);
            init.to_tokens(tokens);
        }
        self.semi_token.to_tokens(tokens);
    }
}

impl ToTokens for TermArray {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.bracket_token.surround(tokens, |tokens| {
            self.elems.to_tokens(tokens);
        })
    }
}

impl ToTokens for TermCall {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.func.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            self.args.to_tokens(tokens);
        })
    }
}

impl ToTokens for TermMethodCall {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.receiver.to_tokens(tokens);
        self.dot_token.to_tokens(tokens);
        self.method.to_tokens(tokens);
        self.turbofish.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            self.args.to_tokens(tokens);
        });
    }
}

impl ToTokens for TermClosure {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.attrs);
        self.or1_token.to_tokens(tokens);
        self.inputs.to_tokens(tokens);
        self.or2_token.to_tokens(tokens);
        self.output.to_tokens(tokens);
        self.body.to_tokens(tokens);
    }
}

impl ToTokens for TermAngleBracketedGenericArguments {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.colon2_token.to_tokens(tokens);
        self.lt_token.to_tokens(tokens);
        self.args.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
    }
}

impl ToTokens for TermGenericMethodArgument {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            TermGenericMethodArgument::Type(t) => t.to_tokens(tokens),
            TermGenericMethodArgument::Const(c) => c.to_tokens(tokens),
        }
    }
}

impl ToTokens for TermTuple {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.paren_token.surround(tokens, |tokens| {
            self.elems.to_tokens(tokens);
            // If we only have one argument, we need a trailing comma to
            // distinguish TermTuple from TermParen.
            if self.elems.len() == 1 && !self.elems.trailing_punct() {
                <Token![,]>::default().to_tokens(tokens);
            }
        })
    }
}

impl ToTokens for TermBinary {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.left.to_tokens(tokens);
        self.op.to_tokens(tokens);
        self.right.to_tokens(tokens);
    }
}

impl ToTokens for TermUnary {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.op.to_tokens(tokens);
        self.term.to_tokens(tokens);
    }
}

impl ToTokens for TermLit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lit.to_tokens(tokens);
    }
}

impl ToTokens for TermCast {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.term.to_tokens(tokens);
        self.as_token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToTokens for TermLogEq {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lhs.to_tokens(tokens);
        self.eqeq_token.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        self.rhs.to_tokens(tokens);
    }
}

impl ToTokens for TermImpl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.hyp.to_tokens(tokens);
        self.eqeq_token.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
        self.cons.to_tokens(tokens);
    }
}

impl ToTokens for TermFinal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.final_token.to_tokens(tokens);
        self.term.to_tokens(tokens);
    }
}

impl ToTokens for TermForall {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.forall_token.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            self.or1_token.to_tokens(tokens);
            for input in self.args.pairs() {
                input.to_tokens(tokens);
            }
            self.or2_token.to_tokens(tokens);
            self.term.to_tokens(tokens);
        });
    }
}

impl ToTokens for TermExists {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.exists_token.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            self.or1_token.to_tokens(tokens);
            for input in self.args.pairs() {
                input.to_tokens(tokens);
            }
            self.or2_token.to_tokens(tokens);
            self.term.to_tokens(tokens);
        });
    }
}

impl ToTokens for QuantArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

impl ToTokens for TermLet {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.let_token.to_tokens(tokens);
        self.pat.to_tokens(tokens);
        self.eq_token.to_tokens(tokens);
        wrap_bare_struct(tokens, &self.term);
    }
}

impl ToTokens for TermIf {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.if_token.to_tokens(tokens);
        wrap_bare_struct(tokens, &self.cond);
        self.then_branch.to_tokens(tokens);
        maybe_wrap_else(tokens, &self.else_branch);
    }
}

impl ToTokens for TermMatch {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.match_token.to_tokens(tokens);
        wrap_bare_struct(tokens, &self.term);
        self.brace_token.surround(tokens, |tokens| {
            for (i, arm) in self.arms.iter().enumerate() {
                arm.to_tokens(tokens);
                // Ensure that we have a comma after a non-block arm, except
                // for the last one.
                let is_last = i == self.arms.len() - 1;
                if !is_last && requires_terminator(&arm.body) && arm.comma.is_none() {
                    <Token![,]>::default().to_tokens(tokens);
                }
            }
        });
    }
}

impl ToTokens for TermBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.label.to_tokens(tokens);
        self.block.brace_token.surround(tokens, |tokens| {
            tokens.append_all(&self.block.stmts);
        });
    }
}

impl ToTokens for TermField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.base.to_tokens(tokens);
        self.dot_token.to_tokens(tokens);
        self.member.to_tokens(tokens);
    }
}

impl ToTokens for TermIndex {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.term.to_tokens(tokens);
        self.bracket_token.surround(tokens, |tokens| {
            self.index.to_tokens(tokens);
        });
    }
}

impl ToTokens for TermRange {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.start.to_tokens(tokens);
        match &self.limits {
            RangeLimits::HalfOpen(t) => t.to_tokens(tokens),
            RangeLimits::Closed(t) => t.to_tokens(tokens),
        }
        self.end.to_tokens(tokens);
    }
}

impl ToTokens for TermPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.inner.to_tokens(tokens)
        // private::print_path(tokens, &self.qself, &self.path);
    }
}

impl ToTokens for TermStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.path.to_tokens(tokens);
        self.brace_token.surround(tokens, |tokens| {
            self.fields.to_tokens(tokens);
            if self.rest.is_some() {
                TokensOrDefault(&self.dot2_token).to_tokens(tokens);
                self.rest.to_tokens(tokens);
            }
        })
    }
}

impl ToTokens for TermRepeat {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.bracket_token.surround(tokens, |tokens| {
            self.term.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
            self.len.to_tokens(tokens);
        })
    }
}

impl ToTokens for TermGroup {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.group_token.surround(tokens, |tokens| {
            self.term.to_tokens(tokens);
        });
    }
}

impl ToTokens for TermParen {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.paren_token.surround(tokens, |tokens| {
            self.term.to_tokens(tokens);
        });
    }
}

impl ToTokens for TermFieldValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.member.to_tokens(tokens);
        if let Some(colon_token) = &self.colon_token {
            colon_token.to_tokens(tokens);
            self.term.to_tokens(tokens);
        }
    }
}

impl ToTokens for TermArm {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pat.to_tokens(tokens);
        if let Some((if_token, guard)) = &self.guard {
            if_token.to_tokens(tokens);
            guard.to_tokens(tokens);
        }
        self.fat_arrow_token.to_tokens(tokens);
        self.body.to_tokens(tokens);
        self.comma.to_tokens(tokens);
    }
}

impl ToTokens for TermModel {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.model_token.to_tokens(tokens);
        self.paren_token
            .surround(tokens, |tokens| self.term.to_tokens(tokens));
    }
}

impl ToTokens for TermOld {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.old_token.to_tokens(tokens);
        self.paren_token.surround(tokens, |tokens| {
            self.term.to_tokens(tokens);
        })
    }
}
