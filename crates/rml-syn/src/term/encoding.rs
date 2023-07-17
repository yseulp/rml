use crate::{
    Encode, TBlock, TermAngleBracketedGenericArguments, TermArm, TermArray, TermBinary, TermBlock,
    TermCall, TermCast, TermClosure, TermExists, TermField, TermFieldValue, TermForall,
    TermGenericMethodArgument, TermGroup, TermIf, TermImpl, TermIndex, TermLet, TermLit, TermLogEq,
    TermMatch, TermMethodCall, TermParen, TermPath, TermRange, TermRepeat, TermStruct, TermTuple,
    TermUnary,
};

use super::Term;

use syn::{
    parse_quote_spanned,
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
    AngleBracketedGenericArguments, Arm, Block, Expr, ExprArray, ExprBinary, ExprBlock, ExprCall,
    ExprCast, ExprClosure, ExprField, ExprGroup, ExprIf, ExprIndex, ExprLet, ExprLit, ExprMatch,
    ExprMethodCall, ExprParen, ExprRange, ExprRepeat, ExprStruct, ExprTuple, ExprUnary, FieldValue,
    GenericArgument,
};

impl Encode for Term {
    type Target = Expr;

    fn encode(self) -> Expr {
        let sp = self.span();
        match self {
            Term::Array(TermArray {
                bracket_token,
                elems,
            }) => Expr::Array(ExprArray {
                attrs: Vec::new(),
                bracket_token: bracket_token,
                elems: encode_punctuated(elems),
            }),
            Term::Binary(TermBinary { left, op, right }) => Expr::Binary(ExprBinary {
                attrs: Vec::new(),
                left: left.encode().into(),
                op,
                right: right.encode().into(),
            }),
            Term::Block(TermBlock { label, block }) => Expr::Block(ExprBlock {
                attrs: Vec::new(),
                label,
                block: block.encode(),
            }),
            Term::Call(TermCall {
                func,
                paren_token,
                args,
            }) => Expr::Call(ExprCall {
                attrs: Vec::new(),
                func: func.encode().into(),
                paren_token,
                args: encode_punctuated(args),
            }),
            Term::Cast(TermCast { term, as_token, ty }) => Expr::Cast(ExprCast {
                attrs: Vec::new(),
                expr: term.encode().into(),
                as_token,
                ty,
            }),
            Term::Closure(TermClosure {
                attrs,
                or1_token,
                inputs,
                or2_token,
                output,
                body,
            }) => Expr::Closure(ExprClosure {
                attrs,
                lifetimes: None,
                constness: None,
                movability: None,
                asyncness: None,
                capture: None,
                or1_token,
                inputs,
                or2_token,
                output,
                body: body.encode().into(),
            }),
            Term::Exists(TermExists { args, term, .. }) => {
                let mut body = term.encode();
                for arg in args.into_iter().rev() {
                    let id = arg.ident;
                    let ty = arg.ty;
                    body = parse_quote_spanned! { sp => ::rml_contracts::stubs::exists(|#id: #ty| #body) }
                }
                body
            }
            Term::Field(TermField {
                base,
                dot_token,
                member,
            }) => Expr::Field(ExprField {
                attrs: Vec::new(),
                base: base.encode().into(),
                dot_token,
                member,
            }),
            Term::Forall(TermForall { args, term, .. }) => {
                let mut body = term.encode();
                for arg in args.into_iter().rev() {
                    let id = arg.ident;
                    let ty = arg.ty;
                    body = parse_quote_spanned! { sp => ::rml_contracts::stubs::forall(|#id: #ty| #body) }
                }
                body
            }
            Term::Group(TermGroup { group_token, term }) => Expr::Group(ExprGroup {
                attrs: Vec::new(),
                group_token,
                expr: term.encode().into(),
            }),
            Term::If(TermIf {
                if_token,
                cond,
                then_branch,
                else_branch,
            }) => Expr::If(ExprIf {
                attrs: Vec::new(),
                if_token,
                cond: cond.encode().into(),
                then_branch: then_branch.encode().into(),
                else_branch: else_branch.map(|(e, b)| (e, b.encode().into())),
            }),
            Term::Impl(TermImpl { hyp, cons, .. }) => {
                let hyp = hyp.encode();
                let cons = cons.encode();
                parse_quote_spanned! { sp => ::rml_contracts::stubs::impl(#hyp, #cons)}
            }
            Term::Index(TermIndex {
                term,
                bracket_token,
                index,
            }) => Expr::Index(ExprIndex {
                attrs: Vec::new(),
                expr: term.encode().into(),
                bracket_token,
                index: index.encode().into(),
            }),
            Term::Let(TermLet {
                let_token,
                pat,
                eq_token,
                term,
            }) => Expr::Let(ExprLet {
                attrs: Vec::new(),
                let_token,
                pat: pat.into(),
                eq_token,
                expr: term.encode().into(),
            }),
            Term::Lit(TermLit { lit }) => Expr::Lit(ExprLit {
                attrs: Vec::new(),
                lit,
            }),
            Term::LogEq(TermLogEq { lhs, rhs, .. }) => {
                let lhs = lhs.encode();
                let rhs = rhs.encode();
                parse_quote_spanned! { sp => ::rml_contracts::stubs::equiv(#lhs, #rhs) }
            }
            Term::Macro(m) => syn::Expr::Macro(m),
            Term::Match(TermMatch {
                match_token,
                term,
                brace_token,
                arms,
            }) => Expr::Match(ExprMatch {
                attrs: Vec::new(),
                match_token,
                expr: term.encode().into(),
                brace_token,
                arms: arms.into_iter().map(TermArm::encode).collect(),
            }),
            Term::MethodCall(TermMethodCall {
                receiver,
                dot_token,
                method,
                turbofish,
                paren_token,
                args,
            }) => Expr::MethodCall(ExprMethodCall {
                attrs: Vec::new(),
                receiver: receiver.encode().into(),
                dot_token,
                method,
                turbofish: turbofish.map(Encode::encode),
                paren_token,
                args: encode_punctuated(args),
            }),
            Term::Paren(TermParen { paren_token, term }) => Expr::Paren(ExprParen {
                attrs: Vec::new(),
                paren_token,
                expr: term.encode().into(),
            }),
            Term::Path(TermPath { inner }) => Expr::Path(inner),
            Term::Range(TermRange { from, limits, to }) => Expr::Range(ExprRange {
                attrs: Vec::new(),
                start: from.map(|s| s.encode().into()),
                limits,
                end: to.map(|e| e.encode().into()),
            }),
            Term::Repeat(TermRepeat {
                bracket_token,
                term,
                semi_token,
                len,
            }) => Expr::Repeat(ExprRepeat {
                attrs: Vec::new(),
                bracket_token,
                expr: term.encode().into(),
                semi_token,
                len: len.encode().into(),
            }),
            Term::Struct(TermStruct {
                path,
                brace_token,
                fields,
                dot2_token,
                rest,
            }) => Expr::Struct(ExprStruct {
                attrs: Vec::new(),
                qself: None,
                path,
                brace_token,
                fields: encode_punctuated(fields),
                dot2_token,
                rest: rest.map(|r| r.encode().into()),
            }),
            Term::Tuple(TermTuple { paren_token, elems }) => Expr::Tuple(ExprTuple {
                attrs: Vec::new(),
                paren_token,
                elems: encode_punctuated(elems),
            }),
            Term::Unary(TermUnary { op, term }) => Expr::Unary(ExprUnary {
                attrs: Vec::new(),
                op,
                expr: term.encode().into(),
            }),
            Term::Verbatim(t) => Expr::Verbatim(t),
        }
    }
}

impl Encode for TBlock {
    type Target = Block;

    fn encode(self) -> Block {
        todo!()
    }
}

impl Encode for TermArm {
    type Target = Arm;

    fn encode(self) -> Arm {
        todo!()
    }
}

impl Encode for TermAngleBracketedGenericArguments {
    type Target = AngleBracketedGenericArguments;

    fn encode(self) -> Self::Target {
        AngleBracketedGenericArguments {
            colon2_token: self.colon2_token,
            lt_token: self.lt_token,
            args: encode_punctuated(self.args),
            gt_token: self.gt_token,
        }
    }
}

impl Encode for TermGenericMethodArgument {
    type Target = GenericArgument;

    fn encode(self) -> Self::Target {
        match self {
            TermGenericMethodArgument::Type(ty) => GenericArgument::Type(ty),
            TermGenericMethodArgument::Const(t) => GenericArgument::Const(t.encode()),
        }
    }
}

impl Encode for TermFieldValue {
    type Target = FieldValue;

    fn encode(self) -> Self::Target {
        FieldValue {
            attrs: Vec::new(),
            member: self.member,
            colon_token: self.colon_token,
            expr: self.term.encode(),
        }
    }
}

fn encode_punctuated<T, P>(punct: Punctuated<T, P>) -> Punctuated<T::Target, P>
where
    T: Encode,
{
    let mut e_args = Punctuated::new();
    for (e, p) in punct.into_pairs().map(Pair::into_tuple) {
        e_args.push_value(e.encode());
        if let Some(p) = p {
            e_args.push_punct(p);
        }
    }
    e_args
}
