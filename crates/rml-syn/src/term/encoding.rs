use syn::{
    AngleBracketedGenericArguments, Arm, Block, Expr, ExprArray, ExprBinary, ExprBlock, ExprCall,
    ExprCast, ExprClosure, ExprField, ExprGroup, ExprIf, ExprLet, ExprLit, ExprMatch,
    ExprMethodCall, ExprParen, ExprRange, ExprRepeat, ExprStruct, ExprTuple, ExprUnary, FieldValue,
    GenericArgument, Local, LocalInit, Stmt, parse_quote_spanned,
    punctuated::{Pair, Punctuated},
    spanned::Spanned,
};

use super::Term;
use crate::{
    Encode, TBlock, TLocal, TermAngleBracketedGenericArguments, TermArm, TermArray, TermBinary,
    TermBlock, TermCall, TermCast, TermClosure, TermExists, TermField, TermFieldValue, TermFinal,
    TermForall, TermGenericMethodArgument, TermGroup, TermIf, TermImpl, TermIndex, TermLet,
    TermLit, TermLogEq, TermMatch, TermMethodCall, TermModel, TermOld, TermParen, TermPath,
    TermRange, TermRepeat, TermStmt, TermStruct, TermTuple, TermUnary,
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
                bracket_token,
                elems: encode_punctuated(elems),
            }),
            Term::Binary(TermBinary { left, op, right }) => {
                use syn::BinOp::*;

                let left = left.encode();
                let right = right.encode();
                match op {
                    Lt(_) => {
                        parse_quote_spanned! {
                            sp => ::rml_contracts::logic::OrdLogic::lt_log(#left, #right)
                        }
                    }
                    Le(_) => {
                        parse_quote_spanned! {
                            sp => ::rml_contracts::logic::OrdLogic::le_log(#left, #right)
                        }
                    }
                    Ge(_) => {
                        parse_quote_spanned! {
                            sp => ::rml_contracts::logic::OrdLogic::ge_log(#left, #right)
                        }
                    }
                    Gt(_) => {
                        parse_quote_spanned! {
                            sp => ::rml_contracts::logic::OrdLogic::gt_log(#left, #right)
                        }
                    }
                    _ => Expr::Binary(ExprBinary {
                        attrs: Vec::new(),
                        left: left.into(),
                        op,
                        right: right.into(),
                    }),
                }
            }
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
                inputs: encode_punctuated(inputs),
                or2_token,
                output,
                body: body.encode().into(),
            }),
            Term::Exists(TermExists { args, term, .. }) => {
                let mut body = term.encode();
                for arg in args.into_iter().rev() {
                    let id = arg.ident;
                    let ty = arg.ty;
                    body = parse_quote_spanned! {
                        sp => ::rml_contracts::stubs::exists(#[rml::decl::logic] |#id: #ty| #body)
                    }
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
            Term::Final(TermFinal { term, .. }) => {
                let expr = term.encode();
                parse_quote_spanned! {sp => ::rml_contracts::stubs::final_value(#expr)}
            }
            Term::Forall(TermForall { args, term, .. }) => {
                let mut body = term.encode();
                for arg in args.into_iter().rev() {
                    let id = arg.ident;
                    let ty = arg.ty;
                    body = parse_quote_spanned! {
                        sp => ::rml_contracts::stubs::forall(#[rml::decl::logic] |#id: #ty| #body)
                    }
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
                then_branch: then_branch.encode(),
                else_branch: else_branch.map(|(e, b)| (e, b.encode().into())),
            }),
            Term::Impl(TermImpl { hyp, cons, .. }) => {
                let hyp = hyp.encode();
                let cons = cons.encode();
                parse_quote_spanned! { sp => ::rml_contracts::stubs::implication(#hyp, #cons)}
            }
            Term::Index(TermIndex { term, index, .. }) => {
                let expr = term.encode();
                let index = index.encode();

                parse_quote_spanned! {
                    sp => ::rml_contracts::logic::IndexLogic::index_logic(#expr, #index)
                }
            }
            Term::Let(TermLet {
                let_token,
                pat,
                eq_token,
                term,
            }) => Expr::Let(ExprLet {
                attrs: Vec::new(),
                let_token,
                pat: pat.encode().into(),
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
            Term::Range(TermRange {
                start: from,
                limits,
                end: to,
            }) => Expr::Range(ExprRange {
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
            Term::Model(TermModel { term, .. }) => {
                parse_quote_spanned! {
                    sp => ::rml_contracts::model::ShallowModel::shallow_model(#term)
                }
            }
            Term::Old(TermOld { term, .. }) => {
                parse_quote_spanned! { sp => ::rml_contracts::stubs::old(#term) }
            }
        }
    }
}

impl Encode for TBlock {
    type Target = Block;

    fn encode(self) -> Block {
        syn::Block {
            brace_token: self.brace_token,
            stmts: self.stmts.into_iter().map(|s| s.encode()).collect(),
        }
    }
}

impl Encode for TermArm {
    type Target = Arm;

    fn encode(self) -> Arm {
        syn::Arm {
            attrs: Vec::new(),
            pat: self.pat.encode(),
            guard: self.guard.map(|(r#if, t)| (r#if, t.encode().into())),
            fat_arrow_token: self.fat_arrow_token,
            body: self.body.encode().into(),
            comma: self.comma,
        }
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

impl Encode for TermStmt {
    type Target = Stmt;

    fn encode(self) -> Self::Target {
        match self {
            TermStmt::Local(TLocal {
                let_token,
                pat,
                init,
                semi_token,
            }) => Stmt::Local(Local {
                attrs: Vec::new(),
                let_token,
                pat: pat.encode(),
                init: init.map(|(eq, t)| LocalInit {
                    eq_token: eq,
                    expr: t.encode().into(),
                    diverge: None,
                }),
                semi_token,
            }),
            TermStmt::Item(i) => Stmt::Item(i),
            TermStmt::Term(t) => Stmt::Expr(t.encode(), None),
            TermStmt::Semi(t, semi) => Stmt::Expr(t.encode(), Some(semi)),
        }
    }
}

pub(crate) fn encode_punctuated<T, P>(punct: Punctuated<T, P>) -> Punctuated<T::Target, P>
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
