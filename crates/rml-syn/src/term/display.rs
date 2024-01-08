use std::fmt;

use quote::ToTokens;
use syn::BinOp;

use super::Term;
use crate::{
    TBlock, TermArray, TermBinary, TermBlock, TermCall, TermCast, TermClosure, TermExists,
    TermField, TermFinal, TermForall, TermGroup, TermIf, TermImpl, TermIndex, TermLet, TermLit,
    TermLogEq, TermMatch, TermMethodCall, TermModel, TermOld, TermParen, TermPath, TermRange,
    TermRepeat, TermStmt, TermStruct, TermTuple, TermUnary,
};

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Array(t) => fmt::Display::fmt(t, f),
            Term::Binary(t) => fmt::Display::fmt(t, f),
            Term::Block(t) => fmt::Display::fmt(t, f),
            Term::Call(t) => fmt::Display::fmt(t, f),
            Term::Cast(t) => fmt::Display::fmt(t, f),
            Term::Closure(t) => fmt::Display::fmt(t, f),
            Term::Exists(t) => fmt::Display::fmt(t, f),
            Term::Field(t) => fmt::Display::fmt(t, f),
            Term::Final(t) => fmt::Display::fmt(t, f),
            Term::Forall(t) => fmt::Display::fmt(t, f),
            Term::Group(t) => fmt::Display::fmt(t, f),
            Term::If(t) => fmt::Display::fmt(t, f),
            Term::Impl(t) => fmt::Display::fmt(t, f),
            Term::Index(t) => fmt::Display::fmt(t, f),
            Term::Let(t) => fmt::Display::fmt(t, f),
            Term::Lit(t) => fmt::Display::fmt(t, f),
            Term::LogEq(t) => fmt::Display::fmt(t, f),
            Term::Macro(t) => fmt::Display::fmt(&t.to_token_stream(), f),
            Term::Match(t) => fmt::Display::fmt(t, f),
            Term::MethodCall(t) => fmt::Display::fmt(t, f),
            Term::Paren(t) => fmt::Display::fmt(t, f),
            Term::Path(t) => fmt::Display::fmt(t, f),
            Term::Range(t) => fmt::Display::fmt(t, f),
            Term::Repeat(t) => fmt::Display::fmt(t, f),
            Term::Struct(t) => fmt::Display::fmt(t, f),
            Term::Tuple(t) => fmt::Display::fmt(t, f),
            Term::Unary(t) => fmt::Display::fmt(t, f),
            Term::Verbatim(t) => fmt::Display::fmt(t, f),
            Term::Model(t) => fmt::Display::fmt(t, f),
            Term::Old(t) => fmt::Display::fmt(t, f),
        }
    }
}

impl fmt::Display for TermArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{}]",
            self.elems
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl fmt::Display for TermBinary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self.op {
            BinOp::Add(_) => "+",
            BinOp::Sub(_) => "-",
            BinOp::Mul(_) => "*",
            BinOp::Div(_) => "/",
            BinOp::Rem(_) => "%",
            BinOp::And(_) => "&&",
            BinOp::Or(_) => "||",
            BinOp::BitXor(_) => "^",
            BinOp::BitAnd(_) => "&",
            BinOp::BitOr(_) => "|",
            BinOp::Shl(_) => "<<",
            BinOp::Shr(_) => ">>",
            BinOp::Eq(_) => "==",
            BinOp::Lt(_) => "<",
            BinOp::Le(_) => "<=",
            BinOp::Ne(_) => "!=",
            BinOp::Ge(_) => ">=",
            BinOp::Gt(_) => ">",
            BinOp::AddAssign(_) => "+=",
            BinOp::SubAssign(_) => "-=",
            BinOp::MulAssign(_) => "*=",
            BinOp::DivAssign(_) => "/=",
            BinOp::RemAssign(_) => "%=",
            BinOp::BitXorAssign(_) => "^=",
            BinOp::BitAndAssign(_) => "&=",
            BinOp::BitOrAssign(_) => "|=",
            BinOp::ShlAssign(_) => "<<=",
            BinOp::ShrAssign(_) => ">>=",
            _ => todo!(),
        };
        write!(f, "{} {} {}", self.left, op, self.right)
    }
}

impl fmt::Display for TermBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(l) = &self.label {
            write!(f, "{}: {}", l.name.ident, self.block)
        } else {
            fmt::Display::fmt(&self.block, f)
        }
    }
}

impl fmt::Display for TBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{{\n{}}}",
            self.stmts
                .iter()
                .map(|s| format!("\t{s}"))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

impl fmt::Display for TermStmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TermStmt::Local(s) => {
                if let Some((_, i)) = &s.init {
                    writeln!(f, "let {} = {};", s.pat.to_token_stream(), i)
                } else {
                    writeln!(f, "let {};", s.pat.to_token_stream())
                }
            }
            TermStmt::Item(s) => writeln!(f, "{}", s.to_token_stream()),
            TermStmt::Term(t) => writeln!(f, "{t}"),
            TermStmt::Semi(t, _) => writeln!(f, "{t};"),
        }
    }
}

impl fmt::Display for TermCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({})",
            self.func,
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl fmt::Display for TermCast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} as {}", self.term, self.ty.to_token_stream())
    }
}

impl fmt::Display for TermClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "|{}| {}", self.inputs.to_token_stream(), self.body)
    }
}

impl fmt::Display for TermExists {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "exists(|{}| {})", self.args.to_token_stream(), self.term)
    }
}

impl fmt::Display for TermField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.base, self.member.to_token_stream())
    }
}

impl fmt::Display for TermFinal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "^{}", self.term)
    }
}

impl fmt::Display for TermForall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "forall(|{}| {})", self.args.to_token_stream(), self.term)
    }
}

impl fmt::Display for TermGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.term, f)
    }
}

impl fmt::Display for TermIf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some((_, e)) = &self.else_branch {
            write!(f, "if {} {} {}", self.cond, self.then_branch, e)
        } else {
            write!(f, "if {} {}", self.cond, self.then_branch)
        }
    }
}

impl fmt::Display for TermImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ==> {}", self.hyp, self.cons)
    }
}

impl fmt::Display for TermIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.term, self.index)
    }
}

impl fmt::Display for TermLet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {}", self.pat.to_token_stream(), self.term)
    }
}

impl fmt::Display for TermLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.lit.to_token_stream(), f)
    }
}

impl fmt::Display for TermLogEq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} === {}", self.lhs, self.rhs)
    }
}

impl fmt::Display for TermMatch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "match {} {{{}}}",
            self.term,
            self.arms
                .iter()
                .map(|a| format!("\n{} => {}", a.pat.to_token_stream(), a.body))
                .collect::<Vec<_>>()
                .join("")
        )
    }
}

impl fmt::Display for TermMethodCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}.{}({})",
            self.receiver,
            self.method,
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl fmt::Display for TermParen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.term)
    }
}

impl fmt::Display for TermPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner.to_token_stream(), f)
    }
}

impl fmt::Display for TermRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (&self.start, self.limits, &self.end) {
            (None, syn::RangeLimits::HalfOpen(_), None) => write!(f, ".."),
            (None, syn::RangeLimits::HalfOpen(_), Some(to)) => write!(f, "..{to}"),
            (None, syn::RangeLimits::Closed(_), None) => write!(f, "..="),
            (None, syn::RangeLimits::Closed(_), Some(to)) => write!(f, "..={to}"),
            (Some(from), syn::RangeLimits::HalfOpen(_), None) => write!(f, "{from}.."),
            (Some(from), syn::RangeLimits::HalfOpen(_), Some(to)) => write!(f, "{from}..{to}"),
            (Some(from), syn::RangeLimits::Closed(_), None) => write!(f, "{from}..="),
            (Some(from), syn::RangeLimits::Closed(_), Some(to)) => write!(f, "{from}..={to}"),
        }
    }
}

impl fmt::Display for TermRepeat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{};{}]", self.term, self.len)
    }
}

impl fmt::Display for TermStruct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(r) = &self.rest {
            write!(
                f,
                "{} {{{}, ..{}}}",
                self.path.to_token_stream(),
                self.fields.to_token_stream(),
                r
            )
        } else {
            write!(
                f,
                "{} {{{}}}",
                self.path.to_token_stream(),
                self.fields.to_token_stream(),
            )
        }
    }
}

impl fmt::Display for TermTuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "({})",
            self.elems
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl fmt::Display for TermUnary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self.op {
            syn::UnOp::Deref(_) => "*",
            syn::UnOp::Not(_) => "!",
            syn::UnOp::Neg(_) => "-",
            _ => todo!(),
        };
        write!(f, "{op}{}", self.term)
    }
}

impl fmt::Display for TermModel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@", self.term)
    }
}

impl fmt::Display for TermOld {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "old({})", self.term)
    }
}
