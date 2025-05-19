use std::{cmp::Ordering, mem};

use syn::{
    braced, bracketed, parenthesized,
    parse::{Parse, ParseStream, Result},
    Error, Lifetime, LitFloat, PathArguments,
};

use super::*;
use crate::PatType;

syn::custom_keyword!(raw);

// When we're parsing expressions which occur before blocks, like in an if
// statement's condition, we cannot parse a struct literal.
//
// Struct literals are ambiguous in certain positions
// https://github.com/rust-lang/rfcs/pull/92
#[derive(Debug, Clone, Copy)]
pub struct AllowStruct(bool);

enum Precedence {
    Any,
    Assign,
    Impl,
    Range,
    Or,
    And,
    Compare,
    BitOr,
    BitXor,
    BitAnd,
    Shift,
    Arithmetic,
    Term,
    Cast,
}

impl Precedence {
    fn of(op: &BinOp) -> Self {
        match *op {
            BinOp::Add(_) | BinOp::Sub(_) => Precedence::Arithmetic,
            BinOp::Mul(_) | BinOp::Div(_) | BinOp::Rem(_) => Precedence::Term,
            BinOp::And(_) => Precedence::And,
            BinOp::Or(_) => Precedence::Or,
            BinOp::BitXor(_) => Precedence::BitXor,
            BinOp::BitAnd(_) => Precedence::BitAnd,
            BinOp::BitOr(_) => Precedence::BitOr,
            BinOp::Shl(_) | BinOp::Shr(_) => Precedence::Shift,
            BinOp::Eq(_)
            | BinOp::Lt(_)
            | BinOp::Le(_)
            | BinOp::Ne(_)
            | BinOp::Ge(_)
            | BinOp::Gt(_) => Precedence::Compare,
            BinOp::AddAssign(_)
            | BinOp::SubAssign(_)
            | BinOp::MulAssign(_)
            | BinOp::DivAssign(_)
            | BinOp::RemAssign(_)
            | BinOp::BitXorAssign(_)
            | BinOp::BitAndAssign(_)
            | BinOp::BitOrAssign(_)
            | BinOp::ShlAssign(_)
            | BinOp::ShrAssign(_) => Precedence::Assign,
            _ => unimplemented!(),
        }
    }
}

impl Copy for Precedence {}

impl Clone for Precedence {
    fn clone(&self) -> Self {
        *self
    }
}

impl PartialEq for Precedence {
    fn eq(&self, other: &Self) -> bool {
        *self as u8 == *other as u8
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let this = *self as u8;
        let other = *other as u8;
        Some(this.cmp(&other))
    }
}

impl TBlock {
    pub fn parse_within(input: ParseStream) -> Result<Vec<TermStmt>> {
        let mut stmts = Vec::new();
        loop {
            while let Some(semi) = input.parse::<Option<Token![;]>>()? {
                stmts.push(TermStmt::Semi(Term::Verbatim(TokenStream::new()), semi));
            }
            if input.is_empty() {
                break;
            }
            let s = parse_stmt(input, true)?;
            let requires_semicolon = if let TermStmt::Term(s) = &s {
                super::requires_terminator(s)
            } else {
                false
            };
            stmts.push(s);
            if input.is_empty() {
                break;
            } else if requires_semicolon {
                return Err(input.error("unexpected token"));
            }
        }
        Ok(stmts)
    }
}

fn parse_stmt(input: ParseStream, allow_nosemi: bool) -> Result<TermStmt> {
    if input.peek(Token![let]) {
        stmt_local(input).map(TermStmt::Local)
    } else if input.peek(Token![use]) {
        let item: Item = input.parse()?;
        Ok(TermStmt::Item(item))
    } else {
        stmt_expr(input, allow_nosemi)
    }
}

fn stmt_local(input: ParseStream) -> Result<TLocal> {
    Ok(TLocal {
        let_token: input.parse()?,
        pat: {
            // let mut pat: Pat = pat::parsing::multi_pat_with_leading_vert(input)?;
            let mut pat = Pat::parse_single(input)?;
            if input.peek(Token![:]) {
                let colon_token: Token![:] = input.parse()?;
                let ty: Type = input.parse()?;
                pat = Pat::Type(PatType {
                    pat: Box::new(pat),
                    colon_token,
                    ty: Box::new(ty),
                });
            }
            pat
        },
        init: {
            if input.peek(Token![=]) {
                let eq_token: Token![=] = input.parse()?;
                let init: Term = input.parse()?;
                Some((eq_token, Box::new(init)))
            } else {
                None
            }
        },
        semi_token: input.parse()?,
    })
}

fn stmt_expr(input: ParseStream, allow_nosemi: bool) -> Result<TermStmt> {
    let e = term_early(input)?;

    if input.peek(Token![;]) {
        return Ok(TermStmt::Semi(e, input.parse()?));
    }

    if allow_nosemi || !super::requires_terminator(&e) {
        Ok(TermStmt::Term(e))
    } else {
        Err(input.error("expected semicolon"))
    }
}

pub(crate) fn term_early(input: ParseStream) -> Result<Term> {
    let mut expr = if input.peek(Token![if]) {
        Term::If(input.parse()?)
    } else if input.peek(Token![match]) {
        Term::Match(input.parse()?)
    } else if input.peek(token::Brace) {
        Term::Block(input.call(term_block)?)
    } else {
        let allow_struct = AllowStruct(true);
        let expr = unary_term(input, allow_struct)?;

        return parse_term(input, expr, allow_struct, Precedence::Any);
    };

    if input.peek(Token![.]) && !input.peek(Token![..]) || input.peek(Token![?]) {
        expr = trailer_helper(input, expr)?;

        let allow_struct = AllowStruct(true);
        return parse_term(input, expr, allow_struct, Precedence::Any);
    }

    Ok(expr)
}

impl Parse for Term {
    fn parse(input: ParseStream) -> Result<Self> {
        ambiguous_term(input, AllowStruct(true))
    }
}

impl Parse for TermIf {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(TermIf {
            if_token: input.parse()?,
            cond: Box::new(input.call(Term::parse_without_eager_brace)?),
            then_branch: input.parse()?,
            else_branch: {
                if input.peek(Token![else]) {
                    Some(input.call(else_block)?)
                } else {
                    None
                }
            },
        })
    }
}

fn else_block(input: ParseStream) -> Result<(Token![else], Box<Term>)> {
    let else_token: Token![else] = input.parse()?;

    let lookahead = input.lookahead1();
    let else_branch = if input.peek(Token![if]) {
        input.parse().map(Term::If)?
    } else if input.peek(token::Brace) {
        Term::Block(TermBlock {
            label: None,
            block: input.parse()?,
        })
    } else {
        return Err(lookahead.error());
    };

    Ok((else_token, Box::new(else_branch)))
}

impl Parse for TBlock {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(TBlock {
            brace_token: braced!(content in input),
            stmts: content.call(TBlock::parse_within)?,
        })
    }
}

impl Parse for TermMatch {
    fn parse(input: ParseStream) -> Result<Self> {
        let match_token: Token![match] = input.parse()?;
        let expr = Term::parse_without_eager_brace(input)?;

        let content;
        let brace_token = braced!(content in input);

        let mut arms = Vec::new();
        while !content.is_empty() {
            arms.push(content.call(TermArm::parse)?);
        }

        Ok(TermMatch {
            match_token,
            term: Box::new(expr),
            brace_token,
            arms,
        })
    }
}

impl Parse for TermArm {
    fn parse(input: ParseStream) -> Result<TermArm> {
        let requires_comma;
        Ok(TermArm {
            pat: Pat::parse_single(input)?,
            guard: {
                if input.peek(Token![if]) {
                    let if_token: Token![if] = input.parse()?;
                    let guard: Term = input.parse()?;
                    Some((if_token, Box::new(guard)))
                } else {
                    None
                }
            },
            fat_arrow_token: input.parse()?,
            body: {
                let body = input.call(term_early)?;
                requires_comma = requires_terminator(&body);
                Box::new(body)
            },
            comma: {
                if requires_comma && !input.is_empty() {
                    Some(input.parse()?)
                } else {
                    input.parse()?
                }
            },
        })
    }
}

impl Parse for TermStmt {
    fn parse(input: ParseStream) -> Result<Self> {
        parse_stmt(input, false)
    }
}

pub fn term_block(input: ParseStream) -> Result<TermBlock> {
    let label: Option<Label> = input.parse()?;

    let content;
    let brace_token = braced!(content in input);
    let stmts = content.call(TBlock::parse_within)?;

    Ok(TermBlock {
        label,
        block: TBlock { brace_token, stmts },
    })
}

impl Term {
    pub fn parse_without_eager_brace(input: ParseStream) -> Result<Term> {
        ambiguous_term(input, AllowStruct(false))
    }
}

// Parse an arbitrary term.
fn ambiguous_term(input: ParseStream, allow_struct: AllowStruct) -> Result<Term> {
    let lhs = unary_term(input, allow_struct)?;
    parse_term(input, lhs, allow_struct, Precedence::Any)
}

fn unary_term(input: ParseStream, allow_struct: AllowStruct) -> Result<Term> {
    if input.peek(Token![*]) || input.peek(Token![!]) || input.peek(Token![-]) {
        // <UnOp> <trailer>
        Ok(Term::Unary(TermUnary {
            op: input.parse()?,
            term: Box::new(unary_term(input, allow_struct)?),
        }))
    } else {
        trailer_term(input, allow_struct)
    }
}

// <atom> (..<args>) ...
// <atom> . <ident> (..<args>) ...
// <atom> . <ident> ...
// <atom> . <lit> ...
// <atom> [ <expr> ] ...
// <atom> ? ...
fn trailer_term(input: ParseStream, allow_struct: AllowStruct) -> Result<Term> {
    let atom = atom_term(input, allow_struct)?;
    let e = trailer_helper(input, atom)?;
    Ok(e)
}

fn trailer_helper(input: ParseStream, mut e: Term) -> Result<Term> {
    loop {
        if input.peek(token::Paren) {
            let content;
            e = Term::Call(TermCall {
                func: Box::new(e),
                paren_token: parenthesized!(content in input),
                args: content.parse_terminated(Term::parse, Token![,])?,
            });
        } else if input.peek(Token![.]) && !input.peek(Token![..]) && !matches!(e, Term::Range(_)) {
            let mut dot_token: Token![.] = input.parse()?;

            let float_token: Option<LitFloat> = input.parse()?;
            if let Some(float_token) = float_token {
                if multi_index(&mut e, &mut dot_token, float_token)? {
                    continue;
                }
            }

            let member: Member = input.parse()?;
            let turbofish = if matches!(member, Member::Named(_)) && input.peek(Token![::]) {
                Some(TermAngleBracketedGenericArguments {
                    colon2_token: input.parse()?,
                    lt_token: input.parse()?,
                    args: {
                        let mut args = Punctuated::new();
                        loop {
                            if input.peek(Token![>]) {
                                break;
                            }
                            let value = input.call(generic_method_argument)?;
                            args.push_value(value);
                            if input.peek(Token![>]) {
                                break;
                            }
                            let punct = input.parse()?;
                            args.push_punct(punct);
                        }
                        args
                    },
                    gt_token: input.parse()?,
                })
            } else {
                None
            };

            if turbofish.is_some() || input.peek(token::Paren) {
                if let Member::Named(method) = member {
                    let content;
                    e = Term::MethodCall(TermMethodCall {
                        receiver: Box::new(e),
                        dot_token,
                        method,
                        turbofish,
                        paren_token: parenthesized!(content in input),
                        args: content.parse_terminated(Term::parse, Token![,])?,
                    });
                    continue;
                }
            }

            e = Term::Field(TermField {
                base: Box::new(e),
                dot_token,
                member,
            });
        } else if input.peek(token::Bracket) {
            let content;
            e = Term::Index(TermIndex {
                term: Box::new(e),
                bracket_token: bracketed!(content in input),
                index: content.parse()?,
            });
        } else {
            break;
        }
    }
    Ok(e)
}

// Parse all atomic expressions which don't have to worry about precedence
// interactions, as they are fully contained.
fn atom_term(input: ParseStream, allow_struct: AllowStruct) -> Result<Term> {
    if input.peek(token::Group)
        && !input.peek2(Token![::])
        && !input.peek2(Token![!])
        && !input.peek2(token::Brace)
    {
        input.call(term_group).map(Term::Group)
    } else if input.peek(Lit) {
        input.parse().map(Term::Lit)
    } else if input.peek(Token![|]) {
        term_closure(input, allow_struct).map(Term::Closure)
    } else if (input.peek(Ident) && !(input.peek(kw::forall) || input.peek(kw::exists)))
        || input.peek(Token![::])
        || input.peek(Token![<])
        || input.peek(Token![self])
        || input.peek(Token![Self])
        || input.peek(Token![super])
        || input.peek(Token![crate])
    {
        path_or_macro_or_struct(input, allow_struct)
    } else if input.peek(token::Paren) {
        paren_or_tuple(input)
    } else if input.peek(token::Bracket) {
        array_or_repeat(input)
    } else if input.peek(Token![let]) {
        input.call(term_let).map(Term::Let)
    } else if input.peek(Token![if]) {
        input.parse().map(Term::If)
    } else if input.peek(kw::forall) {
        input.parse().map(Term::Forall)
    } else if input.peek(kw::exists) {
        input.parse().map(Term::Exists)
    } else if input.peek(kw::old) {
        input.parse().map(Term::Old)
    } else if input.peek(Token![match]) {
        input.parse().map(Term::Match)
    } else if input.peek(token::Brace) {
        input.call(term_block).map(Term::Block)
    } else if input.peek(Token![..]) {
        term_range(input, allow_struct).map(Term::Range)
    } else if input.peek(Lifetime) {
        let the_label: Label = input.parse()?;
        let mut expr = if input.peek(token::Brace) {
            Term::Block(input.call(term_block)?)
        } else {
            return Err(input.error("expected loop or block term"));
        };
        match &mut expr {
            Term::Block(TermBlock { label, .. }) => *label = Some(the_label),
            _ => unreachable!(),
        }
        Ok(expr)
    } else {
        Err(input.error("expected term"))
    }
}

fn path_or_macro_or_struct(input: ParseStream, allow_struct: AllowStruct) -> Result<Term> {
    let begin = input.fork();
    let expr: TermPath = input.parse()?;

    if expr.inner.qself.is_none() && input.peek(Token![!]) && !input.peek(Token![!=]) {
        let mut contains_arguments = false;
        for segment in &expr.inner.path.segments {
            match segment.arguments {
                PathArguments::None => {}
                PathArguments::AngleBracketed(_) | PathArguments::Parenthesized(_) => {
                    contains_arguments = true;
                }
            }
        }

        if !contains_arguments {
            let _bang_token: Token![!] = input.parse()?;
            return Ok(Term::Macro(begin.parse()?));
        }
    }

    if allow_struct.0 && input.peek(token::Brace) {
        term_struct_helper(input, expr.inner.path).map(Term::Struct)
    } else {
        Ok(Term::Path(expr))
    }
}

fn paren_or_tuple(input: ParseStream) -> Result<Term> {
    let content;
    let paren_token = parenthesized!(content in input);
    if content.is_empty() {
        return Ok(Term::Tuple(TermTuple {
            paren_token,
            elems: Punctuated::new(),
        }));
    }

    let first: Term = content.parse()?;
    if content.is_empty() {
        return Ok(Term::Paren(TermParen {
            paren_token,
            term: Box::new(first),
        }));
    }

    let mut elems = Punctuated::new();
    elems.push_value(first);
    while !content.is_empty() {
        let punct = content.parse()?;
        elems.push_punct(punct);
        if content.is_empty() {
            break;
        }
        let value = content.parse()?;
        elems.push_value(value);
    }
    Ok(Term::Tuple(TermTuple { paren_token, elems }))
}

fn array_or_repeat(input: ParseStream) -> Result<Term> {
    let content;
    let bracket_token = bracketed!(content in input);
    if content.is_empty() {
        return Ok(Term::Array(TermArray {
            bracket_token,
            elems: Punctuated::new(),
        }));
    }

    let first: Term = content.parse()?;
    if content.is_empty() || content.peek(Token![,]) {
        let mut elems = Punctuated::new();
        elems.push_value(first);
        while !content.is_empty() {
            let punct = content.parse()?;
            elems.push_punct(punct);
            if content.is_empty() {
                break;
            }
            let value = content.parse()?;
            elems.push_value(value);
        }
        Ok(Term::Array(TermArray {
            bracket_token,
            elems,
        }))
    } else if content.peek(Token![;]) {
        let semi_token: Token![;] = content.parse()?;
        let len: Term = content.parse()?;
        Ok(Term::Repeat(TermRepeat {
            bracket_token,
            term: Box::new(first),
            semi_token,
            len: Box::new(len),
        }))
    } else {
        Err(content.error("expected `,` or `;`"))
    }
}

fn term_group(input: ParseStream) -> Result<TermGroup> {
    input
        .parse_any_delimiter()
        .and_then(|(delim, span, content)| {
            assert_eq!(delim, Delimiter::None);
            Ok(TermGroup {
                group_token: token::Group(span.join()),
                term: content.parse()?,
            })
        })
}

fn generic_method_argument(input: ParseStream) -> Result<TermGenericMethodArgument> {
    if input.peek(Lit) {
        let lit = input.parse()?;
        return Ok(TermGenericMethodArgument::Const(Term::Lit(lit)));
    }

    if input.peek(token::Brace) {
        let block = input.call(super::parsing::term_block)?;
        return Ok(TermGenericMethodArgument::Const(Term::Block(block)));
    }

    input.parse().map(TermGenericMethodArgument::Type)
}

impl Parse for TermLit {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(TermLit {
            lit: input.parse()?,
        })
    }
}

fn term_struct_helper(input: ParseStream, path: Path) -> Result<TermStruct> {
    let content;
    let brace_token = braced!(content in input);

    let mut fields = Punctuated::new();
    while !content.is_empty() {
        if content.peek(Token![..]) {
            return Ok(TermStruct {
                brace_token,
                path,
                fields,
                dot2_token: Some(content.parse()?),
                rest: Some(Box::new(content.parse()?)),
            });
        }

        fields.push(content.parse()?);
        if content.is_empty() {
            break;
        }
        let punct: Token![,] = content.parse()?;
        fields.push_punct(punct);
    }

    Ok(TermStruct {
        brace_token,
        path,
        fields,
        dot2_token: None,
        rest: None,
    })
}

impl Parse for TermFieldValue {
    fn parse(input: ParseStream) -> Result<Self> {
        let member: Member = input.parse()?;
        let (colon_token, value) = if input.peek(Token![:]) || !matches!(member, Member::Named(_)) {
            let colon_token: Token![:] = input.parse()?;
            let value: Term = input.parse()?;
            (Some(colon_token), value)
        } else if let Member::Named(ident) = &member {
            let value = Term::Path(TermPath {
                inner: ExprPath {
                    qself: None,
                    path: Path::from(ident.clone()),
                    attrs: Vec::new(),
                },
            });
            (None, value)
        } else {
            unreachable!()
        };

        Ok(TermFieldValue {
            member,
            colon_token,
            term: value,
        })
    }
}

fn multi_index(e: &mut Term, dot_token: &mut Token![.], float: LitFloat) -> Result<bool> {
    let mut float_repr = float.to_string();
    let trailing_dot = float_repr.ends_with('.');
    if trailing_dot {
        float_repr.truncate(float_repr.len() - 1);
    }
    for part in float_repr.split('.') {
        let index = syn::parse_str(part).map_err(|err| Error::new(float.span(), err))?;
        let base = mem::replace(e, Term::DUMMY);
        *e = Term::Field(TermField {
            base: Box::new(base),
            dot_token: Token![.](dot_token.span),
            member: Member::Unnamed(index),
        });
        *dot_token = Token![.](float.span());
    }
    Ok(!trailing_dot)
}

fn term_closure(input: ParseStream, allow_struct: AllowStruct) -> Result<TermClosure> {
    let or1_token: Token![|] = input.parse()?;

    let mut inputs = Punctuated::new();
    loop {
        if input.peek(Token![|]) {
            break;
        }
        let value = closure_arg(input)?;
        inputs.push_value(value);
        if input.peek(Token![|]) {
            break;
        }
        let punct: Token![,] = input.parse()?;
        inputs.push_punct(punct);
    }

    let or2_token: Token![|] = input.parse()?;

    let (output, body) = if input.peek(Token![->]) {
        let arrow_token: Token![->] = input.parse()?;
        let ty: Type = input.parse()?;
        let body: TBlock = input.parse()?;
        let output = ReturnType::Type(arrow_token, Box::new(ty));
        let block = Term::Block(TermBlock {
            label: None,
            block: body,
        });
        (output, block)
    } else {
        let body = ambiguous_term(input, allow_struct)?;
        (ReturnType::Default, body)
    };

    Ok(TermClosure {
        attrs: Vec::new(),
        or1_token,
        inputs,
        or2_token,
        output,
        body: Box::new(body),
    })
}

fn closure_arg(input: ParseStream) -> Result<Pat> {
    let pat = Pat::parse_single(input)?;

    if input.peek(Token![:]) {
        Ok(Pat::Type(PatType {
            pat: Box::new(pat),
            colon_token: input.parse()?,
            ty: input.parse()?,
        }))
    } else {
        Ok(pat)
    }
}

fn parse_term(
    input: ParseStream,
    mut lhs: Term,
    allow_struct: AllowStruct,
    base: Precedence,
) -> Result<Term> {
    loop {
        if Precedence::Impl >= base && input.peek(Token![==]) && input.peek3(Token![>]) {
            // a ==> b
            let eqeq_token: Token![==] = input.parse()?;
            let gt_token: Token![>] = input.parse()?;
            let precedence = Precedence::Impl;
            let mut rhs = unary_term(input, allow_struct)?;
            loop {
                let next = peek_precedence(input);
                if next >= precedence {
                    rhs = parse_term(input, rhs, allow_struct, next)?;
                } else {
                    break;
                }
            }
            lhs = Term::Impl(TermImpl {
                hyp: Box::new(lhs),
                eqeq_token,
                gt_token,
                cons: Box::new(rhs),
            });
        } else if Precedence::Compare >= base && input.peek(Token![==]) && input.peek3(Token![=]) {
            // a === b
            let eqeq_token: Token![==] = input.parse()?;
            let eq_token: Token![=] = input.parse()?;
            let precedence = Precedence::Compare;
            let mut rhs = unary_term(input, allow_struct)?;
            loop {
                let next = peek_precedence(input);
                if next >= precedence {
                    rhs = parse_term(input, rhs, allow_struct, next)?;
                } else {
                    break;
                }
            }

            lhs = Term::LogEq(TermLogEq {
                lhs: Box::new(lhs),
                eqeq_token,
                eq_token,
                rhs: Box::new(rhs),
            })
        } else if input
            .fork()
            .parse::<BinOp>()
            .ok()
            .is_some_and(|op| Precedence::of(&op) >= base)
            && !(input.peek(Token![==]) && (input.peek3(Token![>]) || input.peek3(Token![=])))
        {
            let op: BinOp = input.parse()?;
            let precedence = Precedence::of(&op);
            let mut rhs = unary_term(input, allow_struct)?;
            loop {
                let next = peek_precedence(input);
                if next > precedence || next == precedence && precedence == Precedence::Assign {
                    rhs = parse_term(input, rhs, allow_struct, next)?;
                } else {
                    break;
                }
            }
            lhs = Term::Binary(TermBinary {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            });
        } else if Precedence::Range >= base && input.peek(Token![..]) {
            let limits: RangeLimits = input.parse()?;
            let rhs = if matches!(limits, RangeLimits::HalfOpen(_))
                && (input.is_empty()
                    || input.peek(Token![,])
                    || input.peek(Token![;])
                    || input.peek(Token![.]) && !input.peek(Token![..])
                    || !allow_struct.0 && input.peek(token::Brace))
            {
                None
            } else {
                let mut rhs = unary_term(input, allow_struct)?;
                loop {
                    let next = peek_precedence(input);
                    if next > Precedence::Range {
                        rhs = parse_term(input, rhs, allow_struct, next)?;
                    } else {
                        break;
                    }
                }
                Some(rhs)
            };
            lhs = Term::Range(TermRange {
                start: Some(Box::new(lhs)),
                limits,
                end: rhs.map(Box::new),
            });
        } else if Precedence::Cast >= base && input.peek(Token![as]) {
            let as_token: Token![as] = input.parse()?;
            let ty = input.call(Type::without_plus)?;
            lhs = Term::Cast(TermCast {
                term: Box::new(lhs),
                as_token,
                ty: Box::new(ty),
            });
        } else {
            break;
        }
    }

    Ok(lhs)
}

fn peek_precedence(input: ParseStream) -> Precedence {
    if input.peek(Token![==]) && input.peek3(Token![=]) {
        Precedence::Compare
    } else if input.peek(Token![==]) && input.peek3(Token![>]) {
        Precedence::Impl
    } else if let Ok(op) = input.fork().parse() {
        Precedence::of(&op)
    } else if input.peek(Token![=]) && !input.peek(Token![=>]) {
        Precedence::Assign
    } else if input.peek(Token![..]) {
        Precedence::Range
    } else if input.peek(Token![as]) {
        Precedence::Cast
    } else {
        Precedence::Any
    }
}

fn term_let(input: ParseStream) -> Result<TermLet> {
    Ok(TermLet {
        let_token: input.parse()?,
        pat: Pat::parse_single(input)?,
        // pat: pat::parsing::multi_pat_with_leading_vert(input)?,
        eq_token: input.parse()?,
        term: Box::new(input.call(Term::parse_without_eager_brace)?),
    })
}

fn term_range(input: ParseStream, allow_struct: AllowStruct) -> Result<TermRange> {
    Ok(TermRange {
        start: None,
        limits: input.parse()?,
        end: {
            if input.is_empty()
                || input.peek(Token![,])
                || input.peek(Token![;])
                || !allow_struct.0 && input.peek(token::Brace)
            {
                None
            } else {
                let to = ambiguous_term(input, allow_struct)?;
                Some(Box::new(to))
            }
        },
    })
}

impl Parse for TermForall {
    fn parse(input: ParseStream) -> Result<Self> {
        let forall_token = input.parse()?;
        let content;
        let paren_token = parenthesized!(content in input);
        let or1_token: Token![|] = content.parse()?;

        let mut args = Punctuated::new();
        while !content.peek(Token![|]) {
            let quantarg = content.parse()?;
            args.push_value(quantarg);
            if content.peek(Token![|]) {
                break;
            }

            let punct = content.parse()?;
            args.push_punct(punct);
        }

        let or2_token: Token![|] = content.parse()?;

        let term = content.parse()?;

        Ok(TermForall {
            forall_token,
            paren_token,
            or1_token,
            args,
            or2_token,
            term,
        })
    }
}

impl Parse for TermExists {
    fn parse(input: ParseStream) -> Result<Self> {
        let exists_token = input.parse()?;
        let content;
        let paren_token = parenthesized!(content in input);
        let or1_token: Token![|] = content.parse()?;

        let mut args = Punctuated::new();
        while !content.peek(Token![|]) {
            let quantarg = content.parse()?;
            args.push_value(quantarg);
            if content.peek(Token![|]) {
                break;
            }

            let punct = content.parse()?;
            args.push_punct(punct);
        }

        let or2_token: Token![|] = content.parse()?;

        let term = content.parse()?;

        Ok(TermExists {
            exists_token,
            paren_token,
            or1_token,
            args,
            or2_token,
            term,
        })
    }
}

impl Parse for QuantArg {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        let colon_token = input.parse()?;
        let ty = input.parse()?;
        Ok(QuantArg {
            ident,
            colon_token,
            ty,
        })
    }
}

impl Parse for TermPath {
    fn parse(input: ParseStream) -> Result<Self> {
        let exp_path: ExprPath = input.parse()?;

        // let (qself, path) = syn::path::parsing::qpath(input, true)?;
        // Ok(TermPath { qself: exp_path.qself, path: exp_path.path })
        Ok(TermPath { inner: exp_path })
    }
}

impl Parse for TermOld {
    fn parse(input: ParseStream) -> Result<Self> {
        let old_token = input.parse()?;
        let content;
        let paren_token = parenthesized!(content in input);
        let term: Term = content.parse()?;

        Ok(TermOld {
            old_token,
            paren_token,
            term: term.into(),
        })
    }
}
