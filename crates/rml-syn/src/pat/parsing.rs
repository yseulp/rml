use quote::ToTokens;
use syn::{
    braced, bracketed, ext::IdentExt, parenthesized, parse::ParseStream, punctuated::Punctuated,
    token, Error, ExprMacro, ExprPath, Ident, Lit, Macro, Member, Path, PathArguments, PathSegment,
    QSelf, RangeLimits, Result, Token, Type,
};

use super::{
    FieldPat, Pat, PatIdent, PatOr, PatParen, PatRest, PatSlice, PatStruct, PatTuple,
    PatTupleStruct, PatWild,
};
use crate::{parse_delimiter, Term, TermLit, TermPath, TermRange};

impl Pat {
    /// Taken from `syn`
    ///
    /// Parse a pattern that does _not_ involve `|` at the top level.
    ///
    /// This parser matches the behavior of the `$:pat_param` macro_rules
    /// matcher, and on editions prior to Rust 2021, the behavior of
    /// `$:pat`.
    ///
    /// In Rust syntax, some examples of where this syntax would occur are
    /// in the argument pattern of functions and closures. Patterns using
    /// `|` are not allowed to occur in these positions.
    ///
    /// ```compile_fail
    /// fn f(Some(_) | None: Option<T>) {
    ///     let _ = |Some(_) | None: Option<T>| {};
    ///     //       ^^^^^^^^^^^^^^^^^^^^^^^^^??? :(
    /// }
    /// ```
    ///
    /// ```console
    /// error: top-level or-patterns are not allowed in function parameters
    ///  --> src/main.rs:1:6
    ///   |
    /// 1 | fn f(Some(_) | None: Option<T>) {
    ///   |      ^^^^^^^^^^^^^^ help: wrap the pattern in parentheses: `(Some(_) | None)`
    /// ```
    pub fn parse_single(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident)
            && (input.peek2(Token![::])
                || input.peek2(Token![!])
                || input.peek2(token::Brace)
                || input.peek2(token::Paren)
                || input.peek2(Token![..]))
            || input.peek(Token![self]) && input.peek2(Token![::])
            || lookahead.peek(Token![::])
            || lookahead.peek(Token![<])
            || input.peek(Token![Self])
            || input.peek(Token![super])
            || input.peek(Token![crate])
        {
            pat_path_or_macro_or_struct_or_range(input)
        } else if lookahead.peek(Token![_]) {
            input.call(pat_wild).map(Pat::Wild)
        } else if input.peek(Token![-]) || lookahead.peek(Lit) {
            pat_lit_or_range(input)
        } else if input.peek(Token![self]) || input.peek(Ident) {
            input.call(pat_ident).map(Pat::Ident)
        } else if lookahead.peek(token::Paren) {
            input.call(pat_paren_or_tuple)
        } else if lookahead.peek(token::Bracket) {
            input.call(pat_slice).map(Pat::Slice)
        } else if lookahead.peek(Token![..]) && !input.peek(Token![...]) {
            pat_range_half_open(input)
        } else {
            Err(lookahead.error())
        }
    }

    /// Parse a pattern, possibly involving `|`, but not a leading `|`.
    pub fn parse_multi(input: ParseStream) -> Result<Self> {
        multi_pat_impl(input, None)
    }

    /// Parse a pattern, possibly involving `|`, possibly including a
    /// leading `|`.
    ///
    /// This parser matches the behavior of the Rust 2021 edition's `$:pat`
    /// macro_rules matcher.
    ///
    /// In Rust syntax, an example of where this syntax would occur is in
    /// the pattern of a `match` arm, where the language permits an optional
    /// leading `|`, although it is not idiomatic to write one there in
    /// handwritten code.
    ///
    /// ```
    /// # let wat = None;
    /// match wat {
    ///     None | Some(false) => {}
    ///     Some(true) => {}
    /// }
    /// ```
    ///
    /// The compiler accepts it only to facilitate some situations in
    /// macro-generated code where a macro author might need to write:
    ///
    /// ```
    /// # macro_rules! doc {
    /// #     ($value:expr, ($($conditions1:pat),*), ($($conditions2:pat),*), $then:expr) => {
    /// match $value {
    ///     $(| $conditions1)* $(| $conditions2)* => $then
    /// }
    /// #     };
    /// # }
    /// #
    /// # doc!(true, (true), (false), {});
    /// # doc!(true, (), (true, false), {});
    /// # doc!(true, (true, false), (), {});
    /// ```
    ///
    /// Expressing the same thing correctly in the case that either one (but
    /// not both) of `$conditions1` and `$conditions2` might be empty,
    /// without leading `|`, is complex.
    ///
    /// Use [`Pat::parse_multi`] instead if you are not intending to support
    /// macro-generated macro input.
    pub fn parse_multi_with_leading_vert(input: ParseStream) -> Result<Self> {
        let leading_vert: Option<Token![|]> = input.parse()?;
        multi_pat_impl(input, leading_vert)
    }
}

fn multi_pat_impl(input: ParseStream, leading_vert: Option<Token![|]>) -> Result<Pat> {
    let mut pat = Pat::parse_single(input)?;
    if leading_vert.is_some()
        || input.peek(Token![|]) && !input.peek(Token![||]) && !input.peek(Token![|=])
    {
        let mut cases = Punctuated::new();
        cases.push_value(pat);
        while input.peek(Token![|]) && !input.peek(Token![||]) && !input.peek(Token![|=]) {
            let punct = input.parse()?;
            cases.push_punct(punct);
            let pat = Pat::parse_single(input)?;
            cases.push_value(pat);
        }
        pat = Pat::Or(PatOr {
            leading_vert,
            cases,
        });
    }
    Ok(pat)
}

fn pat_path_or_macro_or_struct_or_range(input: ParseStream) -> Result<Pat> {
    let (qself, path) = qpath(input, true)?;

    if qself.is_none()
        && input.peek(Token![!])
        && !input.peek(Token![!=])
        && path_is_mod_style(&path)
    {
        let bang_token: Token![!] = input.parse()?;
        let (delimiter, tokens) = parse_delimiter(input)?;
        return Ok(Pat::Macro(ExprMacro {
            attrs: Vec::new(),
            mac: Macro {
                path,
                bang_token,
                delimiter,
                tokens,
            },
        }));
    }

    if input.peek(token::Brace) {
        pat_struct(input, qself, path).map(Pat::Struct)
    } else if input.peek(token::Paren) {
        pat_tuple_struct(input, qself, path).map(Pat::TupleStruct)
    } else if input.peek(Token![..]) {
        pat_range(input, qself, path)
    } else {
        Ok(Pat::Path(TermPath {
            inner: ExprPath {
                attrs: Vec::new(),
                qself,
                path,
            },
        }))
    }
}

fn pat_wild(input: ParseStream) -> Result<PatWild> {
    Ok(PatWild {
        underscore_token: input.parse()?,
    })
}

fn pat_ident(input: ParseStream) -> Result<PatIdent> {
    Ok(PatIdent {
        ident: input.call(Ident::parse_any)?,
        subpat: {
            if input.peek(Token![@]) {
                let at_token: Token![@] = input.parse()?;
                let subpat = Pat::parse_single(input)?;
                Some((at_token, Box::new(subpat)))
            } else {
                None
            }
        },
    })
}

fn pat_tuple_struct(
    input: ParseStream,
    qself: Option<QSelf>,
    path: Path,
) -> Result<PatTupleStruct> {
    let content;
    let paren_token = parenthesized!(content in input);

    let mut elems = Punctuated::new();
    while !content.is_empty() {
        let value = Pat::parse_multi_with_leading_vert(&content)?;
        elems.push_value(value);
        if content.is_empty() {
            break;
        }
        let punct = content.parse()?;
        elems.push_punct(punct);
    }

    Ok(PatTupleStruct {
        qself,
        path,
        paren_token,
        elems,
    })
}

fn pat_struct(input: ParseStream, qself: Option<QSelf>, path: Path) -> Result<PatStruct> {
    let content;
    let brace_token = braced!(content in input);

    let mut fields = Punctuated::new();
    let mut rest = None;
    while !content.is_empty() {
        if content.peek(Token![..]) {
            rest = Some(PatRest {
                dot2_token: content.parse()?,
            });
            break;
        }
        let value = content.call(field_pat)?;
        fields.push_value(value);
        if content.is_empty() {
            break;
        }
        let punct: Token![,] = content.parse()?;
        fields.push_punct(punct);
    }

    Ok(PatStruct {
        qself,
        path,
        brace_token,
        fields,
        rest,
    })
}

fn field_pat(input: ParseStream) -> Result<FieldPat> {
    let member = input.parse()?;

    if input.peek(Token![:]) || member_is_unnamed(&member) {
        return Ok(FieldPat {
            member,
            colon_token: Some(input.parse()?),
            pat: Box::new(Pat::parse_multi_with_leading_vert(input)?),
        });
    }

    let ident = match member {
        Member::Named(ident) => ident,
        Member::Unnamed(_) => unreachable!(),
    };

    let pat = Pat::Ident(PatIdent {
        ident: ident.clone(),
        subpat: None,
    });

    Ok(FieldPat {
        member: Member::Named(ident),
        colon_token: None,
        pat: Box::new(pat),
    })
}

fn pat_range(input: ParseStream, qself: Option<QSelf>, path: Path) -> Result<Pat> {
    let limits = parse_obselete_range_limits(input)?;
    let end = input.call(pat_range_bound)?;
    if let (RangeLimits::Closed(_), None) = (&limits, &end) {
        return Err(input.error("expected range upper bound"));
    }
    Ok(Pat::Range(TermRange {
        start: Some(Box::new(Term::Path(TermPath {
            inner: ExprPath {
                attrs: Vec::new(),
                qself,
                path,
            },
        }))),
        limits,
        end: end.map(PatRangeBound::into_expr),
    }))
}

fn pat_range_half_open(input: ParseStream) -> Result<Pat> {
    let limits: RangeLimits = input.parse()?;
    let end = input.call(pat_range_bound)?;
    if end.is_some() {
        Ok(Pat::Range(TermRange {
            start: None,
            limits,
            end: end.map(PatRangeBound::into_expr),
        }))
    } else {
        match limits {
            RangeLimits::HalfOpen(dot2_token) => Ok(Pat::Rest(PatRest { dot2_token })),
            RangeLimits::Closed(_) => Err(input.error("expected range upper bound")),
        }
    }
}

fn pat_paren_or_tuple(input: ParseStream) -> Result<Pat> {
    let content;
    let paren_token = parenthesized!(content in input);

    let mut elems = Punctuated::new();
    while !content.is_empty() {
        let value = Pat::parse_multi_with_leading_vert(&content)?;
        if content.is_empty() {
            if elems.is_empty() && !matches!(value, Pat::Rest(_)) {
                return Ok(Pat::Paren(PatParen {
                    paren_token,
                    pat: Box::new(value),
                }));
            }
            elems.push_value(value);
            break;
        }
        elems.push_value(value);
        let punct = content.parse()?;
        elems.push_punct(punct);
    }

    Ok(Pat::Tuple(PatTuple { paren_token, elems }))
}

fn pat_lit_or_range(input: ParseStream) -> Result<Pat> {
    let start = input.call(pat_range_bound)?.unwrap();
    if input.peek(Token![..]) {
        let limits = parse_obselete_range_limits(input)?;
        let end = input.call(pat_range_bound)?;
        if let (RangeLimits::Closed(_), None) = (&limits, &end) {
            return Err(input.error("expected range upper bound"));
        }
        Ok(Pat::Range(TermRange {
            start: Some(start.into_expr()),
            limits,
            end: end.map(PatRangeBound::into_expr),
        }))
    } else {
        Ok(start.into_pat())
    }
}

// Patterns that can appear on either side of a range pattern.
enum PatRangeBound {
    Lit(TermLit),
    Path(TermPath),
}

impl PatRangeBound {
    fn into_expr(self) -> Box<Term> {
        Box::new(match self {
            PatRangeBound::Lit(pat) => Term::Lit(pat),
            PatRangeBound::Path(pat) => Term::Path(pat),
        })
    }

    fn into_pat(self) -> Pat {
        match self {
            PatRangeBound::Lit(pat) => Pat::Lit(pat),
            PatRangeBound::Path(pat) => Pat::Path(pat),
        }
    }
}

fn pat_range_bound(input: ParseStream) -> Result<Option<PatRangeBound>> {
    if input.is_empty()
        || input.peek(Token![|])
        || input.peek(Token![=])
        || input.peek(Token![:]) && !input.peek(Token![::])
        || input.peek(Token![,])
        || input.peek(Token![;])
        || input.peek(Token![if])
    {
        return Ok(None);
    }

    let lookahead = input.lookahead1();
    let expr = if lookahead.peek(Lit) {
        PatRangeBound::Lit(input.parse()?)
    } else if lookahead.peek(Ident)
        || lookahead.peek(Token![::])
        || lookahead.peek(Token![<])
        || lookahead.peek(Token![self])
        || lookahead.peek(Token![Self])
        || lookahead.peek(Token![super])
        || lookahead.peek(Token![crate])
    {
        PatRangeBound::Path(input.parse()?)
    } else {
        return Err(lookahead.error());
    };

    Ok(Some(expr))
}

fn pat_slice(input: ParseStream) -> Result<PatSlice> {
    let content;
    let bracket_token = bracketed!(content in input);

    let mut elems = Punctuated::new();
    while !content.is_empty() {
        let value = Pat::parse_multi_with_leading_vert(&content)?;
        match value {
            Pat::Range(pat) if pat.start.is_none() || pat.end.is_none() => {
                let tok: Box<dyn ToTokens> = match pat.limits {
                    RangeLimits::HalfOpen(dot_dot) => Box::new(dot_dot),
                    RangeLimits::Closed(dot_dot_eq) => Box::new(dot_dot_eq),
                };
                let msg = "range pattern is not allowed unparenthesized inside slice pattern";
                return Err(Error::new_spanned(tok, msg));
            }
            _ => {}
        }
        elems.push_value(value);
        if content.is_empty() {
            break;
        }
        let punct = content.parse()?;
        elems.push_punct(punct);
    }

    Ok(PatSlice {
        bracket_token,
        elems,
    })
}

fn parse_obselete_range_limits(input: ParseStream) -> Result<RangeLimits> {
    let lookahead = input.lookahead1();
    let dot_dot = lookahead.peek(Token![..]);
    let dot_dot_eq = dot_dot && lookahead.peek(Token![..=]);
    let dot_dot_dot = dot_dot && input.peek(Token![...]);
    if dot_dot_eq {
        input.parse().map(RangeLimits::Closed)
    } else if dot_dot_dot {
        let dot3: Token![...] = input.parse()?;
        Ok(RangeLimits::Closed(Token![..=](dot3.spans)))
    } else if dot_dot {
        input.parse().map(RangeLimits::HalfOpen)
    } else {
        Err(lookahead.error())
    }
}

fn member_is_unnamed(m: &Member) -> bool {
    match m {
        Member::Named(_) => false,
        Member::Unnamed(_) => true,
    }
}

fn qpath(input: ParseStream, expr_style: bool) -> Result<(Option<QSelf>, Path)> {
    if input.peek(Token![<]) {
        let lt_token: Token![<] = input.parse()?;
        let this: Type = input.parse()?;
        let path = if input.peek(Token![as]) {
            let as_token: Token![as] = input.parse()?;
            let path: Path = input.parse()?;
            Some((as_token, path))
        } else {
            None
        };
        let gt_token: Token![>] = input.parse()?;
        let colon2_token: Token![::] = input.parse()?;
        let mut rest = Punctuated::new();
        loop {
            let path = path_segment_parse_helper(input, expr_style)?;
            rest.push_value(path);
            if !input.peek(Token![::]) {
                break;
            }
            let punct: Token![::] = input.parse()?;
            rest.push_punct(punct);
        }
        let (position, as_token, path) = match path {
            Some((as_token, mut path)) => {
                let pos = path.segments.len();
                path.segments.push_punct(colon2_token);
                path.segments.extend(rest.into_pairs());
                (pos, Some(as_token), path)
            }
            None => {
                let path = Path {
                    leading_colon: Some(colon2_token),
                    segments: rest,
                };
                (0, None, path)
            }
        };
        let qself = QSelf {
            lt_token,
            ty: Box::new(this),
            position,
            as_token,
            gt_token,
        };
        Ok((Some(qself), path))
    } else {
        let path = path_parse_helper(input, expr_style)?;
        Ok((None, path))
    }
}

fn path_segment_parse_helper(input: ParseStream, expr_style: bool) -> Result<PathSegment> {
    if input.peek(Token![super]) || input.peek(Token![self]) || input.peek(Token![crate]) {
        let ident = input.call(Ident::parse_any)?;
        return Ok(PathSegment::from(ident));
    }

    let ident = if input.peek(Token![Self]) {
        input.call(Ident::parse_any)?
    } else {
        input.parse()?
    };

    if !expr_style && input.peek(Token![<]) && !input.peek(Token![<=])
        || input.peek(Token![::]) && input.peek3(Token![<])
    {
        Ok(PathSegment {
            ident,
            arguments: PathArguments::AngleBracketed(input.parse()?),
        })
    } else {
        Ok(PathSegment::from(ident))
    }
}

fn path_parse_helper(input: ParseStream, expr_style: bool) -> Result<Path> {
    let mut path = Path {
        leading_colon: input.parse()?,
        segments: {
            let mut segments = Punctuated::new();
            let value = path_segment_parse_helper(input, expr_style)?;
            segments.push_value(value);
            segments
        },
    };
    path_parse_rest(input, &mut path, expr_style)?;
    Ok(path)
}

fn path_parse_rest(input: ParseStream, path: &mut Path, expr_style: bool) -> Result<()> {
    while input.peek(Token![::]) && !input.peek3(token::Paren) {
        let punct: Token![::] = input.parse()?;
        path.segments.push_punct(punct);
        let value = path_segment_parse_helper(input, expr_style)?;
        path.segments.push_value(value);
    }
    Ok(())
}

fn path_is_mod_style(path: &Path) -> bool {
    path.segments
        .iter()
        .all(|segment| segment.arguments.is_none())
}
