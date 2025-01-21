//! Contains the data types for RML terms.
//!
//! Adapted from syn and creusot.

use proc_macro2::{Delimiter, TokenStream};
use syn::{
    parse::discouraged::AnyDelimiter, punctuated::Punctuated, token, Attribute, BinOp, ExprMacro,
    ExprPath, Ident, Item, Label, Lit, Member, Path, RangeLimits, ReturnType, Token, Type, UnOp,
};

use crate::Pat;

pub(crate) mod display;
pub(crate) mod encoding;
pub(crate) mod parsing;
pub(crate) mod printing;

mod kw {
    syn::custom_keyword!(forall);
    syn::custom_keyword!(exists);
    syn::custom_keyword!(old);
    syn::custom_keyword!(model);
}

ast_enum_of_structs! {
    /// A RML term. Modeled after syn's `Expr`.
    #[derive(Clone)]
    #[non_exhaustive]
    pub enum Term {
        /// A slice literal term: `[a, b, c, d]`.
        Array(TermArray),

        /// A binary operation: `a + b`.
        Binary(TermBinary),

        /// A blocked scope: `{ ... }`.
        Block(TermBlock),

        /// A function call term: `invoke(a, b)`.
        Call(TermCall),

        /// A cast term: `foo as f64`.
        Cast(TermCast),

        /// A closure term: `|a, b| a + b`.
        Closure(TermClosure),

        /// Logical existential quantification: `exists(|i: Int| i > 0)`.
        Exists(TermExists),

        /// Access of a named struct field (`obj.k`) or unnamed tuple struct
        /// field (`obj.0`).
        Field(TermField),

        /// Final value of a mutable reference. Only allowed in `demands` parts: `^x`
        Final(TermFinal),

        /// Logical universal quantification: `forall(|x: u32| x >= 0)`.
        Forall(TermForall),

        /// A term contained within invisible delimiters.
        ///
        /// This variant is important for faithfully representing the precedence
        /// of terms and is related to `None`-delimited spans in a
        /// `TokenStream`.
        Group(TermGroup),

        /// An `if` term with an optional `else` block: `if term { ... }
        /// else { ... }`.
        ///
        /// The `else` branch term may only be an `If` or `Block`
        /// term, not any of the other types of term.
        If(TermIf),

        /// Logical implication: `a ==> b`.
        Impl(TermImpl),

        /// A square bracketed indexing term: `vector[2]`.
        Index(TermIndex),

        /// A `let` guard: `let Some(x) = opt`.
        Let(TermLet),

        /// A literal in place of an term: `1`, `"foo"`.
        Lit(TermLit),

        /// Logical equality: `a === b`, rather than [Eq]
        LogEq(TermLogEq),

        /// A macro invocation term: `format!("{}", q)`.
        Macro(ExprMacro),

        /// A `match` term: `match n { Some(n) => {}, None => {} }`.
        Match(TermMatch),

        /// A method call term: `x.foo::<T>(a, b)`.
        MethodCall(TermMethodCall),

        /// A model term: `model(x)`.
        Model(TermModel),

        /// An `old` term: `old(x)`.
        Old(TermOld),

        /// A parenthesized term: `(a + b)`.
        Paren(TermParen),

        /// A path like `std::mem::replace` possibly containing generic
        /// parameters and a qualified self-type.
        ///
        /// A plain identifier like `x` is a path of length 1.
        Path(TermPath),

        /// A range term: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
        Range(TermRange),

        /// An array literal constructed from one repeated element: `[0u8; N]`.
        Repeat(TermRepeat),

        /// A struct literal term: `Point { x: 1, y: 1 }`.
        ///
        /// The `rest` provides the value of the remaining fields as in `S { a:
        /// 1, b: 1, ..rest }`.
        Struct(TermStruct),

        /// A tuple term: `(a, b, c, d)`.
        Tuple(TermTuple),

        /// A unary operation: `!x`, `*x`.
        Unary(TermUnary),

        /// Tokens in term position not interpreted by Syn.
        Verbatim(TokenStream),

        // For testing exhaustiveness in downstream code, use the following idiom:
        //
        //     match term {
        //         Term::Array(term) => {...}
        //         Term::Binary(term) => {...}
        //         ...
        //         Term::Yield(term) => {...}
        //
        //         #[cfg_attr(test, deny(non_exhaustive_omitted_patterns))]
        //         _ => { /* some sane fallback */ }
        //     }
        //
        // This way we fail your tests but don't break your library when adding
        // a variant. You will be notified by a test failure when a variant is
        // added, so that you can add code to handle it, but your library will
        // continue to compile and work for downstream users in the interim.
    }
}

ast_struct! {
    /// A slice literal term: `[a, b, c, d]`.
    pub struct TermArray {
        pub bracket_token: token::Bracket,
        pub elems: Punctuated<Term, Token![,]>,
    }
}

ast_struct! {
    /// A binary operation: `a + b`, `a * b`.
    pub struct TermBinary {
        pub left: Box<Term>,
        pub op: BinOp,
        pub right: Box<Term>,
    }
}

ast_struct! {
    /// A blocked scope: `{ ... }`.
    pub struct TermBlock {
        pub label: Option<Label>,
        pub block: TBlock,
    }
}

ast_struct! {
    /// A function call term: `invoke(a, b)`.
    pub struct TermCall {
        pub func: Box<Term>,
        pub paren_token: token::Paren,
        pub args: Punctuated<Term, Token![,]>,
    }
}

ast_struct! {
    /// A cast term: `foo as f64`.
    pub struct TermCast {
        pub term: Box<Term>,
        pub as_token: Token![as],
        pub ty: Box<Type>,
    }
}

ast_struct! {
    /// A closure term: `|a, b| a + b`.
    pub struct TermClosure {
        pub attrs: Vec<Attribute>,
        pub or1_token: Token![|],
        pub inputs: Punctuated<Pat, Token![,]>,
        pub or2_token: Token![|],
        pub output: ReturnType,
        pub body: Box<Term>,
    }
}

ast_struct! {
    /// Access of a named struct field (`obj.k`) or unnamed tuple struct
    /// field (`obj.0`).
    pub struct TermField {
        pub base: Box<Term>,
        pub dot_token: Token![.],
        pub member: Member,
    }
}

ast_struct! {
    /// An term contained within invisible delimiters.
    ///
    /// This variant is important for faithfully representing the precedence
    /// of terms and is related to `None`-delimited spans in a
    /// `TokenStream`.
    pub struct TermGroup {
        pub group_token: token::Group,
        pub term: Box<Term>,
    }
}

ast_struct! {
    /// An `if` term with an optional `else` block: `if term { ... }
    /// else { ... }`.
    ///
    /// The `else` branch term may only be an `If` or `Block`
    /// term, not any of the other types of term.
    pub struct TermIf {
        pub if_token: Token![if],
        pub cond: Box<Term>,
        pub then_branch: TBlock,
        pub else_branch: Option<(Token![else], Box<Term>)>,
    }
}

ast_struct! {
    /// A square bracketed indexing term: `vector[2]`.
    pub struct TermIndex {
        pub term: Box<Term>,
        pub bracket_token: token::Bracket,
        pub index: Box<Term>,
    }
}

ast_struct! {
    pub struct TermFinal {
        pub final_token: Token![^],
        pub term: Box<Term>,
    }
}

ast_struct! {
    /// Logical universal quantification: `forall(|x: u32| x >= 0)`.
    pub struct TermForall {
        pub forall_token: kw::forall,
        pub paren_token: token::Paren,
        pub or1_token: Token![|],
        pub args: Punctuated<QuantArg, Token![,]>,
        pub or2_token: Token![|],

        pub term: Box<Term>
    }
}

ast_struct! {
    /// Logical existential quantification: `exists(|i: Int| i > 0)`.
    pub struct TermExists {
        pub exists_token: kw::exists,
        pub paren_token: token::Paren,
        pub or1_token: Token![|],
        pub args: Punctuated<QuantArg, Token![,]>,
        pub or2_token: Token![|],

        pub term: Box<Term>
    }
}

ast_struct! {
    /// Logical implication: `a ==> b`.
    pub struct TermImpl {
        pub hyp: Box<Term>,
        pub eqeq_token: Token![==],
        pub gt_token: Token![>],
        pub cons: Box<Term>,
    }
}

ast_struct! {
    /// A `let` guard: `let Some(x) = opt`.
    pub struct TermLet {
        pub let_token: Token![let],
        pub pat: Pat,
        pub eq_token: Token![=],
        pub term: Box<Term>,
    }
}

ast_struct! {
    /// A literal in place of an term: `1`, `"foo"`.
    pub struct TermLit {
        pub lit: Lit,
    }
}

ast_struct! {
    /// A logical equality between terms: `a === b` rather than [Eq]
    pub struct TermLogEq {
        pub lhs: Box<Term>,
        pub eqeq_token: Token![==],
        pub eq_token: Token![=],
        pub rhs: Box<Term>,
    }
}

ast_struct! {
    /// A `match` term: `match n { Some(n) => {}, None => {} }`.
    pub struct TermMatch {
        pub match_token: Token![match],
        pub term: Box<Term>,
        pub brace_token: token::Brace,
        pub arms: Vec<TermArm>,
    }
}

ast_struct! {
    /// A method call term: `x.foo::<T>(a, b)`.
    pub struct TermMethodCall {
        pub receiver: Box<Term>,
        pub dot_token: Token![.],
        pub method: Ident,
        pub turbofish: Option<TermAngleBracketedGenericArguments>,
        pub paren_token: token::Paren,
        pub args: Punctuated<Term, Token![,]>,
    }
}

ast_struct! {
    /// A model term: `x@`.
    pub struct TermModel{
        pub model_token: kw::model,
        pub paren_token: token::Paren,
        pub term: Box<Term>,
    }
}

ast_struct! {
    /// An `old` term: `old(x)`.
    pub struct TermOld {
        pub old_token: kw::old,
        pub paren_token: token::Paren,
        pub term: Box<Term>,
    }
}

ast_struct! {
    /// A parenthesized term: `(a + b)`.
    pub struct TermParen {
        pub paren_token: token::Paren,
        pub term: Box<Term>,
    }
}

ast_struct! {
    /// A path like `std::mem::replace` possibly containing generic
    /// parameters and a qualified self-type.
    ///
    /// A plain identifier like `x` is a path of length 1.
    pub struct TermPath {
        pub inner: ExprPath,
        // pub qself: Option<QSelf>,
        // pub path: Path,
    }
}

ast_struct! {
    /// A range term: `1..2`, `1..`, `..2`, `1..=2`, `..=2`.
    pub struct TermRange {
        pub start: Option<Box<Term>>,
        pub limits: RangeLimits,
        pub end: Option<Box<Term>>,
    }
}

ast_struct! {
    /// An array literal constructed from one repeated element: `[0u8; N]`.
    pub struct TermRepeat {
        pub bracket_token: token::Bracket,
        pub term: Box<Term>,
        pub semi_token: Token![;],
        pub len: Box<Term>,
    }
}

ast_struct! {
    /// A struct literal term: `Point { x: 1, y: 1 }`.
    ///
    /// The `rest` provides the value of the remaining fields as in `S { a:
    /// 1, b: 1, ..rest }`.
    pub struct TermStruct {
        pub path: Path,
        pub brace_token: token::Brace,
        pub fields: Punctuated<TermFieldValue, Token![,]>,
        pub dot2_token: Option<Token![..]>,
        pub rest: Option<Box<Term>>,
    }
}

ast_struct! {
    /// A tuple term: `(a, b, c, d)`.
    pub struct TermTuple {
        pub paren_token: token::Paren,
        pub elems: Punctuated<Term, Token![,]>,
    }
}

ast_struct! {
    /// A unary operation: `!x`, `*x`.
    pub struct TermUnary {
        pub op: UnOp,
        pub term: Box<Term>,
    }
}

// Auxiliary structs

ast_struct! {
    /// A braced block containing RML statements.
    pub struct TBlock {
        pub brace_token: token::Brace,
        /// Statements in a block
        pub stmts: Vec<TermStmt>,
    }
}

ast_enum! {
    /// A statement, usually ending in a semicolon.
    #[derive(Debug, Clone)]
    pub enum TermStmt {
        /// A local (let) binding.
        Local(TLocal),

        /// An item definition.
        Item(Item),

        /// Term without trailing semicolon.
        Term(Term),

        /// Term with trailing semicolon.
        Semi(Term, Token![;]),
    }
}

ast_struct! {
    /// A local `let` binding: `let x: u64 = s.parse()?`.
    pub struct TLocal {
        pub let_token: Token![let],
        pub pat: Pat,
        pub init: Option<(Token![=], Box<Term>)>,
        pub semi_token: Token![;],
    }
}

ast_struct! {
    /// One arm of a `match` term: `0...10 => { return true; }`.
    ///
    /// As in:
    ///
    /// ```
    /// # fn f() -> bool {
    /// #     let n = 0;
    /// match n {
    ///     0..=10 => {
    ///         return true;
    ///     }
    ///     // ...
    ///     # _ => {}
    /// }
    /// #   false
    /// # }
    /// ```
    pub struct TermArm {
        pub pat: Pat,
        pub guard: Option<(Token![if], Box<Term>)>,
        pub fat_arrow_token: Token![=>],
        pub body: Box<Term>,
        pub comma: Option<Token![,]>,
    }
}

ast_struct! {
    /// A field-value pair in a struct literal.
    pub struct TermFieldValue {
        /// Attributes tagged on the field.

        /// Name or index of the field.
        pub member: Member,

        /// The colon in `Struct { x: x }`. If written in shorthand like
        /// `Struct { x }`, there is no colon.
        pub colon_token: Option<Token![:]>,

        /// Value of the field.
        pub term: Term,
    }
}

ast_struct! {
    /// The `::<>` explicit type parameters passed to a method call:
    /// `parse::<u64>()`.
    pub struct TermAngleBracketedGenericArguments {
        pub colon2_token: Option<Token![::]>,
        pub lt_token: Token![<],
        pub args: Punctuated<TermGenericMethodArgument, Token![,]>,
        pub gt_token: Token![>],
    }
}

ast_enum! {
    /// An individual generic argument to a method, like `T`.
    #[derive(Debug, Clone)]
    pub enum TermGenericMethodArgument {
        /// A type argument.
        Type(Type),
        /// A const term. Must be inside of a block.
        ///
        /// NOTE: Identity terms are represented as Type arguments, as
        /// they are indistinguishable syntactically.
        Const(Term),
    }
}

ast_struct! {
    /// An argument (typed) for a quantifier. See [TermForall] and [TermExists].
    pub struct QuantArg {
        pub ident: Ident,
        pub colon_token: Token![:],
        pub ty: Box<Type>,
    }
}

pub(crate) fn requires_terminator(term: &Term) -> bool {
    // see https://github.com/rust-lang/rust/blob/2679c38fc/src/librustc_ast/util/classify.rs#L7-L25
    !matches!(*term, Term::Block(..) | Term::If(..) | Term::Match(..))
}

impl Term {
    const DUMMY: Self = Term::Path(TermPath {
        inner: ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: Path {
                leading_colon: None,
                segments: Punctuated::new(),
            },
        },
    });
}
