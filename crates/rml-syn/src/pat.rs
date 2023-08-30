//! Patterns for RML.
//!
//! Adapted from syn by replacing all occurrences of expressions with terms.

use proc_macro2::TokenStream;
use syn::{
    punctuated::Punctuated, token, ExprMacro as PatMacro, Ident, Member, Path, QSelf, Token, Type,
};

use super::{TermLit as PatLit, TermPath as PatPath, TermRange as PatRange};

pub(crate) mod display;
pub(crate) mod encoding;
pub(crate) mod parsing;
pub(crate) mod printing;

ast_enum_of_structs! {
    /// A RML pattern. Modeled after syn's Pat.
    #[derive(Clone)]
    #[non_exhaustive]
    pub enum Pat {
        /// A pattern that binds a new variable: `ref mut binding @ SUBPATTERN`.
        Ident(PatIdent),
        /// A literal pattern: `0`.
        Lit(PatLit),
        /// A macro in pattern position.
        Macro(PatMacro),
        /// A pattern that matches any one of a set of cases.
        Or(PatOr),
        /// A parenthesized pattern: `(A | B)`.
        Paren(PatParen),
        /// A path pattern like `Color::Red`, optionally qualified with a
        /// self-type.
        ///
        /// Unqualified path patterns can legally refer to variants, structs,
        /// constants or associated constants. Qualified path patterns like
        /// `<A>::B::C` and `<A as Trait>::B::C` can only legally refer to
        /// associated constants.
        Path(PatPath),
        /// A range pattern: `1..=2`.
        Range(PatRange),
        /// The dots in a tuple or slice pattern: `[0, 1, ..]`.
        Rest(PatRest),
        /// A dynamically sized slice pattern: `[a, b, ref i @ .., y, z]`.
        Slice(PatSlice),
        /// A struct or struct variant pattern: `Variant { x, y, .. }`.
        Struct(PatStruct),
        /// A tuple pattern: `(a, b)`.
        Tuple(PatTuple),
        /// A tuple struct or tuple variant pattern: `Variant(x, y, .., z)`.
        TupleStruct(PatTupleStruct),
        /// A type ascription pattern: `foo: f64`.
        Type(PatType),
        /// Tokens in pattern position not interpreted by Syn.
        Verbatim(TokenStream),
        /// A pattern that matches any value: `_`.
        Wild(PatWild),
    }
}

ast_struct! {
    /// A pattern that binds a new variable: `ref mut binding @ SUBPATTERN`.
    ///
    /// It may also be a unit struct or struct variant (e.g. `None`), or a
    /// constant; these cannot be distinguished syntactically.
    pub struct PatIdent {
        pub ident: Ident,
        pub subpat: Option<(Token![@], Box<Pat>)>,
    }
}

ast_struct! {
    /// A pattern that matches any one of a set of cases.
    pub struct PatOr {
        pub leading_vert: Option<Token![|]>,
        pub cases: Punctuated<Pat,Token![|]>,
    }
}

ast_struct! {
    /// A parenthesized pattern: `(A | B)`.
    pub struct PatParen {
        pub paren_token: token::Paren,
        pub pat: Box<Pat>,
    }
}

ast_struct! {
    pub struct PatRest {
        /// The dots in a tuple or slice pattern: `[0, 1, ..]`.
        pub dot2_token: Token![..],
    }
}

ast_struct! {
    /// A dynamically sized slice pattern: `[a, b, ref i @ .., y, z]`.
    pub struct PatSlice {
        pub bracket_token: token::Bracket,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    /// A struct or struct variant pattern: `Variant { x, y, .. }`.
    pub struct PatStruct {
        pub qself: Option<QSelf>,
        pub path: Path,
        pub brace_token: token::Brace,
        pub fields: Punctuated<FieldPat, Token![,]>,
        pub rest: Option<PatRest>,
    }
}

ast_struct! {
    /// A tuple pattern: `(a, b)`.
    pub struct PatTuple {
        pub paren_token: token::Paren,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    /// A tuple struct or tuple variant pattern: `Variant(x, y, .., z)`.
    pub struct PatTupleStruct {
        pub qself: Option<QSelf>,
        pub path: Path,
        pub paren_token: token::Paren,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    /// A type ascription pattern: `foo: f64`.
    pub struct PatType {
        pub pat: Box<Pat>,
        pub colon_token: Token![:],
        pub ty: Box<Type>,
    }
}

ast_struct! {
    /// A pattern that matches any value: `_`.
    pub struct PatWild {
        pub underscore_token: Token![_],
    }
}

ast_struct! {
    /// A single field in a struct pattern.
    ///
    /// Patterns like the fields of Foo `{ x, ref y, ref mut z }` are treated
    /// the same as `x: x, y: ref y, z: ref mut z` but there is no colon token.
    pub struct FieldPat {
        pub member: Member,
        pub colon_token: Option<Token![:]>,
        pub pat: Box<Pat>,
    }
}
