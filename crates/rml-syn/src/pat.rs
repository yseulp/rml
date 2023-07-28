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
        Ident(PatIdent),
        Lit(PatLit),
        Macro(PatMacro),
        Or(PatOr),
        Paren(PatParen),
        Path(PatPath),
        Range(PatRange),
        Rest(PatRest),
        Slice(PatSlice),
        Struct(PatStruct),
        Tuple(PatTuple),
        TupleStruct(PatTupleStruct),
        Type(PatType),
        Verbatim(TokenStream),
        Wild(PatWild),
    }
}

ast_struct! {
    pub struct PatIdent {
        pub ident: Ident,
        pub subpat: Option<(Token![@], Box<Pat>)>,
    }
}

ast_struct! {
    pub struct PatOr {
        pub leading_vert: Option<Token![|]>,
        pub cases: Punctuated<Pat,Token![|]>,
    }
}

ast_struct! {
    pub struct PatParen {
        pub paren_token: token::Paren,
        pub pat: Box<Pat>,
    }
}

ast_struct! {
    pub struct PatRest {
        pub dot2_token: Token![..],
    }
}

ast_struct! {
    pub struct PatSlice {
        pub bracket_token: token::Bracket,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    pub struct PatStruct {
        pub qself: Option<QSelf>,
        pub path: Path,
        pub brace_token: token::Brace,
        pub fields: Punctuated<FieldPat, Token![,]>,
        pub rest: Option<PatRest>,
    }
}

ast_struct! {
    pub struct PatTuple {
        pub paren_token: token::Paren,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    pub struct PatTupleStruct {
        pub qself: Option<QSelf>,
        pub path: Path,
        pub paren_token: token::Paren,
        pub elems: Punctuated<Pat, Token![,]>,
    }
}

ast_struct! {
    pub struct PatType {
        pub pat: Box<Pat>,
        pub colon_token: Token![:],
        pub ty: Box<Type>,
    }
}

ast_struct! {
    pub struct PatWild {
        pub underscore_token: Token![_],
    }
}

ast_struct! {
    pub struct FieldPat {
        pub member: Member,
        pub colon_token: Option<Token![:]>,
        pub pat: Box<Pat>,
    }
}
