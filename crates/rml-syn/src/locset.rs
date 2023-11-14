//! Location sets. Used for modelling what memory locations may be changed by
//! functions.

use syn::{punctuated::Punctuated, token, Member, Token};

use crate::{Term, TermPath};

mod display;
mod encoding;
mod parsing;
mod printing;

mod kw {
    syn::custom_keyword!(nothing);
}

ast_enum_of_structs! {
    /// A location set describes a part of memory. Used to describe what a function may change.
    #[derive(Clone)]
    pub enum LocSet {
        /// A specific field of a term `obj.k` or `obj.0`.
        Field(LocSetField),

        /// The union of all fields of a term `obj.*`.
        FieldWildcard(LocSetFieldWildcard),

        /// A field of a vec or array: `vector[2]`, `vector[1..]`, `vector[1..=2]`, `vector[..]`.
        Index(LocSetIndex),

        /// A path like `std::mem::replace` possibly containing generic
        /// parameters and a qualified self-type.
        ///
        /// A plain identifier like `x` is a path of length 1.
        Path(LocSetPath),

        /// A comma-separated list of loc sets.
        Group(LocSetGroup),

        /// Nothing. The empty loc set.
        Nothing(LocSetNothing),
    }
}

ast_struct! {
    /// A specific field of a term `obj.k` or `obj.0`.
    pub struct LocSetField {
        pub base: Box<Term>,
        pub dot_token: Token![.],
        pub member: Member,
    }
}

ast_struct! {
    /// The union of all fields of a term `obj.*`.
    pub struct LocSetFieldWildcard {
        pub base: Box<Term>,
        pub dot_token: Token![.],
        pub star_token: Token![*],
    }
}

ast_struct! {
    /// A field of a vec or array: `vector[2]`, `vector[1..]`, `vector[1..=2]`, `vector[..]`.
    pub struct LocSetIndex {
        pub term: Box<Term>,
        pub bracket_token: token::Bracket,
        pub index: Box<Term>,
    }
}

ast_struct! {
    /// A path like `std::mem::replace` possibly containing generic
    /// parameters and a qualified self-type.
    ///
    /// A plain identifier like `x` is a path of length 1.
    pub struct LocSetPath {
        pub inner: TermPath,
    }
}

ast_struct! {
    /// A comma-separated list of loc sets.
    pub struct LocSetGroup {
        pub items: Punctuated<LocSet, Token![,]>,
    }
}

ast_struct! {
    /// Nothing. The empty loc set.
    pub struct LocSetNothing {
        pub nothing_token: kw::nothing,
    }
}
