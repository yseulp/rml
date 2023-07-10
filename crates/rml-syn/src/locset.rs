use syn::parse::Parse;

mod print;

ast_enum_of_structs! {
    pub enum LocSetTerm {
        /// A specific field of a term `obj.k` or `obj.0`.
        Field(LocSetField),

        /// The union of all fields of a term `obj.*`.
        FieldWildcard(LocSetFieldWildcard),

        /// A field of a vec or array: `vector[2]`.
        Index(LocSetIndex),

        /// All fields of a vec or array: `vector[*]`.
        IndexWildcard(LocSetIndexWildcard),

        /// A path like `std::mem::replace` possibly containing generic
        /// parameters and a qualified self-type.
        ///
        /// A plain identifier like `x` is a path of length 1.
        Path(LocSetPath),
    }
}

ast_struct! {
    pub struct LocSetField {}
}

ast_struct! {
    pub struct LocSetFieldWildcard {}
}

ast_struct! {
    pub struct LocSetIndex {}
}

ast_struct! {
    pub struct LocSetIndexWildcard {}
}

ast_struct! {
    pub struct LocSetPath {}
}

impl Parse for LocSetTerm {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        todo!()
    }
}
