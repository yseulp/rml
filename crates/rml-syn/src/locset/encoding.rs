use syn::{parse_quote_spanned, spanned::Spanned, Expr};

use super::{
    LocSet, LocSetField, LocSetFieldWildcard, LocSetGroup, LocSetIndex, LocSetNothing, LocSetPath,
};
use crate::Encode;

impl Encode for LocSet {
    type Target = Expr;

    fn encode(self) -> Self::Target {
        match self {
            LocSet::Field(l) => l.encode(),
            LocSet::FieldWildcard(l) => l.encode(),
            LocSet::Index(l) => l.encode(),
            LocSet::Path(l) => l.encode(),
            LocSet::Group(l) => l.encode(),
            LocSet::Nothing(l) => l.encode(),
        }
    }
}

impl Encode for LocSetField {
    type Target = Expr;

    fn encode(self) -> Self::Target {
        let sp = self.span();
        let base = self.base.encode();
        let member = self.member;
        parse_quote_spanned! { sp => ::rml_contracts::logic::LocSet::field(#base.#member) }
    }
}

impl Encode for LocSetFieldWildcard {
    type Target = Expr;

    fn encode(self) -> Self::Target {
        let sp = self.span();
        let base = self.base.encode();
        parse_quote_spanned! { sp => ::rml_contracts::logic::LocSet::all_fields(#base) }
    }
}

impl Encode for LocSetIndex {
    type Target = Expr;

    fn encode(self) -> Self::Target {
        let sp = self.span();
        let expr = self.term.encode();
        let idx = self.index.encode();

        parse_quote_spanned! { sp => ::rml_contracts::logic::LocSet::index(#expr, #idx) }
    }
}

impl Encode for LocSetPath {
    type Target = Expr;

    fn encode(self) -> Self::Target {
        let sp = self.span();
        let expr = self.inner;
        parse_quote_spanned! { sp => ::rml_contracts::logic::LocSet::path(#expr) }
    }
}

impl Encode for LocSetGroup {
    type Target = Expr;

    fn encode(self) -> Self::Target {
        let sp = self.span();
        self.items.into_iter().fold(
            parse_quote_spanned! { sp => ::rml_contracts::logic::LocSet::empty() },
            |e, i| {
                let i = i.encode();
                parse_quote_spanned! { sp => ::rml_contracts::logic::LocSet::union(#e, #i) }
            },
        )
    }
}

impl Encode for LocSetNothing {
    type Target = Expr;

    fn encode(self) -> Self::Target {
        let sp = self.span();
        parse_quote_spanned! { sp => ::rml_contracts::logic::LocSet::empty() }
    }
}
