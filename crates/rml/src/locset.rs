mod translation;

use serde::{Deserialize, Serialize};

use crate::term::{
    wrappers::{HirIdWrapper, IdentWrapper, SpanWrapper},
    Term, TermQPath,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocSet {
    pub hir_id: HirIdWrapper,
    pub kind: LocSetKind,
    pub span: SpanWrapper,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LocSetKind {
    Field(Box<Term>, IdentWrapper),
    FieldWildcard(Box<Term>),
    Index(Box<Term>, Box<Term>),
    Path(TermQPath),
    Group(Vec<LocSet>),
    Nothing,
}
