//! Extract and handle location sets.

mod translation;

use serde::Serialize;

use crate::{
    hir::{HirId, Ident, Span},
    term::{Term, TermQPath},
};

/// Location set information. Extracted from a expression.
#[derive(Debug, Clone, Serialize)]
pub struct LocSet {
    /// HIR id of the expression the location set is extracted from.
    pub hir_id: HirId,
    /// Kind of the location set.
    pub kind: LocSetKind,
    /// Original span of the expression.
    pub span: Span,
}

/// Kind of the location set.
#[derive(Debug, Clone, Serialize)]
pub enum LocSetKind {
    /// A field location set, like `expr.field`.
    Field(Box<Term>, Ident),
    /// A field wildcard for all fields of a expression, e.g., `expr.*`.
    FieldWildcard(Box<Term>),
    /// An index location set, e.g., `expr[i], expr[i..j], expr[_]`.
    Index(Box<Term>, Box<Term>),
    /// A path location set, e.g., `x`.
    Path(TermQPath),
    /// A group of location sets.
    Group(Vec<LocSet>),
    /// Empty location set.
    Nothing,
}
