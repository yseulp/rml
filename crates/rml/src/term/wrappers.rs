//! In the end, we want to serialize the HIR structures and make them available
//! to other tools. Unfortunately, `rustc`'s data structures do not implement
//! [Serialize] nor [Deserialize]. Neither can we implement them for foreign
//! items.
//!
//! To get aroung this, we define wrappers.

use rustc_hir::{def_id::LocalDefId, HirId, ItemId};
use rustc_span::{def_id::DefId, symbol::Ident, Span, Symbol};
use serde::{Deserialize, Serialize};

use super::serialize;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct SpanWrapper(
    #[serde(
        serialize_with = "serialize::serialize_span",
        deserialize_with = "serialize::deserialize_span"
    )]
    pub Span,
);

impl From<Span> for SpanWrapper {
    fn from(value: Span) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HirIdWrapper(
    #[serde(
        serialize_with = "serialize::serialize_hir_id",
        deserialize_with = "serialize::deserialize_hir_id"
    )]
    pub HirId,
);

impl From<HirId> for HirIdWrapper {
    fn from(value: HirId) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct ItemIdWrapper(
    #[serde(
        serialize_with = "serialize::serialize_item_id",
        deserialize_with = "serialize::deserialize_item_id"
    )]
    pub ItemId,
);

impl From<ItemId> for ItemIdWrapper {
    fn from(value: ItemId) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct LocalDefIdWrapper(
    #[serde(
        serialize_with = "serialize::serialize_local_def_id",
        deserialize_with = "serialize::deserialize_local_def_id"
    )]
    pub LocalDefId,
);

impl From<LocalDefId> for LocalDefIdWrapper {
    fn from(value: LocalDefId) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct DefIdWrapper(
    #[serde(
        serialize_with = "crate::spec::serialize::serialize_did",
        deserialize_with = "crate::spec::serialize::deserialize_did"
    )]
    pub DefId,
);

impl From<DefId> for DefIdWrapper {
    fn from(value: DefId) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct IdentWrapper(
    #[serde(
        serialize_with = "serialize::serialize_ident",
        deserialize_with = "serialize::deserialize_ident"
    )]
    pub Ident,
);

impl From<Ident> for IdentWrapper {
    fn from(value: Ident) -> Self {
        Self(value)
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct SymbolWrapper(
    #[serde(
        serialize_with = "serialize::serialize_symbol",
        deserialize_with = "serialize::deserialize_symbol"
    )]
    pub Symbol,
);

impl From<Symbol> for SymbolWrapper {
    fn from(value: Symbol) -> Self {
        Self(value)
    }
}
