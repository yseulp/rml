//! In the end, we want to serialize the HIR structures and make them available
//! to other tools. Unfortunately, `rustc`'s data structures do not implement
//! [Serialize] nor [Deserialize]. Neither can we implement them for foreign
//! items.
//!
//! To get aroung this, we define wrappers.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct BytePos(pub u32);

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub struct Span {
    pub lo: BytePos,
    pub hi: BytePos,
    // pub ctxt: SyntaxContext,
    pub parent: Option<LocalDefId>,
}

impl From<rustc_span::Span> for Span {
    fn from(value: rustc_span::Span) -> Self {
        Self {
            lo: BytePos(value.lo().0),
            hi: BytePos(value.hi().0),
            parent: value.parent().map(Into::into),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct HirId {
    pub owner: OwnerId,
    pub local_id: ItemLocalId,
}

impl From<rustc_hir::HirId> for HirId {
    fn from(value: rustc_hir::HirId) -> Self {
        Self {
            owner: value.owner.into(),
            local_id: value.local_id.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct ItemLocalId(pub u32);

impl From<rustc_hir::ItemLocalId> for ItemLocalId {
    fn from(value: rustc_hir::ItemLocalId) -> Self {
        Self(value.as_u32())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ItemId {
    pub owner_id: OwnerId,
}

impl From<rustc_hir::ItemId> for ItemId {
    fn from(value: rustc_hir::ItemId) -> Self {
        Self {
            owner_id: value.owner_id.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct OwnerId {
    pub def_id: LocalDefId,
}

impl From<rustc_hir::OwnerId> for OwnerId {
    fn from(value: rustc_hir::OwnerId) -> Self {
        Self {
            def_id: value.def_id.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct LocalDefId {
    pub local_def_index: DefIndex,
}

impl From<rustc_hir::def_id::LocalDefId> for LocalDefId {
    fn from(value: rustc_hir::def_id::LocalDefId) -> Self {
        LocalDefId {
            local_def_index: value.local_def_index.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct DefIndex(pub u32);

impl From<rustc_span::def_id::DefIndex> for DefIndex {
    fn from(value: rustc_span::def_id::DefIndex) -> Self {
        DefIndex(value.as_u32())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct DefId {
    pub index: DefIndex,
    pub krate: CrateNum,
}

impl From<rustc_hir::def_id::DefId> for DefId {
    fn from(value: rustc_hir::def_id::DefId) -> Self {
        Self {
            index: value.index.into(),
            krate: value.krate.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub struct CrateNum(pub u32);

impl From<rustc_span::def_id::CrateNum> for CrateNum {
    fn from(value: rustc_span::def_id::CrateNum) -> Self {
        Self(value.as_u32())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl From<rustc_span::symbol::Ident> for Ident {
    fn from(value: rustc_span::symbol::Ident) -> Self {
        Self {
            name: value.name.into(),
            span: value.span.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Symbol(String);

impl From<rustc_span::Symbol> for Symbol {
    fn from(value: rustc_span::Symbol) -> Self {
        Self(value.to_ident_string())
    }
}
