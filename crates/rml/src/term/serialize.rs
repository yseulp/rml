use rustc_hir::{def_id::LocalDefId, HirId, ItemId};
use rustc_span::{symbol::Ident, Span, Symbol};

use serde::{ser::SerializeStruct, Deserializer, Serializer};

use super::SpanWrapper;

pub(crate) fn serialize_hir_id<S>(hir_id: &HirId, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut state = s.serialize_struct("HirId", 2)?;
    state.serialize_field("owner", &hir_id.owner.def_id.local_def_index.as_usize())?;
    state.serialize_field("local_id", &hir_id.local_id.as_usize())?;
    state.end()
}

pub(crate) fn deserialize_hir_id<'de, D>(_d: D) -> Result<HirId, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_span<S>(span: &Span, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut state = s.serialize_struct("Span", 4)?;
    state.serialize_field("lo", &span.lo().0)?;
    state.serialize_field("hi", &span.hi().0)?;
    // Stupid hack
    let ctxt_str = format!("{:?}", span.ctxt());
    let ctxt: u32 = ctxt_str[1..].parse().unwrap();
    state.serialize_field("ctxt", &ctxt)?;
    state.serialize_field(
        "parent",
        &span.parent().map(|id| id.local_def_index.as_usize()),
    )?;
    state.end()
}

pub(crate) fn deserialize_span<'de, D>(_d: D) -> Result<Span, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_ident<S>(ident: &Ident, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut state = s.serialize_struct("Ident", 2)?;
    state.serialize_field("name", &ident.name.as_u32())?;
    state.serialize_field("span", &SpanWrapper(ident.span))?;
    state.end()
}

pub(crate) fn deserialize_ident<'de, D>(_d: D) -> Result<Ident, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_local_def_id<S>(id: &LocalDefId, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut state = s.serialize_struct("LocalDefId", 1)?;
    state.serialize_field("local_def_index", &id.local_def_index.as_usize())?;
    state.end()
}

pub(crate) fn deserialize_local_def_id<'de, D>(_d: D) -> Result<LocalDefId, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_symbol<S>(id: &Symbol, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.serialize_newtype_struct("Symbol", &id.as_u32())
}

pub(crate) fn deserialize_symbol<'de, D>(_d: D) -> Result<Symbol, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_item_id<S>(id: &ItemId, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut state = s.serialize_struct("ItemId", 1)?;
    state.serialize_field("owner_id", &id.owner_id.def_id.local_def_index.as_usize())?;
    state.end()
}

pub(crate) fn deserialize_item_id<'de, D>(_d: D) -> Result<ItemId, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}
