use std::rc::Rc;

use rustc_hir::{def_id::LocalDefId, ConstArg, HirId, InferArg, ItemId};
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

pub(crate) fn deserialize_hir_id<'de, D>(d: D) -> Result<HirId, D::Error>
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

pub(crate) fn deserialize_span<'de, D>(d: D) -> Result<Span, D::Error>
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

pub(crate) fn deserialize_ident<'de, D>(d: D) -> Result<Ident, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_local_def_id<S>(id: &LocalDefId, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    todo!()
}

pub(crate) fn deserialize_local_def_id<'de, D>(d: D) -> Result<LocalDefId, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_const_arg<S>(id: &ConstArg, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    todo!()
}

pub(crate) fn deserialize_const_arg<'de, D>(d: D) -> Result<ConstArg, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_infer_arg<S>(id: &InferArg, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    todo!()
}

pub(crate) fn deserialize_infer_arg<'de, D>(d: D) -> Result<InferArg, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_symbol<S>(id: &Symbol, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    todo!()
}

pub(crate) fn deserialize_symbol<'de, D>(d: D) -> Result<Symbol, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_item_id<S>(id: &ItemId, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    todo!()
}

pub(crate) fn deserialize_item_id<'de, D>(d: D) -> Result<ItemId, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}

pub(crate) fn serialize_rc_u8_slice<S>(id: &Rc<[u8]>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    todo!()
}

pub(crate) fn deserialize_rc_u8_slice<'de, D>(d: D) -> Result<Rc<[u8]>, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}
