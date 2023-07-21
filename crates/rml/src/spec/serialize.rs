use rustc_span::def_id::DefId;

use serde::{ser::SerializeStruct, Deserializer, Serializer};

pub(crate) fn serialize_did<S>(did: &DefId, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut state = s.serialize_struct("DefId", 2)?;
    state.serialize_field("index", &did.index.as_usize())?;
    state.serialize_field("krate", &did.krate.as_usize())?;
    state.end()
}

pub(crate) fn deserialize_did<'de, D>(d: D) -> Result<DefId, D::Error>
where
    D: Deserializer<'de>,
{
    todo!()
}
