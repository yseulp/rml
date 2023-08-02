use crate::*;

pub struct LocSet(*mut ());

impl LocSet {
    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn field_named<T>(_base: T, _field: &'static str) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn field_unnamed<T>(_base: T, _field: u32) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn all_fields<T>(_base: T) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn index<T, I>(_expr: T, _idx: I) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn path<T>(_expr: T) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn empty() -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn intersect(self, _o: Self) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn union(self, _o: Self) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn minus(self, _o: Self) -> Self {
        panic!()
    }
}
