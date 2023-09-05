use crate::*;

pub struct LocSet(*mut ());

impl LocSet {
    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn field<T>(_expr: T) -> Self {
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
