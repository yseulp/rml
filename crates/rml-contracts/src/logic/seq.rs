use crate::*;

pub struct Seq<T>(std::marker::PhantomData<T>)
where
    T: ?Sized;

impl<T> Clone for Seq<T> {
    fn clone(&self) -> Self {
        panic!()
    }
}

impl<T> Copy for Seq<T> {}

impl<T> Seq<T> {
    #[cfg(rml)]
    #[trusted]
    pub const EMPTY: Self = { Seq(std::marker::PhantomData) };

    #[logic]
    pub fn new() -> Self {
        Self::EMPTY
    }

    #[logic]
    #[rml::decl::internal]
    pub fn get(self, ix: Int) -> Option<T> {
        if ix < self.len() {
            Some(self.index_logic(ix))
        } else {
            None
        }
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn subsequence(self, _: Int, _: Int) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn singleton(_: T) -> Self {
        panic!()
    }

    #[logic]
    pub fn tail(self) -> Self {
        self.subsequence(1u8.shallow_model(), self.len())
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn len(self) -> Int {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn set(self, _: Int, _: T) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn ext_eq(self, _: Self) -> bool {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn push(self, _: T) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn concat(self, _: Self) -> Self {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn reverse(self) -> Self {
        panic!()
    }

    #[logic]
    pub fn permutation_of(self, o: Self) -> bool {
        self.permut(o, 0u8.shallow_model(), self.len())
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn permut(self, _: Self, _: Int, _: Int) -> bool {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn exchange(self, _: Self, _: Int, _: Int) -> bool {
        panic!()
    }

    #[logic]
    pub fn contains(self, _: &T) -> bool {
        false
    }

    #[logic]
    pub fn sorted_range(self, _l: Int, _u: Int) -> bool
    where
        T: Ord,
    {
        rml! {
            true
        }
    }

    #[logic]
    pub fn sorted(self) -> bool
    where
        T: Ord,
    {
        self.sorted_range(0u8.shallow_model(), self.len())
    }
}

impl<T> IndexLogic<Int> for Seq<T> {
    type Item = T;

    #[logic]
    #[trusted]
    #[rml::decl::internal]
    fn index_logic(self, _: Int) -> Self::Item {
        panic!()
    }
}
