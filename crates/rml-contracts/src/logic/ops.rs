use crate::*;

pub trait IndexLogic<I> {
    type Item;

    #[logic]
    fn index_logic(self, idx: I) -> Self::Item;
}

impl<T, S> IndexLogic<Int> for S
where
    S: ShallowModel<ShallowModelTy = Seq<T>> + ?Sized,
{
    type Item = T;

    #[logic]
    #[rml::decl::internal]
    fn index_logic(self, _: Int) -> Self::Item {
        panic!()
    }
}

impl<T, S> IndexLogic<usize> for S
where
    S: ShallowModel<ShallowModelTy = Seq<T>> + ?Sized,
{
    type Item = T;

    #[logic]
    #[rml::decl::internal]
    fn index_logic(self, _: usize) -> Self::Item {
        panic!()
    }
}

impl<T> IndexLogic<Int> for Ghost<Seq<T>> {
    type Item = T;

    #[logic]
    #[trusted]
    #[rml::decl::internal]
    fn index_logic(self, _: Int) -> Self::Item {
        panic!()
    }
}
