use crate::{std::alloc::Allocator, *};

impl<T, A: Allocator> ShallowModel for Vec<T, A> {
    type ShallowModelTy = Seq<T>;

    #[logic]
    #[rml::decl::internal]
    fn shallow_model(self) -> Self::ShallowModelTy {
        panic!()
    }
}
