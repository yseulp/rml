use crate::*;

impl<T, const N: usize> ShallowModel for [T; N] {
    type ShallowModelTy = Seq<T>;

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    fn shallow_model(self) -> Self::ShallowModelTy {
        panic!()
    }
}

impl<T: DeepModel, const N: usize> DeepModel for [T; N] {
    type DeepModelTy = Seq<T::DeepModelTy>;

    #[logic]
    #[trusted]
    #[rml::decl::internal]
    fn deep_model(self) -> Self::DeepModelTy {
        panic!()
    }
}
