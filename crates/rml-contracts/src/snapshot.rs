use crate::{std::ops::Deref, *};

pub struct Snapshot<T>(std::marker::PhantomData<T>)
where
    T: ?Sized;

impl<T: ?Sized> Deref for Snapshot<T> {
    type Target = T;

    #[trusted]
    #[logic]
    fn deref(&self) -> &Self::Target {
        panic!()
    }
}

impl<T: ShallowModel + ?Sized> ShallowModel for Snapshot<T> {
    type ShallowModelTy = T::ShallowModelTy;

    // Some comment

    #[trusted]
    #[logic]
    fn shallow_model(self) -> Self::ShallowModelTy {
        panic!()
    }
}

impl<T: ?Sized> Snapshot<T> {
    #[trusted]
    #[logic]
    pub fn new(_: T) -> Snapshot<T> {
        panic!()
    }

    #[trusted]
    #[logic]
    pub fn from_fn<F: Fn() -> Snapshot<T>>(_: F) -> Snapshot<T> {
        panic!()
    }

    #[trusted]
    #[logic]
    pub fn inner(self) -> T
    where
        T: Sized, // TODO: don't require T: Sized here. Problem: return type is T.
    {
        panic!()
    }
}
