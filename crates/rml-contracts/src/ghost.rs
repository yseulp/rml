use crate::{
    std::ops::{Deref, DerefMut},
    *,
};

pub struct Ghost<T>(std::marker::PhantomData<T>)
where
    T: ?Sized;

impl<T: ?Sized> Deref for Ghost<T> {
    type Target = T;

    #[trusted]
    #[strictly_pure]
    fn deref(&self) -> &Self::Target {
        panic!()
    }
}

impl<T: ?Sized> DerefMut for Ghost<T> {
    #[trusted]
    #[strictly_pure]
    fn deref_mut(&mut self) -> &mut Self::Target {
        panic!()
    }
}

impl<T: ShallowModel + ?Sized> ShallowModel for Ghost<T> {
    type ShallowModelTy = T::ShallowModelTy;

    // Some comment

    #[trusted]
    #[logic]
    fn shallow_model(self) -> Self::ShallowModelTy {
        panic!()
    }
}

impl<T: ?Sized> Ghost<T> {
    #[trusted]
    #[strictly_pure]
    pub fn new(_: T) -> Ghost<T> {
        panic!()
    }

    #[trusted]
    #[strictly_pure]
    pub fn phantom() -> Ghost<T> {
        Self(std::marker::PhantomData)
    }

    #[trusted]
    #[strictly_pure]    #[trusted]
    #[logic]
    pub fn inner(self) -> T
    where
        T: Sized, // TODO: don't require T: Sized here. Problem: return type is T.
    {
        panic!()
    }
    pub fn from_fn<F: Fn() -> Ghost<T>>(_: F) -> Ghost<T> {
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
