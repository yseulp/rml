use super::logic;

/// Taken from Creusot:
///
/// The shallow model of a type is typically used to specify a data
/// structure. This kind of model is mostly useful for notation purposes,
/// because this trait is linked to the @ notation of pearlite.
/// Models of inner types are typically not involved.
pub trait ShallowModel {
    type ShallowModelTy;

    #[logic]
    fn shallow_model(self) -> Self::ShallowModelTy;
}

/// Taken from Creusot:
///
/// The deep model corresponds to the model used for specifying
/// operations such as equality, hash function or ordering, which are
/// computed deeply in a data structure.
/// Typically, such a model recursively calls deep models of inner types.
pub trait DeepModel {
    type DeepModelTy;

    #[logic]
    fn deep_model(self) -> Self::DeepModelTy;
}

impl<T> DeepModel for &T
where
    T: DeepModel + ?Sized,
{
    type DeepModelTy = T::DeepModelTy;

    #[logic]
    fn deep_model(self) -> Self::DeepModelTy {
        (*self).deep_model()
    }
}

impl<T> ShallowModel for &T
where
    T: ShallowModel + ?Sized,
{
    type ShallowModelTy = T::ShallowModelTy;

    #[logic]
    fn shallow_model(self) -> Self::ShallowModelTy {
        (*self).shallow_model()
    }
}

impl<T> DeepModel for &mut T
where
    T: DeepModel + ?Sized,
{
    type DeepModelTy = T::DeepModelTy;
    #[logic]
    fn deep_model(self) -> Self::DeepModelTy {
        (*self).deep_model()
    }
}

impl<T> ShallowModel for &mut T
where
    T: ShallowModel + ?Sized,
{
    type ShallowModelTy = T::ShallowModelTy;
    #[logic]
    fn shallow_model(self) -> Self::ShallowModelTy {
        (*self).shallow_model()
    }
}

impl DeepModel for bool {
    type DeepModelTy = bool;

    #[logic]
    fn deep_model(self) -> Self::DeepModelTy {
        self
    }
}
