use crate::*;

pub trait WellFounded {}

impl WellFounded for u8 {}

impl WellFounded for u16 {}

impl WellFounded for u32 {}

impl WellFounded for u64 {}

impl WellFounded for u128 {}

impl WellFounded for usize {}

impl WellFounded for i8 {}

impl WellFounded for i16 {}

impl WellFounded for i32 {}

impl WellFounded for i64 {}

impl WellFounded for i128 {}

impl WellFounded for isize {}

impl<T> WellFounded for &T where T: WellFounded {}

impl<T> WellFounded for &mut T where T: WellFounded {}

impl<T> WellFounded for Box<T> where T: WellFounded {}

impl<T1, T2> WellFounded for (T1, T2)
where
    T1: WellFounded,
    T2: WellFounded,
{
}

impl<T1, T2, T3> WellFounded for (T1, T2, T3)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
{
}

impl<T1, T2, T3, T4> WellFounded for (T1, T2, T3, T4)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
{
}

impl<T1, T2, T3, T4, T5> WellFounded for (T1, T2, T3, T4, T5)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
{
}

impl<T1, T2, T3, T4, T5, T6> WellFounded for (T1, T2, T3, T4, T5, T6)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
    T6: WellFounded,
{
}

impl<T1, T2, T3, T4, T5, T6, T7> WellFounded for (T1, T2, T3, T4, T5, T6, T7)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
    T6: WellFounded,
    T7: WellFounded,
{
}

impl<T1, T2, T3, T4, T5, T6, T7, T8> WellFounded for (T1, T2, T3, T4, T5, T6, T7, T8)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
    T6: WellFounded,
    T7: WellFounded,
    T8: WellFounded,
{
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9> WellFounded for (T1, T2, T3, T4, T5, T6, T7, T8, T9)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
    T6: WellFounded,
    T7: WellFounded,
    T8: WellFounded,
    T9: WellFounded,
{
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10> WellFounded
    for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
    T6: WellFounded,
    T7: WellFounded,
    T8: WellFounded,
    T9: WellFounded,
    T10: WellFounded,
{
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11> WellFounded
    for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
    T6: WellFounded,
    T7: WellFounded,
    T8: WellFounded,
    T9: WellFounded,
    T10: WellFounded,
    T11: WellFounded,
{
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12> WellFounded
    for (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)
where
    T1: WellFounded,
    T2: WellFounded,
    T3: WellFounded,
    T4: WellFounded,
    T5: WellFounded,
    T6: WellFounded,
    T7: WellFounded,
    T8: WellFounded,
    T9: WellFounded,
    T10: WellFounded,
    T11: WellFounded,
    T12: WellFounded,
{
}

#[strictly_pure]
pub fn well_founded_check<T>(t: T) -> T
where
    T: WellFounded,
{
    t
}
