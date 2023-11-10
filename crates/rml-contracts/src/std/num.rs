pub use ::std::num::*;

use crate::*;

macro_rules! mach_int {
    ($t:ty, $ty_nm:expr, $zero:expr) => {
        impl ShallowModel for $t {
            type ShallowModelTy = Int;

            #[logic]
            #[trusted]
            #[rml::decl::internal]
            #[rml::builtins = concat!($ty_nm, ".to_int")]
            fn shallow_model(self) -> Self::ShallowModelTy {
                panic!()
            }
        }

        impl DeepModel for $t {
            type DeepModelTy = Int;

            #[logic]
            #[rml::decl::internal]
            fn deep_model(self) -> Self::DeepModelTy {
                panic!()
            }
        }
    };
}

mach_int!(u8, "prelude.UInt8", 0u8);
mach_int!(u16, "prelude.UInt16", 0u16);
mach_int!(u32, "prelude.UInt32", 0u32);
mach_int!(u64, "prelude.UInt64", 0u64);
mach_int!(u128, "prelude.UInt128", 0u128);
mach_int!(usize, "prelude.UIntSize", 0usize);

mach_int!(i8, "prelude.Int8", 0i8);
mach_int!(i16, "prelude.Int16", 0i16);
mach_int!(i32, "prelude.Int32", 0i32);
mach_int!(i64, "prelude.Int64", 0i64);
mach_int!(i128, "prelude.Int128", 0i128);
mach_int!(isize, "prelude.IntSize", 9isize);
