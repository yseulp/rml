use crate::{
    std::ops::{Add, Div, Mul, Neg, Rem, Sub},
    *,
};

pub struct Int(*mut ());

impl Int {
    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn pow(self, _: Int) -> Int {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn max(self, _: Int) -> Int {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn min(self, _: Int) -> Int {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn div_euclid(self, _: Int) -> Int {
        panic!()
    }

    #[trusted]
    #[logic]
    #[rml::decl::internal]
    pub fn rem_euclid(self, _: Int) -> Int {
        panic!()
    }

    #[logic]
    #[rml::decl::internal]
    pub fn abs_diff(self, other: Int) -> Int {
        if self < other {
            other - self
        } else {
            self - other
        }
    }
}

impl Add<Int> for Int {
    type Output = Int;

    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn add(self, _: Int) -> Self {
        panic!()
    }
}

impl Sub<Int> for Int {
    type Output = Int;

    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn sub(self, _: Int) -> Self {
        panic!()
    }
}

impl Mul<Int> for Int {
    type Output = Int;

    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn mul(self, _: Int) -> Self {
        panic!()
    }
}

impl Div<Int> for Int {
    type Output = Int;

    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn div(self, _: Int) -> Self {
        panic!()
    }
}

impl Rem<Int> for Int {
    type Output = Int;

    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn rem(self, _: Int) -> Self {
        panic!()
    }
}

impl Neg for Int {
    type Output = Int;

    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn neg(self) -> Self {
        panic!()
    }
}

impl PartialEq for Int {
    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn eq(&self, _: &Self) -> bool {
        panic!()
    }
}

impl Eq for Int {}

impl PartialOrd for Int {
    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn partial_cmp(&self, _: &Self) -> Option<::std::cmp::Ordering> {
        panic!()
    }
}

impl Ord for Int {
    #[pure]
    #[cfg_attr(rml, rml::decl::internal)]
    fn cmp(&self, _: &Self) -> ::std::cmp::Ordering {
        panic!()
    }
}
