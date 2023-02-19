use static_assertions::assert_impl_all;
use type_union::{define_type_union, match_type_union, type_union};

define_type_union! {
    #[impl(
        Debug, Clone,
        PartialEq, PartialEq<u8>, PartialEq<u16>, PartialEq<u64>,
        PartialEq<type_union!(u8 | u64)>
    )]
    enum (u8 | u16 | u64);

    #[impl(
        Debug, Clone,
        PartialEq, PartialEq<u8>, PartialEq<u64>,
        PartialEq<type_union!(u8 | u16 | u64)>
    )]
    enum (u8 | u64);
}

macro_rules! assert_eq_reflexive {
    ($left:expr, $right:expr) => {
        assert_eq!($left, $right);
        assert_eq!($right, $left);
    };
}

macro_rules! assert_ne_reflexive {
    ($left:expr, $right:expr) => {
        assert_ne!($left, $right);
        assert_ne!($right, $left);
    };
}

assert_impl_all!(
    type_union!(u8 | u16 | u64): PartialEq<type_union!(u8 | u16 | u64)>,
    PartialEq<type_union!(u8 | u64)>,
);

assert_impl_all!(
    type_union!(u8 | u64): PartialEq<type_union!(u8 | u64)>,
    PartialEq<type_union!(u8 | u16 | u64)>,
);

fn main() {
    let la: type_union!(u8 | u16 | u64) = 1_u8.into();
    let lb: type_union!(u8 | u16 | u64) = 1_u16.into();
    let lc: type_union!(u8 | u16 | u64) = 24_u64.into();

    let ra: type_union!(u8 | u64) = 1_u8.into();
    let rb: type_union!(u8 | u64) = 5_u64.into();
    let rc: type_union!(u8 | u64) = 24_u64.into();

    assert_eq_reflexive!(la, la);
    assert_ne_reflexive!(la, lb);
    assert_ne_reflexive!(la, lc);

    assert_eq_reflexive!(ra, ra);
    assert_ne_reflexive!(ra, rb);
    assert_ne_reflexive!(ra, rc);

    assert_eq_reflexive!(la, ra);
    assert_ne_reflexive!(la, rb);
    assert_ne_reflexive!(la, rc);

    assert_ne_reflexive!(lb, ra);
    assert_ne_reflexive!(lb, rb);
    assert_ne_reflexive!(lb, rc);

    assert_ne_reflexive!(lc, ra);
    assert_ne_reflexive!(lc, rb);
    assert_eq_reflexive!(lc, rc);
}
