use static_assertions::assert_impl_all;
use type_union::{define_type_union, match_type_union, type_union};

define_type_union! {
    #[impl(Debug, PartialEq, Eq)]
    enum (u8 | u16 | u32);
}

assert_impl_all!(type_union!(u8 | u16 | u32): PartialEq<type_union!(u8 | u16 | u32)>);
assert_impl_all!(u8: ::core::cmp::Eq);
assert_impl_all!(u16: ::core::cmp::Eq);
assert_impl_all!(u32: ::core::cmp::Eq);
assert_impl_all!(type_union!(u8 | u16 | u32): ::core::cmp::Eq);

fn main() {}
