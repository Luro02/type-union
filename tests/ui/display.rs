use static_assertions::assert_impl_all;
use type_union::{define_type_union, match_type_union, type_union};

define_type_union! {
    #[impl(Display)]
    enum (u8 | u16 | u64);

    #[impl(::core::fmt::Display)]
    enum (String | u8);
}

assert_impl_all!(type_union!(u8 | u16 | u64): ::core::fmt::Display);
assert_impl_all!(type_union!(String | u8): ::core::fmt::Display);

fn main() {
    let a: type_union!(u8 | u16 | u64) = 42_u8.into();
    let b: type_union!(u8 | u16 | u64) = 43_u16.into();
    let c: type_union!(u8 | u16 | u64) = 44_u64.into();

    assert_eq!(a.to_string(), "42".to_string());
    assert_eq!(b.to_string(), "43".to_string());
    assert_eq!(c.to_string(), "44".to_string());

    let d: type_union!(String | u8) = 42_u8.into();
    let e: type_union!(String | u8) = "hello world".to_string().into();

    assert_eq!(d.to_string(), "42".to_string());
    assert_eq!(e.to_string(), "hello world".to_string());
}
