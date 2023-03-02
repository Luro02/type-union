//! Tests implementing on non-type unions.

use static_assertions::assert_impl_all;
use type_union::{define_type_union, match_type_union, type_union};

pub struct MyWrapper<T>(T);

define_type_union! {
    impl<anyA> TryFrom<type_union!(..A)> for MyWrapper<type_union!(..A)> {
        type Error = String;

        fn try_from(value: type_union!(..A)) -> Result<Self, Self::Error> {
            Ok(Self(value))
        }
    };

    impl<T, anyA> TryFrom<type_union!(T | ..A)> for T {
        type Error = String;

        fn try_from(value: type_union!(T | ..A)) -> Result<Self, Self::Error> {
            match_type_union!(value: type_union!(T | ..A) {
                value: T => Ok(value),
                _x: anyA => {
                    let expected = ::core::any::type_name::<T>();
                    let actual = ::core::any::type_name::<anyA>();
                    Err(format!("Expected a variable of type `{expected}`, but got `{actual}`"))
                },
            })
        }
    };

    #[impl(TryFrom<Self> for MyWrapper<Self>, TryFrom<Self> for u8, TryFrom<Self> for u16)]
    enum (u8 | u16 | u32);
}

assert_impl_all!(MyWrapper<type_union!(u8 | u16 | u32)>: TryFrom<type_union!(u8 | u16 | u32)>);
assert_impl_all!(u8: TryFrom<type_union!(u8 | u16 | u32)>);
assert_impl_all!(u16: TryFrom<type_union!(u8 | u16 | u32)>);

fn main() {
    let type_union: type_union!(u8 | u16 | u32) = <type_union!(u8 | u16 | u32)>::from(42_u8);
    let _: MyWrapper<type_union!(u8 | u16 | u32)> = MyWrapper::try_from(type_union).unwrap();

    let type_union: type_union!(u8 | u16 | u32) = <type_union!(u8 | u16 | u32)>::from(42_u16);
    assert_eq!(u16::try_from(type_union), Ok(42_u16));
    assert_eq!(
        u8::try_from(type_union),
        Err("Expected a variable of type `u8`, but got `u16`".to_string())
    );
}
