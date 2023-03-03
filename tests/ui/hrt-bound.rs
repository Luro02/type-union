use static_assertions::assert_impl_all;
use type_union::{define_type_union, match_type_union, type_union};

define_type_union! {
    impl<T, anyA> ::core::convert::TryFrom<type_union!(T | ..A)> for T {
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

    #[impl(for<T in Self> PartialEq<T>, Display)]
    enum (u8 | u16 | u32);

    #[impl(for<T in Self> TryFrom<Self> for T)]
    enum (i8 | i16 | i32);
}

assert_impl_all!(
    type_union!(u8 | u16 | u32): PartialEq<u8>,
    PartialEq<u16>,
    PartialEq<u32>,
    ::core::fmt::Display
);

assert_impl_all!(i8: TryFrom<type_union!(i8 | i16 | i32)>);
assert_impl_all!(i16: TryFrom<type_union!(i8 | i16 | i32)>);
assert_impl_all!(i32: TryFrom<type_union!(i8 | i16 | i32)>);

fn main() {}
