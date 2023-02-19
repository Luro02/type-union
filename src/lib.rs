pub use type_union_derive::*;

define_type_union! {
    enum (u8 | u16 | u64);

    // #[impl(PartialEq<u8>, PartialEq<u64>, PartialEq<type_union!(u8 | String | u64)>)]
    // #[impl(PartialEq<type_union!(u8 | u64 | String)>)]
    enum (u8 | u64);

    // #[impl(PartialEq<u8>, PartialEq<u64>)]
    enum (u8 | String | u64);
}

pub fn get_number() -> type_union!(u8 | u16 | u64) {
    42_u64.into()
}

pub fn print_number(input: &type_union!(u8 | u16 | u64)) {
    match_type_union!(input: &type_union!(u8 | u16 | u64) {
        value: &u8 => {
            println!("u8: {}", value)
        },
        value: &u16 => {
            println!("u16: {}", value)
        },
        value: &u64 => {
            println!("u64: {}", value)
        },
    });
}
