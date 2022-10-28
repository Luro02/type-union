pub use type_union_derive::*;

define_type_union! {
    #[impl(default, Clone, PartialEq)]
    enum (u8 | u16 | u64);

    #[impl(Clone, PartialEq, Display)]
    enum (u8 | String | u64);

    #[declare_impl = Display]
    impl<anyT> ::core::fmt::Display for type_union!(anyT)
    where
        anyT: ::core::fmt::Display,
    {
        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
            /* // not supported yet
            let s = self;
            match_type_union!(s: &(anyT) {
                value: &anyT => value.fmt(f),
            }) */
            f.write_str("type union")
        }
    }
}

pub fn get_number() -> type_union!(u8 | u16 | u64) {
    42_u64.into()
}

pub fn print_number(input: &type_union!(u8 | u16 | u64)) {
    match_type_union!(input: &(u8 | u16 | u64) {
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
