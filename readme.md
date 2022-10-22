`type-union`
=============
Adds support for enums without names:

```rust
use type_union::{define_type_union, type_union, match_type_union};

define_type_union! {
    #[impl(default, Clone, PartialEq)]
    enum (u8 | u16 | u64);
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
```
