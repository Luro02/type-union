use type_union::{define_type_union, type_union};

define_type_union! {
    enum (u8 | u16 | u64);
}

pub fn get_number() -> type_union!(u8 | u16 | u64) {
    42_u64.into()
}

fn main() {
    let number = get_number();
    assert_eq!(number, <type_union!(u8 | u16 | u64)>::from(42_u64));
}
