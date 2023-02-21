use static_assertions::assert_impl_all;
use type_union::{define_type_union, match_type_union, type_union};

use core::array::IntoIter;

// NOTE: this is needed, because generics are not supported yet
type Once = core::iter::Once<u8>;
type Empty = core::iter::Empty<u8>;
type ArrayIter = IntoIter<u8, 3>;

define_type_union! {
    #[impl(Iterator<Item = u8>)]
    enum (Empty | Once | ArrayIter);
}

assert_impl_all!(type_union!(Empty | Once | ArrayIter): Iterator);

fn main() {
    let mut iter: type_union!(Empty | Once | ArrayIter) = [1_u8, 2_u8, 3_u8].into_iter().into();

    assert_eq!(iter.next(), Some(1_u8));
    assert_eq!(iter.next(), Some(2_u8));
    assert_eq!(iter.next(), Some(3_u8));
    assert_eq!(iter.next(), None);
}
