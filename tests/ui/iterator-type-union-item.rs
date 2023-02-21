use static_assertions::assert_impl_all;
use type_union::{define_type_union, match_type_union, type_union};

use core::array::IntoIter;

// NOTE: this is needed, because generics are not supported yet
type Once = core::iter::Once<u8>;
type Empty = core::iter::Empty<u8>;
type ArrayIter = IntoIter<usize, 3>;

define_type_union! {
    #[impl(Debug, PartialEq)]
    enum (u8 | usize);

    #[impl(Iterator<Item = type_union!(u8 | usize)>)]
    enum (Empty | Once | ArrayIter);
}

assert_impl_all!(type_union!(Empty | Once | ArrayIter): Iterator<Item = type_union!(u8 | usize)>);

fn main() {
    let mut iter: type_union!(Empty | Once | ArrayIter) =
        [1_usize, 2_usize, 3_usize].into_iter().into();

    assert_eq!(iter.next(), Some(<type_union!(u8 | usize)>::from(1_usize)));
    assert_eq!(iter.next(), Some(<type_union!(u8 | usize)>::from(2_usize)));
    assert_eq!(iter.next(), Some(<type_union!(u8 | usize)>::from(3_usize)));
    assert_eq!(iter.next(), None);
}
