//! Tests that this crates macros are expanded inside its macro invocations.

// no macros are in scope from the `type_union` crate
type_union::define_type_union! {
    #[impl(Display)]
    enum (u8 | u16 | u64);

    #[impl(::core::fmt::Display)]
    enum (String | u8);
}

static_assertions::assert_impl_all!(type_union::type_union!(u8 | u16 | u64): ::core::fmt::Display);
static_assertions::assert_impl_all!(type_union::type_union!(String | u8): ::core::fmt::Display);

fn main() {}
