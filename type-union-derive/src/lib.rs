#![feature(iter_intersperse, assert_matches)]
#![allow(clippy::uninlined_format_args, clippy::module_inception)]

extern crate proc_macro;

mod impl_declaration;
mod input;
mod meta_union;
mod utils;

use quote::quote;
use syn::parse_macro_input;

use crate::input::DefineTypeUnion;

#[proc_macro]
pub fn define_type_union(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DefineTypeUnion);

    quote!(#input).into()
}

// TODO: rename match_type_union to typed_match?

/// This macro expands to the type of the union.
///
/// For example invoke it like this:
/// ```
/// # use type_union_derive as type_union;
/// use type_union::{define_type_union, type_union};
///
/// define_type_union! {
///     #[impl(Debug, Clone, PartialEq)]
///     enum (u8 | u16 | u64);
/// }
///
/// fn get_number() -> type_union!(u8 | u16 | u64) {
///     42_u64.into()
/// }
/// ```
#[proc_macro]
pub fn type_union(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as input::TypeUnion<syn::Type>);

    quote!(#input).into()
}

/// Matches a type union:
///
/// ```
/// # use type_union_derive as type_union;
/// use type_union::{define_type_union, type_union, match_type_union};
///
/// define_type_union! {
///     #[impl(Debug, Clone, PartialEq)]
///     enum (u8 | u16 | u64);
/// }
///
/// let my_type: type_union!(u8 | u16 | u64) = 42_u64.into();
///
/// match_type_union!(my_type: type_union!(u8 | u16 | u64) {
///     value: u8 => println!("u8: {}", value),
///     value: u16 => println!("u16: {}", value),
///     value: u64 => println!("u64: {}", value),
/// })
/// ```
#[proc_macro]
pub fn match_type_union(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as input::TypedMatch<syn::Type>);

    quote!(#input).into()
}
