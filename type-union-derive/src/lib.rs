extern crate proc_macro;

mod input;
mod utils;

use quote::quote;
use syn::parse_macro_input;

use crate::input::DefineTypeUnion;

#[proc_macro]
pub fn define_type_union(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DefineTypeUnion);

    quote!(#input).into()
}

/// This macro expands to the type of the union.
///
/// For example invoke it like this:
/// ```ignore
/// fn get_number() -> type_union![u8 | u16 | u64] {
///     42_u64.into()
/// }
/// ```
#[proc_macro]
pub fn type_union(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as input::TypeUnionInput);

    quote!(#input).into()
}

/// Matches a type union:
///
/// ```
/// let my_type: type_union![u8 | u16 | u64] = 42_u64.into();
///
/// match_type_union!(my_type {
///     value: u8 => println!("u8: {}", value),
///     value: u16 => println!("u16: {}", value),
///     value: u64 => println!("u64: {}", value),
/// })
/// ```
#[proc_macro]
pub fn match_type_union(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as input::TypeUnionMatch);

    quote!(#input).into()
}
