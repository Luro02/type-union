use convert_case::{Case, Casing};
use indexmap::IndexSet;
use proc_macro2::Span;
use quote::ToTokens;
use syn::spanned::Spanned;
use syn::{Ident, Type};

mod generator;
mod looks_like;
mod punctuated_set;
mod syn_ext;

pub use generator::*;
pub use looks_like::*;
pub use punctuated_set::*;
pub use syn_ext::*;

#[must_use]
pub fn is_macro<T>(mac: &syn::Macro, name: T) -> bool
where
    syn::Ident: PartialEq<T>,
{
    mac.path.segments.len() == 1 && mac.path.segments[0].ident == name
}

// TODO: test that the span is applied correctly

#[must_use]
pub fn assert_expr_is_type(expr: &syn::Expr, ty: &syn::Type) -> syn::Expr {
    syn::parse_quote_spanned!(ty.span() => {
        fn _assert_is_ty(v: #ty) -> #ty { v }
        _assert_is_ty(#expr)
    })
}

pub fn resolve_type_name(ty: &Type) -> Option<Ident> {
    match ty {
        Type::Path(ty) => Some(ty.path.segments.last()?.ident.clone()),
        _ => None,
    }
}

pub fn join_idents(idents: impl Iterator<Item = Ident>) -> syn::Ident {
    syn::Ident::new(
        &idents
            .map(|ident| ident.to_string())
            .collect::<Vec<_>>()
            .join("")
            .to_case(Case::UpperCamel),
        Span::call_site(),
    )
}

#[must_use]
pub fn resolve_type_union_name<'a>(variants: impl IntoIterator<Item = &'a Type>) -> syn::Ident {
    let mut variants = variants
        .into_iter()
        .flat_map(resolve_type_name)
        .collect::<Vec<_>>();

    // TODO: assert no duplicates?

    variants.sort_unstable();

    join_idents(variants.into_iter())
}

#[allow(dead_code)] // useful for debugging
pub(crate) fn debug_set<T: ToTokens>(set: IndexSet<T>) -> IndexSet<T> {
    eprint!("[");
    let mut iter = set.iter();
    if let Some(first) = iter.next() {
        eprint!("`{}`", first.into_token_stream());
    }

    for item in iter {
        eprint!(", `{}`", item.into_token_stream());
    }

    eprintln!("]");
    set
}
