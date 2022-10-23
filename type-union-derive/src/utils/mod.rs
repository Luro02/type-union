use convert_case::{Case, Casing};
use proc_macro2::Span;
use quote::quote;
use syn::{Ident, Type};

mod syn_ext;

pub use syn_ext::*;

pub fn assert_ident_is_type(ident: &syn::Ident, ty: &syn::Type) -> syn::Expr {
    syn::parse2(quote! {
        {
            fn _assert_is_ty(v: #ty) -> #ty { v }
            _assert_is_ty(#ident)
        }
    })
    .expect("macro is broken")
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
        .flat_map(|ty| resolve_type_name(&ty))
        .collect::<Vec<_>>();

    // TODO: assert no duplicates?

    variants.sort_unstable();

    join_idents(variants.into_iter())
}
