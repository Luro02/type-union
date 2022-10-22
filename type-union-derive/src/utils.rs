use convert_case::{Case, Casing};
use proc_macro2::Span;
use quote::quote;
use syn::parse::{Parse, ParseBuffer, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Ident, Type};

pub fn assert_ident_is_type(ident: &syn::Ident, ty: &syn::Type) -> syn::Expr {
    syn::parse2(quote! {
        {
            fn _assert_is_ty(v: #ty) -> #ty { v }
            _assert_is_ty(#ident)
        }
    })
    .expect("macro is broken")
}

pub trait ParseStreamExt<'a> {
    /// # Why does this exist?
    ///
    /// <https://github.com/dtolnay/syn/issues/697>
    fn parse_terminated2<T, P: Parse, F>(&self, parser: F) -> syn::Result<Punctuated<T, P>>
    where
        for<'b> F: FnMut(ParseStream<'b>) -> syn::Result<T>;
}

impl<'a> ParseStreamExt<'a> for ParseBuffer<'a> {
    fn parse_terminated2<T, P: Parse, F>(&self, parser: F) -> syn::Result<Punctuated<T, P>>
    where
        for<'b> F: FnMut(ParseStream<'b>) -> syn::Result<T>,
    {
        <ParseStream<'_> as ParseStreamExt<'_>>::parse_terminated2(&self, parser)
    }
}

impl<'a> ParseStreamExt<'a> for ParseStream<'a> {
    fn parse_terminated2<T, P: Parse, F>(&self, mut parser: F) -> syn::Result<Punctuated<T, P>>
    where
        for<'b> F: FnMut(ParseStream<'b>) -> syn::Result<T>,
    {
        // NOTE: source code has been copied from Punctuated::parse_terminated_with
        let mut punctuated = Punctuated::new();

        loop {
            if self.is_empty() {
                break;
            }

            let value = parser(self)?;
            punctuated.push_value(value);
            if self.is_empty() {
                break;
            }
            let punct = self.parse()?;
            punctuated.push_punct(punct);
        }

        Ok(punctuated)
    }
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
