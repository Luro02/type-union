use convert_case::{Case, Casing};
use proc_macro2::Span;
use syn::spanned::Spanned;
use syn::{Ident, Type};

mod generator;
mod syn_ext;

pub use generator::*;
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

pub fn unique_product<V, T>(values: V, size: usize) -> Vec<Vec<T>>
where
    T: Clone,
    V: AsRef<[T]>,
{
    let values = values.as_ref();
    if size == 0 {
        return vec![];
    } else if size == 1 {
        return values.iter().cloned().map(|v| vec![v]).collect();
    }

    let mut result = Vec::new();

    for (index, a) in values.iter().enumerate() {
        for b in unique_product(&values[(index + 1)..], size - 1) {
            let mut c = vec![a.clone()];
            c.extend(b);
            result.push(c);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    // TODO: write more tests?
    fn test_unique_product() {
        assert_eq!(
            unique_product(vec!['a', 'b', 'c'], 2),
            vec![vec!['a', 'b'], vec!['a', 'c'], vec!['b', 'c']]
        );

        assert_eq!(
            unique_product(vec!['a', 'b', 'c'], 3),
            vec![vec!['a', 'b', 'c']]
        );

        assert_eq!(
            unique_product(vec!['a', 'b', 'c'], 1),
            vec![vec!['a'], vec!['b'], vec!['c']]
        );
        assert_eq!(unique_product(&['a', 'b', 'c'], 0), Vec::<Vec<char>>::new());
        assert_eq!(
            unique_product(&Vec::<char>::new(), 0),
            Vec::<Vec<char>>::new()
        );

        assert_eq!(
            unique_product(["usize", "u8", "u16", "u32"], 2),
            vec![
                vec!["usize", "u8"],
                vec!["usize", "u16"],
                vec!["usize", "u32"],
                vec!["u8", "u16"],
                vec!["u8", "u32"],
                vec!["u16", "u32"],
            ]
        );
    }

    #[test]
    fn test_unique_product_against_itertools() {
        use itertools::Itertools;

        let source = vec!['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'];

        for size in 1..=source.len() {
            let expected = source
                .iter()
                .cloned()
                .combinations(size)
                .collect::<Vec<_>>();
            let actual = unique_product(&source, size);

            assert_eq!(actual, expected);
        }
    }
}
