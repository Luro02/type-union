use std::mem;

use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Token;

use crate::input::TypeSignature;
use crate::utils::{Context, ErrorExt, LooksLike};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ImplAttr {
    _pound_token: Token![#],
    _bracket_token: syn::token::Bracket,
    _impl_token: Token![impl],
    _paren_token: syn::token::Paren,
    /// A list of traits this type wants to implement.
    implements: Punctuated<TypeSignature, Token![,]>,
}

impl Parse for ImplAttr {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        let content2;
        Ok(Self {
            _pound_token: input.parse()?,
            _bracket_token: syn::bracketed!(content in input),
            _impl_token: content.parse()?,
            _paren_token: syn::parenthesized!(content2 in content),
            implements: content2.parse_terminated(TypeSignature::parse)?,
        })
    }
}

impl ImplAttr {
    const DERIVES: [(&str, &str); 4] = [
        ("Copy", "::core::marker::Copy"),
        ("Clone", "::core::clone::Clone"),
        ("Debug", "::core::fmt::Debug"),
        ("PartialEq", "::core::cmp::PartialEq"),
    ];

    fn resolve(&mut self, current_ty: syn::Type, new_ty: syn::Type) -> syn::Result<()> {
        let mut errors = Vec::new();
        for item in &mut self.implements {
            if let Err(e) = item.resolve(current_ty.clone(), new_ty.clone()) {
                errors.push(e);
            }
        }

        let mut errors = errors.into_iter();
        if let Some(first) = errors.next() {
            return Err(first.combine_all(errors));
        }

        self.implements = mem::take(&mut self.implements)
            .into_iter()
            .flat_map(|sig| sig.unfold())
            .collect();

        Ok(())
    }

    pub fn resolve_self(&mut self, new_ty: syn::Type) -> syn::Result<()> {
        self.resolve(syn::parse_quote!(Self), new_ty)
    }

    fn get_traits<'a, 'b>(
        &'a self,
        path: &'b syn::Path,
    ) -> impl Iterator<Item = &'a TypeSignature> + 'b
    where
        'a: 'b,
    {
        self.implements
            .iter()
            .filter(move |item| item.looks_like(path))
    }

    pub fn get_traits_with<'a, 'b>(
        &'a self,
        path: &'b TypeSignature,
        ctx: &'b dyn Context,
    ) -> impl Iterator<Item = &'a TypeSignature> + 'b
    where
        'a: 'b,
    {
        self.implements
            .iter()
            .filter(move |item| item.looks_like_with(path, ctx))
    }

    #[must_use]
    fn has_trait(&self, ty: &syn::Path) -> bool {
        self.get_traits(ty).next().is_some()
    }

    #[must_use]
    pub fn has_trait_with(&self, ty: &TypeSignature, ctx: &dyn Context) -> bool {
        self.get_traits_with(ty, ctx).next().is_some()
    }

    #[must_use]
    pub fn derives(&self) -> Option<syn::Attribute> {
        let mut traits: Punctuated<syn::Path, Token![,]> = Punctuated::new();

        for (trait_name, path) in Self::DERIVES {
            let name = syn::parse_str::<syn::Path>(trait_name).unwrap();

            if self.has_trait(&name) {
                traits.push(syn::parse_str::<syn::Path>(path).unwrap());
            }
        }

        if traits.is_empty() {
            return None;
        }

        Some(syn::Attribute {
            pound_token: self._pound_token,
            style: syn::AttrStyle::Outer,
            bracket_token: self._bracket_token,
            path: syn::parse_quote!(derive),
            tokens: quote!((#traits)),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_resolve_self_has_trait() {
        let mut impl_attr: ImplAttr = syn::parse_quote! {
            #[impl(Copy)]
        };

        assert!(impl_attr.has_trait(&syn::parse_quote!(Copy)));

        impl_attr
            .resolve_self(syn::parse_quote!(type_union!(u8 | u16)))
            .unwrap();

        assert!(impl_attr.has_trait(&syn::parse_quote!(Copy)));
    }

    #[test]
    fn test_derive() {
        let impl_attr: ImplAttr = syn::parse_quote! {
            #[impl(Copy, Clone, Debug, PartialEq)]
        };

        assert_eq!(
            impl_attr.derives(),
            Some(syn::parse_quote! {
                #[derive(::core::marker::Copy, ::core::clone::Clone, ::core::fmt::Debug, ::core::cmp::PartialEq)]
            })
        );
    }

    #[test]
    fn test_get_traits() {
        let attr: ImplAttr = syn::parse_quote! {
            #[impl(Copy, PartialEq, PartialEq<u8>)]
        };

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(Copy))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(Copy)]
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(::core::marker::Copy))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(Copy)]
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(::core::cmp::PartialEq))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(PartialEq)]
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(::core::cmp::PartialEq<u8>))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(PartialEq<u8>)]
        );

        let ctx: syn::Generics = syn::parse_quote!(<T>);
        assert_eq!(
            attr.get_traits_with(&syn::parse_quote!(::core::cmp::PartialEq<T>), &ctx)
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(PartialEq<u8>)]
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(::core::cmp::PartialEq<T>))
                .collect::<Vec<_>>(),
            Vec::<&TypeSignature>::new(),
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(
                ::core::cmp::PartialEq<type_union!(u8 | u16)>
            ))
            .collect::<Vec<_>>(),
            Vec::<&TypeSignature>::new(),
        );
    }

    #[test]
    fn test_get_args() {
        let attr: ImplAttr = syn::parse_quote! {
            #[impl(Copy, PartialEq, PartialEq<u8>)]
        };

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(Copy))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(Copy)]
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(PartialEq))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(PartialEq)]
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(::core::cmp::PartialEq<u8>))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(PartialEq<u8>)]
        );

        let ctx: syn::Generics = syn::parse_quote!(<T, U>);
        assert_eq!(
            attr.get_traits_with(&syn::parse_quote!(::core::cmp::PartialEq<T, U>), &ctx)
                .collect::<Vec<_>>(),
            Vec::<&TypeSignature>::new()
        );
    }

    #[test]
    fn test_get_args_conflict() {
        let attr: ImplAttr = syn::parse_quote! {
            #[impl(PartialEq<u8>, PartialEq<u64>, PartialEq<String>, PartialEq<type_union!(u8 | u64)>)]
        };

        let ctx: syn::Generics = syn::parse_quote!(<T>);
        assert_eq!(
            attr.get_traits_with(&syn::parse_quote!(::core::cmp::PartialEq<T>), &ctx)
                .collect::<Vec<_>>(),
            vec![
                &syn::parse_quote!(PartialEq<u8>),
                &syn::parse_quote!(PartialEq<u64>),
                &syn::parse_quote!(PartialEq<String>),
            ]
        );
    }

    #[test]
    fn test_trait_binding() {
        let attr: ImplAttr = syn::parse_quote! {
            #[impl(Iterator<Item = u8>)]
        };

        assert_eq!(
            attr.has_trait(&syn::parse_quote!(::core::iter::Iterator)),
            true
        );
    }

    // TODO: better fn name
    #[test]
    fn test_buggy() {
        let mut impl_attr: ImplAttr = syn::parse_quote!(#[impl(TryFrom<Self> for MyWrapper<Self>)]);
        impl_attr
            .resolve(
                syn::parse_quote!(Self),
                syn::parse_quote!(type_union!(u8 | u16 | u32)),
            )
            .unwrap();
        let signature = TypeSignature::new(
            syn::parse_quote!(TryFrom<type_union!(..A)>),
            Some(syn::parse_quote!(MyWrapper<type_union!(..A)>)),
        )
        .unwrap();

        let ctx: syn::Generics = syn::parse_quote!(<anyA>);

        assert_eq!(impl_attr.has_trait_with(&signature, &ctx), true);

        let mut impl_attr: ImplAttr = syn::parse_quote!(#[impl(Display)]);
        let ctx: syn::Generics = syn::parse_quote!(<anyT>);
        let signature = TypeSignature::new(
            syn::parse_quote!(::core::fmt::Display),
            Some(syn::parse_quote!(type_union!(anyT))),
        )
        .unwrap();

        impl_attr
            .resolve(
                syn::parse_quote!(Self),
                syn::parse_quote!(type_union!(u8 | u16 | u32)),
            )
            .unwrap();

        assert_eq!(impl_attr.has_trait_with(&signature, &ctx), true);
    }
}
