use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Token;

#[derive(Debug, Clone, Default)]
pub struct ImplAttr {
    _pound_token: Token![#],
    _bracket_token: syn::token::Bracket,
    _impl_token: Token![impl],
    _paren_token: syn::token::Paren,
    /// A list of trait this type wants to implement.
    implements: Punctuated<syn::Path, Token![,]>,
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
            implements: content2.parse_terminated(syn::Path::parse)?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ImplementationArguments {
    args: Punctuated<syn::GenericArgument, Token![,]>,
}

impl ImplementationArguments {
    pub fn types(&self) -> impl Iterator<Item = &syn::Type> + '_ {
        self.args.iter().filter_map(|arg| match arg {
            syn::GenericArgument::Type(ty) => Some(ty),
            _ => None,
        })
    }
}

impl Parse for ImplementationArguments {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let args: syn::AngleBracketedGenericArguments = input.parse()?;
        Ok(Self::from(args))
    }
}

impl From<syn::AngleBracketedGenericArguments> for ImplementationArguments {
    fn from(args: syn::AngleBracketedGenericArguments) -> Self {
        Self { args: args.args }
    }
}

trait PathArgumentsExt {
    fn len(&self) -> usize;
}

impl PathArgumentsExt for syn::PathArguments {
    fn len(&self) -> usize {
        match self {
            syn::PathArguments::None => 0,
            syn::PathArguments::AngleBracketed(args) => args.args.len(),
            syn::PathArguments::Parenthesized(args) => args.inputs.len(),
        }
    }
}

impl ImplAttr {
    const DERIVES: [(&str, &str); 4] = [
        ("Copy", "::core::marker::Copy"),
        ("Clone", "::core::clone::Clone"),
        ("Debug", "::core::fmt::Debug"),
        ("PartialEq", "::core::cmp::PartialEq"),
    ];

    fn traits(&self) -> impl Iterator<Item = &syn::Path> + '_ {
        self.implements.iter()
    }

    fn look_segments_equal(left: &syn::PathSegment, right: &syn::PathSegment) -> bool {
        if left.ident == right.ident && left.arguments.len() == right.arguments.len() {
            match (&left.arguments, &right.arguments) {
                (syn::PathArguments::None, syn::PathArguments::None) => true,
                (
                    syn::PathArguments::AngleBracketed(left),
                    syn::PathArguments::AngleBracketed(right),
                ) => {
                    left.args
                        .iter()
                        .zip(right.args.iter())
                        .all(|(larg, rarg)| match (larg, rarg) {
                            (syn::GenericArgument::Type(lty), syn::GenericArgument::Type(rty)) => {
                                let is_left_macro = matches!(lty, syn::Type::Macro(_));
                                let is_right_macro = matches!(rty, syn::Type::Macro(_));
                                // TODO: improve, expand
                                if is_left_macro {
                                    return is_right_macro;
                                } else if is_right_macro {
                                    return false;
                                } else {
                                    true
                                }
                            }
                            _ => true,
                        })
                }
                (
                    syn::PathArguments::Parenthesized(left),
                    syn::PathArguments::Parenthesized(right),
                ) => left.inputs.len() == right.inputs.len(),
                _ => false,
            }
        } else {
            false
        }
    }

    fn get_traits(&self, ty: &syn::Path) -> impl Iterator<Item = &syn::Path> + '_ {
        // NOTE: this can be replaced with any + iterator
        let mut result = Vec::new();
        for trait_ in self.traits() {
            if trait_ == ty {
                result.push(trait_);
            }
        }

        if !result.is_empty() {
            return result.into_iter();
        }

        // when will two paths be equal?
        // - the segments are the same and the last segment has the same number of arguments
        for trait_ in self.traits() {
            let mut are_equal = true;
            for (segment, other_segment) in
                trait_.segments.iter().rev().zip(ty.segments.iter().rev())
            {
                if !Self::look_segments_equal(segment, other_segment) {
                    are_equal = false;
                    break;
                }
            }

            if are_equal {
                result.push(trait_);
            }
        }

        result.into_iter()
    }

    pub fn get_args(&self, path: &syn::Path) -> impl Iterator<Item = ImplementationArguments> + '_ {
        self.get_traits(path).map(|trait_| {
            if let Some(syn::PathArguments::AngleBracketed(args)) =
                &trait_.segments.last().map(|s| &s.arguments)
            {
                ImplementationArguments::from(args.clone())
            } else {
                ImplementationArguments::default()
            }
        })
    }

    #[must_use]
    pub fn has_trait(&self, ty: &syn::Path) -> bool {
        self.get_traits(ty).next().is_some()
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

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(::core::cmp::PartialEq<T>))
                .collect::<Vec<_>>(),
            vec![&syn::parse_quote!(PartialEq<u8>)]
        );

        assert_eq!(
            attr.get_traits(&syn::parse_quote!(
                ::core::cmp::PartialEq<type_union!(u8 | u16)>
            ))
            .collect::<Vec<_>>(),
            Vec::<&syn::Path>::new(),
        );
    }

    #[test]
    fn test_get_args() {
        let attr: ImplAttr = syn::parse_quote! {
            #[impl(Copy, PartialEq, PartialEq<u8>)]
        };

        assert_eq!(
            attr.get_args(&syn::parse_quote!(Copy)).collect::<Vec<_>>(),
            vec![ImplementationArguments::default()]
        );

        assert_eq!(
            attr.get_args(&syn::parse_quote!(PartialEq))
                .collect::<Vec<_>>(),
            vec![ImplementationArguments::default()]
        );

        assert_eq!(
            attr.get_args(&syn::parse_quote!(::core::cmp::PartialEq<u8>))
                .collect::<Vec<_>>(),
            vec![{
                let args: syn::AngleBracketedGenericArguments = syn::parse_quote!(<u8>);
                ImplementationArguments::from(args)
            }]
        );

        assert_eq!(
            attr.get_args(&syn::parse_quote!(::core::cmp::PartialEq<T>))
                .collect::<Vec<_>>(),
            vec![{
                let args: syn::AngleBracketedGenericArguments = syn::parse_quote!(<u8>);
                ImplementationArguments::from(args)
            }]
        );

        assert_eq!(
            attr.get_args(&syn::parse_quote!(::core::cmp::PartialEq<T, U>))
                .collect::<Vec<_>>(),
            vec![]
        );
    }

    #[test]
    fn test_get_args_conflict() {
        let attr: ImplAttr = syn::parse_quote! {
            #[impl(PartialEq<u8>, PartialEq<u64>, PartialEq<String>, PartialEq<type_union!(u8 | u64)>)]
        };

        assert_eq!(
            attr.get_args(&syn::parse_quote!(::core::cmp::PartialEq<T>))
                .collect::<Vec<_>>(),
            vec![
                syn::parse_quote!(<u8>),
                syn::parse_quote!(<u64>),
                syn::parse_quote!(<String>),
            ]
        );
    }
}
