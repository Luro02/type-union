use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{bracketed, parenthesized, Attribute, Ident, Path, Token, Type};

use crate::utils::{resolve_type_name, resolve_type_union_name, ParseStreamExt};

pub struct ImplAttr {
    _pound_token: Token![#],
    _bracket_token: syn::token::Bracket,
    _impl_token: Token![impl],
    _paren_token: syn::token::Paren,
    traits: Punctuated<Ident, Token![,]>,
}

impl Parse for ImplAttr {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let brack_content;
        let paren_content;
        Ok(Self {
            _pound_token: input.parse()?,
            _bracket_token: bracketed!(brack_content in input),
            _impl_token: brack_content.parse()?,
            _paren_token: parenthesized!(paren_content in brack_content),
            traits: paren_content.parse_terminated(Ident::parse)?,
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

    pub fn traits(&self) -> impl Iterator<Item = &Ident> + '_ {
        self.traits.iter()
    }

    #[must_use]
    pub fn has<T>(&self, ty: T) -> bool
    where
        Ident: PartialEq<T>,
    {
        self.traits().any(|t| t == &ty)
    }

    #[must_use]
    pub fn derives(&self) -> Attribute {
        let mut traits: Punctuated<Path, Token![,]> = Punctuated::new();

        for (trait_name, path) in Self::DERIVES {
            if self.has(trait_name) {
                traits.push(syn::parse_str::<Path>(path).unwrap());
            }
        }

        Attribute {
            pound_token: self._pound_token,
            style: syn::AttrStyle::Outer,
            bracket_token: self._bracket_token,
            path: syn::parse_str("derive").unwrap(),
            tokens: quote!((#traits)),
        }
    }
}

pub struct TypeUnionDeclaration {
    impl_attr: Option<ImplAttr>,
    // attrs: Vec<Attribute>,
    _enum_token: Token!(enum),
    ident: Option<Ident>,
    _paren_token: syn::token::Paren,
    variants: Punctuated<Type, Token![|]>,
}

fn parse_type(visited: &mut HashSet<Ident>, input: ParseStream<'_>) -> syn::Result<Type> {
    let ty = input.parse::<Type>()?;
    let ident = resolve_type_name(&ty).ok_or_else(|| input.error("could not resolve type name"))?;

    if visited.contains(&ident) {
        return Err(input.error("duplicate type name"));
    }

    Ok(ty)
}

impl Parse for TypeUnionDeclaration {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut visited = HashSet::new();
        let content;
        Ok(Self {
            impl_attr: {
                if input.peek(Token![#]) {
                    Some(input.parse()?)
                } else {
                    None
                }
            },
            _enum_token: input.parse()?,
            ident: input.parse()?,
            _paren_token: parenthesized!(content in input),
            variants: content.parse_terminated2(|input| parse_type(&mut visited, input))?,
        })
    }
}

impl TypeUnionDeclaration {
    pub fn ident(&self) -> syn::Ident {
        resolve_type_union_name(self.variants.iter())
    }

    pub fn variants(&self) -> impl Iterator<Item = (syn::Ident, syn::Type)> + '_ {
        self.variants
            .iter()
            .flat_map(|ty| resolve_type_name(&ty).map(|ident| (ident, ty.clone())))
    }

    #[must_use]
    pub fn has_trait<T>(&self, ty: T) -> bool
    where
        Ident: PartialEq<T>,
    {
        self.impl_attr.as_ref().map_or(false, |i| i.has(ty))
    }
}

impl ToTokens for TypeUnionDeclaration {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = self.ident();

        let (variant_names, variant_types): (Vec<_>, Vec<_>) = self.variants().unzip();

        let derives = {
            if let Some(attr) = &self.impl_attr {
                let d = attr.derives();
                quote!(#d)
            } else {
                quote!()
            }
        };

        let optional_partial_eq = {
            if self.has_trait("PartialEq") {
                quote! {
                    #(
                        impl PartialEq<#variant_types> for #ident {
                            fn eq(&self, other: &#variant_types) -> bool {
                                if let Self::#variant_names(value) = self {
                                    value.eq(other)
                                } else {
                                    false
                                }
                            }
                        }
                    )*
                }
            } else {
                quote!()
            }
        };

        tokens.append_all(quote!(
            #derives
            #[allow(non_camel_case_types)]
            pub enum #ident {
                #(#variant_names(#variant_types)),*
            }

            #(
                impl From<#variant_types> for #ident {
                    fn from(value: #variant_types) -> Self {
                        Self::#variant_names(value)
                    }
                }
            )*

            #optional_partial_eq
        ));
    }
}

pub struct DefineTypeUnion {
    punctuated: Punctuated<TypeUnionDeclaration, Token![;]>,
}

impl Parse for DefineTypeUnion {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            punctuated: input.parse_terminated(TypeUnionDeclaration::parse)?,
        })
    }
}

impl ToTokens for DefineTypeUnion {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for declaration in self.punctuated.iter() {
            tokens.append_all(quote!(
                #declaration
            ));
        }
    }
}
