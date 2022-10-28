use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token};

use crate::input::{ImplAttr, TypeUnion};
use crate::utils::resolve_type_name;

pub struct TypeUnionDeclaration {
    impl_attr: Option<ImplAttr>,
    // attrs: Vec<Attribute>,
    _enum_token: Token!(enum),
    ident: Option<Ident>,
    type_union: TypeUnion,
}

impl TypeUnionDeclaration {
    pub fn ident(&self) -> syn::Ident {
        self.type_union.ident()
    }

    pub fn variants(&self) -> impl Iterator<Item = (syn::Ident, syn::Type)> + '_ {
        self.type_union
            .types()
            .flat_map(|ty| resolve_type_name(&ty).map(|ident| (ident, ty.clone())))
    }

    pub fn type_union(&self) -> &TypeUnion {
        &self.type_union
    }

    #[must_use]
    pub fn has_trait<T>(&self, ty: &T) -> bool
    where
        Ident: PartialEq<T>,
    {
        self.impl_attr.as_ref().map_or(false, |i| i.has(ty))
    }
}

impl Parse for TypeUnionDeclaration {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
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
            type_union: {
                let result: TypeUnion = input.parse()?;

                // ensure that the TypeUnion is wrapped in `()`
                result.has_parens_or_err()?;

                result
            },
        })
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
            if self.has_trait(&"PartialEq") {
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
