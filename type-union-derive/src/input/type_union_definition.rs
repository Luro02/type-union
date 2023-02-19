use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token};

use crate::input::{ImplAttr, TypeUnion};
use crate::utils::resolve_type_name;

pub struct TypeUnionDefinition {
    impl_attr: ImplAttr,
    // attrs: Vec<Attribute>,
    _enum_token: Token!(enum),
    ident: Option<Ident>,
    type_union: TypeUnion<syn::Type>,
}

impl TypeUnionDefinition {
    #[must_use]
    pub fn impl_attr(&self) -> &ImplAttr {
        &self.impl_attr
    }

    fn path(&self) -> syn::Path {
        self.type_union.path()
    }

    fn variants(&self) -> impl Iterator<Item = (syn::Ident, syn::Type)> + '_ {
        self.type_union
            .iter_types()
            .flat_map(|ty| resolve_type_name(ty).map(|ident| (ident, ty.clone())))
    }

    pub fn type_union(&self) -> &TypeUnion<syn::Type> {
        &self.type_union
    }
}

impl Parse for TypeUnionDefinition {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            impl_attr: {
                if input.peek(Token![#]) {
                    input.parse()?
                } else {
                    ImplAttr::default()
                }
            },
            _enum_token: input.parse()?,
            ident: input.parse()?,
            type_union: {
                let result: TypeUnion<syn::Type> = input.parse()?;

                // ensure that the TypeUnion is wrapped in `()`
                result.has_parens_or_err()?;

                result
            },
        })
    }
}

impl ToTokens for TypeUnionDefinition {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = self.path();

        let (variant_names, variant_types): (Vec<_>, Vec<_>) = self.variants().unzip();

        let derives = {
            if let Some(attr) = self.impl_attr().derives() {
                quote!(#attr)
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
        ));
    }
}
