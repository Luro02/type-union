use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Ident, ItemImpl, Token, Type};

use crate::input::{TypeUnion, TypeUnionDeclaration};
use crate::utils::unique_pairs;

enum Definition {
    Declaration(TypeUnionDeclaration),
    Impl(ItemImpl),
}

impl Parse for Definition {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        if input.peek(Token![impl]) {
            Ok(Self::Impl(input.parse()?))
        } else {
            Ok(Self::Declaration(input.parse()?))
        }
    }
}

impl ToTokens for Definition {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Declaration(decl) => decl.to_tokens(tokens),
            Self::Impl(impl_) => impl_.to_tokens(tokens),
        }
    }
}

pub struct DefineTypeUnion {
    punctuated: Punctuated<Definition, Token![;]>,
}

impl DefineTypeUnion {
    pub fn declarations(&self) -> impl Iterator<Item = &TypeUnionDeclaration> + '_ {
        self.punctuated.iter().filter_map(|d| match d {
            Definition::Declaration(decl) => Some(decl),
            _ => None,
        })
    }

    /// Returns all implementations associated with the given type.
    pub fn impls_of<'a, 'b, T>(&'a self, sty: &'b T) -> impl Iterator<Item = &'a ItemImpl> + 'a
    where
        'b: 'a,
        Type: PartialEq<T>,
    {
        self.punctuated
            .iter()
            .filter_map(|d| match d {
                Definition::Impl(v) => Some(v),
                _ => None,
            })
            .filter(move |&i| {
                let ty: &Type = &*i.self_ty;
                ty == sty
            })
    }

    pub fn impls(&self) -> impl Iterator<Item = &ItemImpl> + '_ {
        self.punctuated.iter().filter_map(|d| match d {
            Definition::Impl(v) => Some(v),
            _ => None,
        })
    }
}

impl Parse for DefineTypeUnion {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            punctuated: input.parse_terminated(Definition::parse)?,
        })
    }
}

#[must_use]
fn generate_partial_eq_impl(
    a: &TypeUnionDeclaration,
    b: &TypeUnionDeclaration,
) -> proc_macro2::TokenStream {
    let mut mapping: Vec<((Ident, Ident), Type)> = Vec::new();

    for (variant, ty) in a.variants() {
        for (other_variant, other_ty) in b.variants() {
            if &ty == &other_ty {
                mapping.push(((variant.clone(), other_variant), ty.clone()));
            }
        }
    }

    if !mapping.is_empty() {
        let a_ident = a.ident();
        let b_ident = b.ident();

        let a_variants = mapping.iter().map(|((a, _), _)| a);
        let b_variants = mapping.iter().map(|((_, b), _)| b);

        return quote! {
            impl ::core::cmp::PartialEq<#a_ident> for #b_ident {
                fn eq(&self, other: &#a_ident) -> bool {
                    use #a_ident as ThisB;
                    use #b_ident as ThisA;

                    match (self, other) {
                        #(
                            (
                                ThisA::#a_variants (a),
                                ThisB::#b_variants (b)
                            ) => {
                                a.eq(b)
                            }
                        )*
                        _ => false,
                    }
                }
            }

            impl PartialEq<#b_ident> for #a_ident {
                fn eq(&self, other: &#b_ident) -> bool {
                    other.eq(self)
                }
            }
        };
    }

    quote!()
}

impl ToTokens for DefineTypeUnion {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut with_partialeqs = Vec::new();
        for declaration in self.declarations() {
            if declaration.has_trait("PartialEq") {
                with_partialeqs.push(declaration);
            }

            tokens.append_all(quote!(
                #declaration
            ));
        }

        for (a, b) in unique_pairs(with_partialeqs) {
            tokens.append_all(generate_partial_eq_impl(a, b));
        }

        for impl_ in self.impls() {
            tokens.append_all(quote!(
                #impl_
            ));
        }
    }
}
