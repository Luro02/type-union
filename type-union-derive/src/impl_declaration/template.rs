use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream};
use syn::Token;

use crate::impl_declaration::{Folder, Generics, TypeSolver};
use crate::input::{TypeSignature, TypeUnion};

/// Represents an implementation of a trait for a type union.
#[derive(Debug, Clone)]
pub struct Template {
    self_ty: syn::Type,
    item_impl: syn::ItemImpl,
    generics: Generics,
    trait_: (Option<Token![!]>, syn::Path, Token![for]),
    signature: TypeSignature,
}

impl Template {
    /// Returns the implementations that are built-in to the library.
    pub fn built_in_impls() -> Vec<Self> {
        vec![
            syn::parse_quote! {
                impl<anyA> ::core::iter::Iterator for type_union!(anyA)
                where
                    A: ::core::iter::Iterator,
                {
                    type Item = optional_arg!(Item = <A as ::core::iter::Iterator>::Item);
                    fn next(&mut self) -> ::core::option::Option<Self::Item> {
                        match_type_union!(self: &mut type_union!(anyA) {
                            value: &mut anyA => { value.next().map(|value| Self::Item::from(value)) },
                        })
                    }
                }
            },
            syn::parse_quote! {
                impl<T, anyA> ::core::cmp::PartialEq<T> for type_union!(T | anyA)
                where
                    T: ::core::cmp::PartialEq<T>,
                {
                    fn eq(&self, other: &T) -> bool {
                        match_type_union!(self: &type_union!(T | anyA) {
                            value: &T => value == other,
                            value: &anyA => false
                        })
                    }
                }
            },
            syn::parse_quote! {
                impl<anyA, anyB> ::core::cmp::PartialEq<type_union!(anyA)> for type_union!(anyA | anyB)
                where
                    A: ::core::cmp::PartialEq<A>,
                {
                    fn eq(&self, other: &type_union!(anyA)) -> bool {
                        match_type_union!(self: &type_union!(anyA | anyB) {
                            value: &anyA => other == value,
                            value: &anyB => false
                        })
                    }
                }
            },
            // TODO: PartialEq for not subsets of the other type union?
            syn::parse_quote! {
                impl<anyA> ::core::cmp::Eq for type_union!(anyA)
                where
                    A: ::core::cmp::Eq,
                    Self: ::core::cmp::PartialEq,
                {
                }
            },
            syn::parse_quote! {
                impl<anyT> ::core::fmt::Display for type_union!(anyT)
                where
                    T: ::core::fmt::Display,
                {
                    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                        match_type_union!(self: &type_union!(anyT) {
                            value: &anyT => ::core::write!(f, "{}", value),
                        })
                    }
                }
            },
            // TODO: test this impl
            syn::parse_quote! {
                impl<T, anyA> ::core::convert::From<T> for type_union!(T | anyA) {
                    fn from(value: T) -> Self {
                        __promote!(value: T)
                    }
                }
            },
        ]
    }

    /// Returns the name of the trait that is implemented.
    pub fn trait_path(&self) -> &syn::Path {
        &self.trait_.1
    }

    fn try_apply_args(
        &self,
        mut solver: TypeSolver,
        concrete_signature: &TypeSignature,
    ) -> syn::Result<Vec<syn::ItemImpl>> {
        for (targ, arg) in self.signature.types().zip(concrete_signature.types()) {
            solver.add_type_constraint(targ, arg)?;
        }

        let mut impls = Vec::new();

        for type_mapping in solver.solve()? {
            let mut folder = Folder {
                extra_mapping: HashMap::new(),
                type_mapping,
                errors: Vec::new(),
                trait_path: &self.trait_path(),
                signature: &concrete_signature,
                generics: &self.generics,
            };

            let item_impl = folder.fold_item_impl(self.item_impl.clone());

            // if an error occured, combine all errors into one and return it
            let mut errors = folder.errors.into_iter();
            if let Some(mut error) = errors.next() {
                for err in errors {
                    error.combine(err);
                }

                return Err(error);
            }

            impls.push(item_impl);
        }

        Ok(impls)
    }

    pub fn try_apply<'a, I>(
        &self,
        type_union: &TypeUnion<syn::Type>,
        traits: I,
    ) -> syn::Result<TokenStream>
    where
        I: IntoIterator<Item = &'a TypeSignature>,
    {
        let mut solver = TypeSolver::new(self.generics.clone());

        solver.add_type_constraint(&self.self_ty, &type_union.to_macro_type())?;

        let mut impls = Vec::new();
        for args in traits {
            impls.extend(self.try_apply_args(solver.clone(), args)?);
        }

        Ok(quote!(#(#impls)*))
    }
}

impl Parse for Template {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let item_impl = input.parse::<syn::ItemImpl>()?;

        // first ensure that the self type is a type union (other impls are not yet supported)
        // TODO: ^ issue about this missing feature
        let self_ty = *item_impl.self_ty.clone();
        let generics = Generics::new(item_impl.generics.clone());

        // then check generics for any `anyX` types

        // TODO: check for correct constraints on generics?
        // TODO: check that all generics are used and declared correctly?

        let Some((_bang, path, _for)) = item_impl.trait_.clone() else {
            return Err(syn::Error::new_spanned(
                &item_impl , "at the moment only trait impls are supported"
            ));
        };

        let signature = TypeSignature::try_from(path.clone())?;

        Ok(Self {
            self_ty,
            item_impl,
            trait_: (_bang, path, _for),
            generics,
            signature,
        })
    }
}
