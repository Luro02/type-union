use proc_macro2::TokenStream;
use quote::quote;
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream};

use crate::impl_declaration::{Folder, Generics, MacroResolver, TypeSolver};
use crate::input::{ImplAttr, TypeSignature, TypeUnion};
use crate::utils::{Context, LooksLike};

/// Represents an implementation of a trait for a type union.
#[derive(Debug, Clone)]
pub struct Template {
    self_ty: syn::Type,
    item_impl: syn::ItemImpl,
    generics: Generics,
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

    #[must_use]
    fn context(&self) -> &dyn Context {
        &self.generics
    }

    /// Checks if this template overrides the provided template.
    #[must_use]
    pub fn does_override(&self, other: &Self) -> bool {
        self.signature
            .looks_like_with(&other.signature, &(self.context(), &other.generics))
    }

    /// Check if the template can be applied to the given type union.
    #[must_use]
    pub fn does_apply(&self, _type_union: &TypeUnion<syn::Type>, impl_attr: &ImplAttr) -> bool {
        impl_attr.has_trait_with(&self.signature, self.context())
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
            let mut folder = Folder::new(type_mapping, &concrete_signature, &self.generics);

            let item_impl = folder.fold_item_impl(self.item_impl.clone());

            // if an error occured while folding, emit it:
            folder.error_if_present()?;

            impls.push(item_impl);
        }

        Ok(impls)
    }

    pub fn try_apply(
        &self,
        // the type union for which the impl should be generated
        type_union: &TypeUnion<syn::Type>,
        impl_attr: &ImplAttr,
    ) -> syn::Result<TokenStream> {
        let mut impls = Vec::new();

        for signature in impl_attr.get_traits_with(&self.signature, self.context()) {
            let mut solver = TypeSolver::new(self.generics.clone());

            let declaration = signature
                .self_ty()
                .cloned()
                .unwrap_or_else(|| type_union.to_macro_type());
            solver.add_type_constraint(&self.self_ty, &declaration)?;

            impls.extend(self.try_apply_args(solver, signature)?);
        }

        // expand macros from this crate, so that they do not have to be imported.
        let mut macro_resolver = MacroResolver::default();
        let mut result = Vec::new();
        for item in impls {
            result.push(macro_resolver.fold_item_impl(item));
        }

        let identity_macro = macro_resolver.identity_macro();

        Ok(quote!( #identity_macro #(#result)*))
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

        let Some((_, trait_path, _)) = item_impl.trait_.clone() else {
            return Err(syn::Error::new_spanned(
                &item_impl , "at the moment only trait impls are supported"
            ));
        };

        let signature = TypeSignature::new(trait_path, Some(self_ty.clone()))?;

        Ok(Self {
            self_ty,
            item_impl,
            generics,
            signature,
        })
    }
}
