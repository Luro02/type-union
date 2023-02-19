use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream};
use syn::Token;

use crate::impl_declaration::{EitherType, Folder, Generics, TypeSolver};
use crate::input::{ImplementationArguments, TypeUnion, TypeUnionDefinition};

/// Represents an implementation of a trait for a type union.
#[derive(Debug, Clone)]
pub struct Template {
    self_ty: TypeUnion<EitherType>,
    item_impl: syn::ItemImpl,
    generics: Generics,
    trait_: (Option<Token![!]>, syn::Path, Token![for]),
    template_args: ImplementationArguments,
}

impl Template {
    /// Returns the implementations that are built-in to the library.
    pub fn built_in_impls() -> Vec<Self> {
        vec![
            syn::parse_quote! {
                impl<T, anyA> ::core::cmp::PartialEq<T> for type_union!(T | anyA)
                where
                    T: ::core::cmp::PartialEq<T>,
                {
                    fn eq(&self, other: &T) -> bool {
                        let s = self;
                        match_type_union!(s: &type_union!(T | anyA) {
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
                        let s = self;
                        match_type_union!(s: &type_union!(anyA | anyB) {
                            value: &anyA => other == value,
                            value: &anyB => false
                        })
                    }
                }
            },
            syn::parse_quote! {
                impl<anyA, anyB, anyC> ::core::cmp::PartialEq<type_union!(anyA | anyC)> for type_union!(anyA | anyB)
                where
                    A: ::core::cmp::PartialEq<A>,
                {
                    fn eq(&self, other: &type_union!(anyA | anyC)) -> bool {
                        let s = self;
                        match_type_union!(s: &type_union!(anyA | anyB) {
                            value: &anyA => other == value,
                            value: &anyB => false
                        })
                    }
                }
            },
            syn::parse_quote! {
                impl<anyT> ::core::fmt::Display for type_union!(anyT)
                where
                    T: ::core::fmt::Display,
                {
                    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                        let s = self;
                        match_type_union!(s: &type_union!(anyT) {
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
        

        &self.trait_.1 as _
    }

    fn try_apply_args(
        &self,
        declaration: &TypeUnionDefinition,
        mut solver: TypeSolver,
        args: ImplementationArguments,
    ) -> syn::Result<Vec<syn::ItemImpl>> {
        for (targ, arg) in self.template_args.types().zip(args.types()) {
            solver.add_type_constraint(targ, arg)?;
        }

        let mut impls = Vec::new();

        for type_mapping in solver.solve()? {
            let mut folder = Folder {
                extra_mapping: HashMap::new(),
                type_mapping,
                declaration,
                self_ty: &self.self_ty,
                errors: Vec::new(),
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

    pub fn try_apply(&self, declaration: &TypeUnionDefinition) -> syn::Result<TokenStream> {
        let mut solver = TypeSolver::new(self.generics.clone());

        solver.add_union_constraint(&self.self_ty, declaration.type_union())?;

        let mut impls = Vec::new();
        for args in declaration.impl_attr().get_args(self.trait_path()) {
            impls.extend(self.try_apply_args(declaration, solver.clone(), args)?);
        }

        Ok(quote!(#(#impls)*))
    }

    fn parse_self_ty_union(
        item_impl: &syn::ItemImpl,
    ) -> syn::Result<(TypeUnion<EitherType>, Generics)> {
        let self_ty = &item_impl.self_ty;

        let generics = Generics::new(item_impl.generics.clone());

        if let syn::Type::Macro(syn::TypeMacro { mac }) = self_ty.as_ref() {
            if mac.path.is_ident("type_union") {
                let tokens = mac.tokens.clone();
                let mut type_union = syn::parse2::<TypeUnion<EitherType>>(tokens)?;

                let mut found_wildcard = false;
                for ty in type_union.iter_types() {
                    if ty.is_wildcard() {
                        found_wildcard = true;
                    } else if found_wildcard {
                        // TODO: parse everything and emit a multi-span containing all
                        // concrete types that violate this rule
                        return Err(syn::Error::new_spanned(
                            ty,
                            "first list all concrete types, then all wildcards",
                        ));
                    }
                }

                // TODO: what about lifetimes and const generics?
                type_union = type_union.map_types(|ty| {
                    if let EitherType::Concrete(ty) = &ty {
                        if let syn::Type::Path(ty_path) = ty.as_ref() {
                            if let Some(generic) = generics.get(&ty_path.path) {
                                return EitherType::Generic(generic);
                            }
                        }
                    }

                    ty
                });

                return Ok((type_union, generics));
            }
        }

        Err(syn::Error::new_spanned(
            self_ty,
            "expected type union for self type",
        ))
    }
}

impl Parse for Template {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let item_impl = input.parse::<syn::ItemImpl>()?;

        // first ensure that the self type is a type union (other impls are not yet supported)
        // TODO: ^ issue about this missing feature
        let (self_ty, generics) = Self::parse_self_ty_union(&item_impl)?;

        // then check generics for any `anyX` types

        // TODO: check for correct constraints on generics?
        // TODO: check that all generics are used and declared correctly?

        let Some((_bang, path, _for)) = item_impl.trait_.clone() else {
            return Err(syn::Error::new_spanned(
                &item_impl , "at the moment only trait impls are supported"
            ));
        };

        let template_args = {
            match path.segments.last().unwrap().arguments.clone() {
                syn::PathArguments::AngleBracketed(args) => {
                    Some(ImplementationArguments::from(args))
                }
                syn::PathArguments::Parenthesized(template_args) => {
                    return Err(syn::Error::new_spanned(
                        template_args,
                        "expected angle bracketed arguments",
                    ));
                }
                _ => None,
            }
        };

        Ok(Self {
            self_ty,
            item_impl,
            trait_: (_bang, path, _for),
            generics,
            template_args: template_args.unwrap_or_default(),
        })
    }
}
