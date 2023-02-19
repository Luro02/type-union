use syn::punctuated::Punctuated;
use syn::{GenericParam, Token};

use crate::impl_declaration::{EitherType, GenericType};
use crate::input::TypeUnion;

#[derive(Debug, Clone)]
pub struct Generics {
    // TODO: add constraints?
    generics: Punctuated<syn::GenericParam, Token![,]>,
}

impl Generics {
    pub fn new(generics: syn::Generics) -> Self {
        Self {
            generics: generics.params,
        }
    }

    pub fn get(&self, path: &syn::Path) -> Option<GenericType> {
        self.generics.iter().find_map(|item| {
            if let syn::GenericParam::Type(ty) = item {
                if Some(&ty.ident) == path.get_ident() {
                    return Some(GenericType::from(ty.clone()));
                }
            }

            None
        })
    }

    // TODO: what about TypeUnion<syn::Type> -> TypeUnion<EitherType>?
    pub fn resolve(&self, type_union: TypeUnion<EitherType>) -> TypeUnion<EitherType> {
        type_union.map_types(|ty| {
            if let EitherType::Concrete(ty) = ty {
                if let syn::Type::Path(syn::TypePath { path, .. }) = *ty.clone() {
                    if let Some(type_param) = self.generics.iter().find_map(|p| {
                        if let GenericParam::Type(param) = p {
                            if Some(&param.ident) == path.get_ident() {
                                return Some(param);
                            }
                        }

                        None
                    }) {
                        return EitherType::Generic(GenericType::from(type_param.clone()));
                    }
                }

                EitherType::Concrete(ty)
            } else {
                ty
            }
        })
    }
}
