use indexmap::IndexMap;

use crate::impl_declaration::{EitherType, GenericType, Variadic};
use crate::input::TypeUnion;

/// Represents all declared generics and their bounds.
#[derive(Debug, Clone)]
pub struct Generics {
    // TODO: add constraints?
    variadics: IndexMap<Variadic, syn::TypeParam>,
    generics: IndexMap<syn::Ident, syn::TypeParam>,
    // other generic params like lifetimes and const generics
    _others: Vec<syn::GenericParam>,
}

impl Generics {
    #[must_use]
    pub fn new(value: syn::Generics) -> Self {
        let mut variadics = IndexMap::new();
        let mut generics = IndexMap::new();
        let mut others = Vec::new();

        for param in value.params {
            if let syn::GenericParam::Type(ty) = param {
                if let Ok(variadic) = Variadic::try_from(ty.ident.clone()) {
                    variadics.insert(variadic, ty);
                } else {
                    generics.insert(ty.ident.clone(), ty);
                }
            } else {
                others.push(param);
            }
        }

        Self {
            variadics,
            generics,
            _others: others,
        }
    }

    pub fn get_generic(&self, path: &syn::Path) -> Option<GenericType> {
        let Some(ident) = path.get_ident() else {
            return None;
        };

        self.generics
            .get(ident)
            .map(|ty| GenericType::from(ty.clone()))
    }

    /// Returns the variadic type if the given type is a variadic type and present in the generics.
    #[must_use]
    pub fn get_variadic(&self, path: &syn::Path) -> Option<Variadic> {
        if let Some(ident) = path.get_ident() {
            // NOTE: one can not directly call get on the hashmap with an ident, because then
            //       .get(anyA) would fail.
            if let Ok(variadic) = Variadic::try_from(ident.clone()) {
                if self.variadics.get(&variadic).is_some() {
                    return Some(variadic);
                }
            }

            if let Some((variadic, _)) = self.variadics.get_key_value(ident) {
                return Some(variadic.clone());
            }
        }

        None
    }

    pub fn resolve(&self, type_union: TypeUnion<EitherType>) -> TypeUnion<EitherType> {
        type_union.map_types(|ty| {
            let EitherType::Concrete(ty) = ty else { return ty; };

            if let syn::Type::Path(syn::TypePath { path, .. }) = *ty.clone() {
                if let Some(type_param) = self.get_generic(&path) {
                    return EitherType::Generic(GenericType::from(type_param.clone()));
                }

                if let Some(variadic) = self.get_variadic(&path) {
                    return EitherType::Variadic(variadic);
                }
            }

            EitherType::Concrete(ty)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_get_variadic() {
        let generics = Generics::new(syn::parse_quote! {
            <A, B, anyC>
        });

        let any_c: Variadic = syn::parse_quote!(anyC);

        assert_eq!(
            generics.get_variadic(&syn::parse_quote! { anyC }),
            Some(any_c.clone())
        );
        assert_eq!(generics.get_variadic(&syn::parse_quote! { C }), Some(any_c));
        assert_eq!(generics.get_variadic(&syn::parse_quote! { A }), None);
        assert_eq!(generics.get_variadic(&syn::parse_quote! { B }), None);
    }
}
