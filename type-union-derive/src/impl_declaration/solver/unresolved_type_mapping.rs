use std::hash::Hash;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;

use super::{IndexMapExt, TypeMapping};
use crate::impl_declaration::{GenericType, Variadic};

#[derive(Clone, Debug, PartialEq)]
pub struct UnresolvedTypeMapping {
    generic_types: IndexMap<GenericType, IndexSet<syn::Type>>,
    variadics: IndexMap<Variadic, IndexSet<syn::Type>>,
}

// TODO: improve efficiency
pub fn combinations<T, V>(mapping: IndexMap<T, IndexSet<V>>) -> Vec<IndexMap<T, V>>
where
    T: Hash + Eq + Clone,
    V: Hash + Eq + Clone,
{
    let keys = mapping.keys().cloned().collect::<Vec<_>>();
    let result = mapping
        .values()
        .flatten()
        .collect::<IndexSet<_>>()
        .into_iter()
        .cloned()
        .combinations(keys.len())
        .map(|combination| {
            combination
                .into_iter()
                .enumerate()
                .map(|(i, v)| (keys[i].clone(), v))
                .collect::<IndexMap<_, _>>()
        })
        .filter(|combination| combination.into_iter().all(|(k, v)| mapping[k].contains(v)))
        .collect::<Vec<_>>();

    result
}

impl UnresolvedTypeMapping {
    pub fn new() -> Self {
        Self {
            generic_types: IndexMap::new(),
            variadics: IndexMap::new(),
        }
    }

    pub fn merge(mut self, other: Self) -> Self {
        for (k, v) in other.generic_types {
            self.add_generic_type(k, v);
        }

        for (k, v) in other.variadics {
            self.variadics
                .entry(k)
                .and_modify(|entry| {
                    entry.retain(|value| v.contains(value));
                })
                .or_insert(v);
        }

        self
    }

    pub fn add_generic_type(
        &mut self,
        generic_type: GenericType,
        possible_values: IndexSet<syn::Type>,
    ) {
        self.generic_types
            .insert_or_intersection(generic_type, possible_values);
    }

    pub fn add_variadic_type(&mut self, variadic: Variadic, possible_values: IndexSet<syn::Type>) {
        self.variadics.insert(variadic, possible_values);
    }

    pub fn resolve(&self) -> Vec<TypeMapping> {
        let mut result = Vec::new();
        // TODO: what happens if there are no generic types?
        for combination in combinations(self.generic_types.clone()) {
            let mut type_mapping = TypeMapping::new();

            for (variadic, mut possible_values) in self.variadics.clone() {
                possible_values.retain(|ty| !combination.values().contains(ty));

                type_mapping.add_variadic_type(variadic, possible_values);
            }

            for (generic, ty) in combination {
                type_mapping.add_generic_type(generic, ty);
            }

            result.push(type_mapping);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use indexmap::indexset;
    use pretty_assertions::assert_eq;

    // TODO: copy-paste from solver.rs (share this code)
    macro_rules! type_mapping {
        ( types => { $( $left_ty:expr => $right_ty:expr ),* $(,)? }, variadics => { $( $left_var:expr => $right_var:expr ),* $(,)? } $(,)? ) => {
            {
                let mut _mapping = TypeMapping::new();

                $(
                    _mapping.add_generic_type($left_ty, $right_ty);
                )*

                $(
                    _mapping.add_variadic_type($left_var, $right_var);
                )*

                _mapping
            }
        };
    }

    #[test]
    fn test_resolve() {
        let mut mapping = UnresolvedTypeMapping::new();

        mapping.add_generic_type(syn::parse_quote!(T), indexset! { syn::parse_quote!(u8) });
        mapping.add_variadic_type(
            syn::parse_quote!(..A),
            indexset! { syn::parse_quote!(u16), syn::parse_quote!(u32) },
        );

        assert_eq!(
            mapping.resolve(),
            vec![type_mapping! {
                types => {
                    syn::parse_quote!(T) => syn::parse_quote!(u8),
                },
                variadics => {
                    syn::parse_quote!(..A) => indexset! { syn::parse_quote!(u16), syn::parse_quote!(u32) },
                },
            }]
        );
    }
}
