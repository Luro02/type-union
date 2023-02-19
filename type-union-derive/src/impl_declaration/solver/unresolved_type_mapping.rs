use std::hash::Hash;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use quote::ToTokens;

use super::{IndexMapExt, TypeMapping};
use crate::impl_declaration::{GenericType, WildcardParam};

#[derive(Clone, Debug)]
pub struct UnresolvedTypeMapping {
    generic_types: IndexMap<GenericType, IndexSet<syn::Type>>,
    variadics: IndexMap<WildcardParam, IndexSet<syn::Type>>,
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

fn debug_set<T: ToTokens>(set: IndexSet<T>) -> IndexSet<T> {
    eprint!("[");
    let mut iter = set.iter();
    if let Some(first) = iter.next() {
        eprint!("`{}`", first.into_token_stream().to_string());
    }

    for item in iter {
        eprint!(", `{}`", item.into_token_stream().to_string());
    }

    eprint!("]\n");
    set
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
            // TODO: here conflicts have to be resolved
            // for example:
            //
            // (..A | ..B) = (u8 | u16 | u32)
            // (..A | ..C) = (u8 | u32 | String)
            // -> (..A) = (u8 | u32) and (..B) = (u16) and (..C) = (String)
            self.variadics
                .entry(k)
                .and_modify(|entry| {
                    debug_set(v.clone());
                    debug_set(entry.clone());
                    // TODO: resolve conflict
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

    pub fn add_variadic_type(
        &mut self,
        variadic: WildcardParam,
        possible_values: IndexSet<syn::Type>,
    ) {
        self.variadics.insert(variadic, possible_values);
    }

    pub fn resolve(&self) -> Vec<TypeMapping> {
        let mut result = Vec::new();
        for combination in combinations(self.generic_types.clone()) {
            let mut type_mapping = TypeMapping::new();

            for (variadic, mut possible_values) in self.variadics.clone() {
                // TODO: this does not make sense when there is more than one set possible
                /*if let Some(mut values) = possible_values.into_iter().next() {
                    values.retain(|ty| !combination.values().contains(ty));

                    type_mapping.add_variadic_type(variadic, values);
                }*/
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
