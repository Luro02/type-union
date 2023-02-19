use std::hash::Hash;

use indexmap::indexset;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use quote::ToTokens;

use super::{SetId, UnresolvedTypeMapping};
use crate::impl_declaration::solver::InferredValue;
use crate::impl_declaration::{GenericType, SetCollection, WildcardParam};
use crate::utils::ErrorExt;

/// Represents the generics and variadics of a type union.
///
/// For example the type union in the template is `(u16 | A | B | ..C | ..D)`
/// and then the user says that this should be equal to the type
/// `(u8 | u16 | u32 | u64 | u128 | String | usize)`.
///
/// The `TypeUnionSet` does not contain the fixed types from the template, only
/// those that are generic or variadic. In this case:
/// - the generics: `A`, `B`
/// - the variadics: `..C`, `..D`
///
/// and those generics/types share the types `(u8 | u32 | u64 | u128 | String | usize)`.
#[derive(Debug, Clone)]
pub struct TypeUnionSet {
    set: SetId,
    generic_types: IndexMap<GenericType, InferredValue<syn::Type>>,
    variadics: IndexMap<WildcardParam, InferredValue<SetId>>,
}

pub fn iter_subsets<I, T>(iter: I) -> impl Iterator<Item = IndexSet<T>>
where
    T: Hash + Eq + Clone,
    I: IntoIterator<Item = T>,
{
    iter.into_iter()
        .powerset()
        // each set must contain at least one element
        .filter(|s| !s.is_empty())
        .map(|set| set.into_iter().collect())
}

#[derive(Debug, Clone)]
struct TypeSet {
    base_types: IndexSet<syn::Type>,
    values: InferredValue<SetId>,
}

impl TypeUnionSet {
    pub fn new(
        set: SetId,
        generic_types: IndexSet<GenericType>,
        variadics: IndexSet<WildcardParam>,
    ) -> Self {
        Self {
            set,
            generic_types: generic_types
                .into_iter()
                .map(|k| (k, InferredValue::Unknown))
                .collect(),
            variadics: variadics
                .into_iter()
                .map(|k| (k, InferredValue::Unknown))
                .collect(),
        }
    }

    fn update_variadic_inference<S: SetCollection>(
        mut set_collection: S,
        left: TypeSet,
        right: TypeSet,
    ) -> (TypeSet, TypeSet) {
        let updated_left_types = left.base_types;
        let updated_right_types = right.base_types;

        let mut updated_left_values = left.values;
        let mut updated_right_values = right.values;

        match (updated_left_values.clone(), updated_right_values.clone()) {
            // the other type union knows exactly what type the variadic must have, now both know it:
            (InferredValue::Fixed(set), _) | (_, InferredValue::Fixed(set)) => {
                updated_left_values = InferredValue::Fixed(set);
                updated_right_values = updated_left_values.clone();

                // TODO: the inferred type is fixed, so they have to removed from the base types?
            }
            // the other type union knows more about the variadic, so update both:
            (InferredValue::Any(values), InferredValue::Unknown)
            | (InferredValue::Unknown, InferredValue::Any(values)) => {
                // TODO: this is not correct, all known values from self must be removed from this set
                let other_values = {
                    if matches!(updated_right_values, InferredValue::Unknown) {
                        updated_right_types.clone()
                    } else {
                        debug_assert!(matches!(updated_left_values, InferredValue::Unknown));
                        updated_left_types.clone()
                    }
                };

                let new_values = values
                    .into_iter()
                    .filter_map(|set| {
                        let set = set_collection.get_set(&set).clone();
                        let intersection = set
                            .intersection(&other_values)
                            .cloned()
                            .collect::<IndexSet<_>>();

                        if intersection.is_empty() {
                            None
                        } else {
                            let id = set_collection.add_set(intersection);
                            Some(id)
                        }
                    })
                    .collect::<IndexSet<_>>();

                updated_left_values = InferredValue::Any(new_values);
                updated_right_values = updated_left_values.clone();
            }
            // both sets have a list of possible sets, so the intersection is the new set
            (InferredValue::Any(mut left_values), InferredValue::Any(right_values)) => {
                // only keep the values that are present in both sets
                left_values.retain(|v| right_values.contains(v));

                // TODO: it might happen that the intersection is empty!

                if left_values.len() == 1 {
                    let set = left_values.into_iter().next().unwrap();
                    updated_left_values = InferredValue::Fixed(set);
                }

                updated_right_values = updated_left_values.clone();
            }
            (InferredValue::Unknown, InferredValue::Unknown) => {
                let mut new_values = IndexSet::new();

                // both sets do not know the concrete value of the variadic
                // but both sets have a set of possible types the variadic
                // must consist of
                //
                // obviously the variadic must consist of the intersection:

                // TODO: deal with very large intersections, powerset will be 2^n-1 elements!
                let intersection = updated_left_types.intersection(&updated_right_types);

                for set in iter_subsets(intersection.cloned()) {
                    let set_id = set_collection.add_set(set);
                    new_values.insert(set_id);
                }

                if new_values.len() == 1 {
                    let set = new_values.iter().next().unwrap();
                    updated_left_values = InferredValue::Fixed(*set);
                } else {
                    updated_left_values = InferredValue::Any(new_values);
                }

                updated_right_values = updated_left_values.clone();
            }
        }

        (
            TypeSet {
                base_types: updated_left_types,
                values: updated_left_values,
            },
            TypeSet {
                base_types: updated_right_types,
                values: updated_right_values,
            },
        )
    }

    pub fn update_with<S>(&mut self, mut set_collection: S, others: &mut [Self])
    where
        S: SetCollection,
    {
        for (generic_type, values) in self
            .generic_types
            .iter_mut()
            .filter(|(_, v)| v.is_unknown())
        {
            let mut updated_value = values.clone();

            for other in others.iter_mut() {
                let inferred_value = other.generic_types.get(generic_type);

                // TODO: should others be updated as well if self knows more?
                match (inferred_value, &mut updated_value) {
                    (Some(InferredValue::Fixed(value)), updated_value) => {
                        *updated_value = InferredValue::Fixed(value.clone());
                        break;
                    }
                    (Some(InferredValue::Any(values)), InferredValue::Unknown) => {
                        updated_value = InferredValue::Any(values.clone());
                    }
                    (Some(InferredValue::Any(values)), InferredValue::Any(updated_values)) => {
                        // only keep the values that are present in both sets
                        updated_values.retain(|v| values.contains(v));
                    }
                    (_, _) => {}
                }
            }

            *values = updated_value;
        }

        // TODO: figure out conditions when the value of a variadic can be inferred
        // TODO: the test that fails has the following ambiguity:
        // for the current test case there is a unique solution, but for larger unions
        // the type might belong to either ..A or ..B and ..C
        //
        // one may solve this by greedily taking the variadics?
        //
        // For example (..A | ..B) & (..A | ..C) = (u8 | u16 | u32) & (u8 | u16 | u32)
        // here ..A could be either (u8 | u16) or (u8)
        //
        // How could one infer that?:
        // Iterate through all variadics in the first set,
        // then check in the other set if the variadic is present as well
        //
        // if that is the case, then the variadic can only consist of the intersection
        for (variadic, values) in self.variadics.iter_mut() {
            let mut updated_value = values.take();

            for other_union in others.iter_mut() {
                let other_union_set = other_union.set;
                let Some(other_values) = other_union.get_variadic_mut(variadic) else {
                    // the other union does not have this variadic => continue
                    continue;
                };

                let left = TypeSet {
                    base_types: set_collection.get_set(&self.set).clone(),
                    values: updated_value.take(),
                };
                let right = TypeSet {
                    base_types: set_collection.get_set(&other_union_set).clone(),
                    values: other_values.take(),
                };

                let (left, right) =
                    Self::update_variadic_inference(&mut set_collection, left, right);

                updated_value = left.values;
                *other_values = right.values;

                // TODO: update set ids as well if they changed
            }

            *values = updated_value;
        }
    }

    pub fn has_generic_type(&self, generic_type: &GenericType) -> bool {
        self.generic_types.contains_key(generic_type)
    }

    pub fn set_generic_type_equal_to(&mut self, generic_type: GenericType, value: syn::Type) {
        self.generic_types
            .insert(generic_type, InferredValue::Fixed(value));
    }

    pub fn has_variadic(&self, variadic: &WildcardParam) -> bool {
        self.variadics.contains_key(variadic)
    }

    pub fn get_variadic_mut(
        &mut self,
        variadic: &WildcardParam,
    ) -> Option<&mut InferredValue<SetId>> {
        self.variadics.get_mut(variadic)
    }

    /// Call this function when the variadic is equal to a known set.
    ///
    /// For example, if the variadic `..A` is known to be `(u8 | u16)`,
    /// then one would call this function to set `A` equal to that value.
    pub fn set_variadic_equal_to(&mut self, variadic: WildcardParam, value: SetId) {
        self.variadics.insert(variadic, InferredValue::Fixed(value));
    }

    pub fn solve(
        &self,
        mut f: impl FnMut(SetId) -> IndexSet<syn::Type>,
    ) -> syn::Result<UnresolvedTypeMapping> {
        let mut type_mapping = UnresolvedTypeMapping::new();
        let mut types = f(self.set);
        let mut generic_types = self.generic_types.clone();

        let mut unknown_generics = IndexSet::new();
        for (generic, ty) in &self.generic_types {
            // TODO: deal with any generics!
            let InferredValue::Fixed(ty) = ty else {
                unknown_generics.insert(generic.clone());
                continue;
            };

            type_mapping.add_generic_type(generic.clone(), indexset! { ty.clone() });

            // the generic has been resolved, so remove it:
            generic_types.remove(generic);
            // no duplicates allowed, so remove the type from the set of possible types:
            let was_possible = types.remove(ty);
            if !was_possible {
                return Err(syn::Error::new_spanned(
                    ty,
                    format!("type is not possible for the generic `{generic}`"),
                ));
            }
        }

        for generic in unknown_generics {
            type_mapping.add_generic_type(generic, types.clone());
        }

        let mut unknown_variadics = IndexSet::new();
        for (variadic, values) in self.variadics.clone() {
            // TODO: deal with any variadics?
            let InferredValue::Fixed(values) = values else {
                if let InferredValue::Any(values) = &values {
                    let vs = values.into_iter().map(|v| (*v, f(*v))).collect::<Vec<_>>();
                    dbg!((&variadic, &vs));
                } else {
                    dbg!((&variadic, &values));
                }
                unknown_variadics.insert(variadic);
                continue;
            };

            let tys = f(values);

            for ty in &tys {
                if !types.contains(ty) {
                    return Err(syn::Error::new_spanned(
                        ty,
                        format!("type is not possible for the variadic `{variadic}`"),
                    ));
                }
            }

            // no duplicates allowed
            types.retain(|ty| !tys.contains(ty));

            type_mapping.add_variadic_type(variadic, tys);
        }

        dbg!(&unknown_variadics);
        let mut iter = unknown_variadics.into_iter();
        if let Some(variadic) = iter.next() {
            type_mapping.add_variadic_type(variadic, types.clone());
        }

        if let Some(next_unknown) = iter.next() {
            return Err(syn::Error::new_spanned(
                &next_unknown,
                format!(
                    "cannot have more than one unknown variadic: `{}`",
                    (&next_unknown).into_token_stream()
                ),
            )
            .with_spans(iter));
        }

        Ok(type_mapping)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_type_union_set() {
        let set_id = SetId { id: 0 };

        let mut type_union_set = TypeUnionSet::new(
            set_id,
            indexset! {},
            indexset! { syn::parse_quote!(..A), syn::parse_quote!(..B) },
        );
    }
}
