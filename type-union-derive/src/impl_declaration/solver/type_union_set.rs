use std::assert_matches::debug_assert_matches;
use std::hash::Hash;
use std::mem;

use indexmap::indexset;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use quote::ToTokens;

use super::{SetId, UnresolvedTypeMapping};
use crate::impl_declaration::solver::InferredValue;
use crate::impl_declaration::{GenericType, SetCollection, Variadic};
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
    variadics: IndexMap<Variadic, InferredValue<SetId>>,
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
struct InferenceArgument<V> {
    base_types: IndexSet<syn::Type>,
    value: InferredValue<V>,
}

impl<V> InferenceArgument<V> {
    pub fn new(base_types: IndexSet<syn::Type>, value: InferredValue<V>) -> Self {
        Self { base_types, value }
    }
}

impl TypeUnionSet {
    pub fn new(
        set: SetId,
        generic_types: IndexSet<GenericType>,
        variadics: IndexSet<Variadic>,
    ) -> Self {
        let mut result = Self {
            set,
            generic_types: generic_types
                .into_iter()
                .map(|k| (k, InferredValue::Unknown))
                .collect(),
            variadics: variadics
                .into_iter()
                .map(|k| (k, InferredValue::Unknown))
                .collect(),
        };

        result.resolve();

        result
    }

    fn resolve(&mut self) {
        // TODO: add more inferences?
        if self.variadics.len() == 1 && self.generic_types.is_empty() {
            self.variadics = mem::take(&mut self.variadics)
                .into_iter()
                .map(|(variadic, _)| (variadic, InferredValue::Fixed(self.set)))
                .collect();
        }
    }

    pub fn base_types<S: SetCollection>(&self, set_collection: S) -> IndexSet<syn::Type> {
        let mut set = set_collection.get_set(&self.set).clone();
        for value in self.generic_types.values() {
            if let InferredValue::Fixed(value) = value {
                set.remove(value);
            }
        }

        for values in self.variadics.values() {
            if let InferredValue::Fixed(value) = values {
                for ty in set_collection.get_set(value) {
                    set.remove(ty);
                }
            }
        }

        set
    }

    fn update_variadic_inference<S: SetCollection>(
        mut set_collection: S,
        mut left: InferenceArgument<SetId>,
        mut right: InferenceArgument<SetId>,
    ) -> (InferenceArgument<SetId>, InferenceArgument<SetId>) {
        // TODO: the modification of the base type is questionable

        // essentially the following are uninteresting:
        // - Invalid
        // - Fixed
        //
        // the following are of interest:
        // - Unknown
        // - Any
        //
        // ^ the Unknown and Any cases share the same code, the Unknown has the
        //   base types as the possible values.

        match (left.value.clone(), right.value.clone()) {
            // if either is invalid, then both are invalid
            (InferredValue::Invalid, _) | (_, InferredValue::Invalid) => {
                left.value = InferredValue::Invalid;
            }
            // the other type union knows exactly what type the variadic must have, now both know it:
            (InferredValue::Fixed(set), _) | (_, InferredValue::Fixed(set)) => {
                left.value = InferredValue::Fixed(set);
            }
            // the other type union knows more about the variadic, so update both:
            (InferredValue::Any(values), InferredValue::Unknown)
            | (InferredValue::Unknown, InferredValue::Any(values)) => {
                let other_values = {
                    if matches!(right.value, InferredValue::Unknown) {
                        right.base_types.clone()
                    } else {
                        debug_assert_matches!(left.value, InferredValue::Unknown);
                        left.base_types.clone()
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

                left.value = InferredValue::from_possible_values(new_values);
            }
            // both sets have a list of possible sets, so the intersection is the new set
            (InferredValue::Any(mut left_values), InferredValue::Any(right_values)) => {
                // only keep the values that are present in both sets
                left_values.retain(|v| right_values.contains(v));

                left.value = InferredValue::from_possible_values(left_values);
            }
            (InferredValue::Unknown, InferredValue::Unknown) => {
                let mut new_values = IndexSet::new();

                // both sets do not know the concrete value of the variadic
                // but both sets have a set of possible types the variadic
                // must consist of
                //
                // obviously the variadic must consist of the intersection:

                // TODO: deal with very large intersections, powerset will be 2^n-1 elements!
                // A possible solution: do not compute the powerset directly. Instead introduce a
                // new variant for InferredValue:
                // PowerSet { set: SetId, } where the set is the intersection.
                //
                // Then later on, when one encounters a PowerSet or a Fixed/Any, one can compute
                // the intersection of the sets and then compute the powerset of that
                let intersection = left.base_types.intersection(&right.base_types);

                for set in iter_subsets(intersection.cloned()) {
                    let set_id = set_collection.add_set(set);
                    new_values.insert(set_id);
                }

                left.value = InferredValue::from_possible_values(new_values);
            }
        }

        // the right side is the same as the left side (in the match only the left side is updated)
        right.value = left.value.clone();

        if let InferredValue::Fixed(set) = left.value {
            // the variadic is now fixed, so remove the types from the base types:
            let fixed_set = set_collection.get_set(&set);

            left.base_types.retain(|ty| !fixed_set.contains(ty));
            // the right side should be fixed as well to the same set
            debug_assert_matches!(right.value, InferredValue::Fixed(_));
            right.base_types.retain(|ty| !fixed_set.contains(ty));
        }

        (left, right)
    }

    fn update_generic_inference(
        mut left: InferenceArgument<syn::Type>,
        mut right: InferenceArgument<syn::Type>,
    ) -> (InferenceArgument<syn::Type>, InferenceArgument<syn::Type>) {
        match (left.value.clone(), right.value.clone()) {
            (InferredValue::Invalid, _) | (_, InferredValue::Invalid) => {
                left.value = InferredValue::Invalid;
                right.value = InferredValue::Invalid;
            }
            // one of the types is fixed, so the other must be fixed as well:
            (InferredValue::Fixed(value), _) | (_, InferredValue::Fixed(value)) => {
                left.value = InferredValue::Fixed(value);
                right.value = left.value.clone();
            }
            (InferredValue::Any(value), InferredValue::Unknown)
            | (InferredValue::Unknown, InferredValue::Any(value)) => {
                // one of the types is unknown, so it must be one of the base types:
                let other_types = {
                    if matches!(right.value, InferredValue::Unknown) {
                        &right.base_types
                    } else {
                        debug_assert_matches!(left.value, InferredValue::Unknown);
                        &left.base_types
                    }
                };

                // the possible types must be the intersection:
                let intersection = value.intersection(other_types).cloned().collect();

                left.value = InferredValue::from_possible_values(intersection);
                right.value = left.value.clone();
            }
            (InferredValue::Any(mut left_values), InferredValue::Any(right_values)) => {
                // only keep the intersection:
                left_values.retain(|v| right_values.contains(v));

                left.value = InferredValue::from_possible_values(left_values);
                right.value = left.value.clone();
            }
            (InferredValue::Unknown, InferredValue::Unknown) => {
                let mut values = left.base_types.clone();

                values.retain(|v| right.base_types.contains(v));

                left.value = InferredValue::from_possible_values(values);
                right.value = left.value.clone();
            }
        }

        // if the generic is fixed, remove the its type from the base types:
        if let InferredValue::Fixed(value) = &left.value {
            // TODO: invalidate types if they are not in the base_types
            left.base_types.remove(value);
        }

        if let InferredValue::Fixed(value) = &right.value {
            right.base_types.remove(value);
        }

        (left, right)
    }

    pub fn update_with<S>(&mut self, mut set_collection: S, others: &mut [Self])
    where
        S: SetCollection,
    {
        // TODO: remove this extra variable
        let immutable_base_types = set_collection.get_set(&self.set).clone();
        let mut self_base_types = self.base_types(&mut set_collection);
        for (generic_type, value) in self.generic_types.iter_mut() {
            let mut updated_value = value.take();

            for other_union in others.iter_mut() {
                let other_base_types = other_union.base_types(&mut set_collection);
                let Some(inferred_value) = other_union.generic_types.get_mut(generic_type) else {
                    // the other union does not have this generic => continue
                    continue;
                };

                let left = InferenceArgument::new(self_base_types, updated_value);
                let right = InferenceArgument::new(other_base_types, inferred_value.clone());

                let (left, right) = Self::update_generic_inference(left, right);

                updated_value = left.value;
                *inferred_value = right.value;

                self_base_types = left.base_types;
            }

            *value = updated_value;
        }

        let self_number_of_variadics = self.variadics.len();
        for (variadic, values) in self.variadics.iter_mut() {
            let mut updated_value = values.take();

            for other_union in others.iter_mut() {
                let other_base_types = other_union.base_types(&mut set_collection);
                let Some(other_values) = other_union.get_variadic_mut(variadic) else {
                    // the other union does not have this variadic => continue
                    continue;
                };

                let left = InferenceArgument::new(self_base_types.clone(), updated_value.take());
                let right = InferenceArgument::new(other_base_types, other_values.take());

                let (mut left, mut right) =
                    Self::update_variadic_inference(&mut set_collection, left, right);

                if let InferredValue::Any(values) = left.value {
                    let mut new_values = values.clone();
                    // eliminate sets, where
                    // - len(variadic) == 0,
                    // - generic needs to be more than one type at once (e.g. (T) for (u16 | u8))
                    // - generic is not used
                    for set_id in values {
                        let configuration = set_collection.get_set(&set_id);
                        let number_of_generics = self.generic_types.len();

                        // assume at least one type per variadic:
                        if configuration.len() + number_of_generics + (self_number_of_variadics - 1)
                            > immutable_base_types.len()
                        {
                            new_values.remove(&set_id);
                        }
                    }

                    left.value = InferredValue::from_possible_values(new_values);
                    right.value = left.value.clone();
                }

                updated_value = left.value;
                *other_values = right.value;

                self_base_types = left.base_types;
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

    pub fn get_variadic_mut(&mut self, variadic: &Variadic) -> Option<&mut InferredValue<SetId>> {
        self.variadics.get_mut(variadic)
    }

    pub fn solve(
        &self,
        mut f: impl FnMut(SetId) -> IndexSet<syn::Type>,
    ) -> syn::Result<UnresolvedTypeMapping> {
        let mut type_mapping = UnresolvedTypeMapping::new();
        let mut types = f(self.set);
        let mut generic_types = self.generic_types.clone();

        let mut unknown_generics = Vec::new();
        for (generic, ty) in self.generic_types.clone() {
            let InferredValue::Fixed(ty) = ty else {
                let mut values = None;
                if let InferredValue::Any(possible_values) = ty {
                    values = Some(possible_values);
                }

                // TODO: error for InferredValue::Invalid?

                unknown_generics.push((generic, values));
                continue;
            };

            // the generic has been resolved, so remove it:
            generic_types.remove(&generic);

            // no duplicates allowed, so remove the type from the set of possible types:
            let was_possible = types.remove(&ty);
            if !was_possible {
                return Err(syn::Error::new_spanned(
                    ty,
                    format!("type is not possible for the generic `{generic}`"),
                ));
            }

            // update the type mapping:
            type_mapping.add_generic_type(generic, indexset! { ty });
        }

        for (generic, values) in unknown_generics {
            type_mapping.add_generic_type(generic, values.unwrap_or_else(|| types.clone()));
        }

        let mut unknown_variadics = IndexSet::new();
        for (variadic, values) in self.variadics.clone() {
            // TODO: deal with any variadics?
            let InferredValue::Fixed(values) = values else {
                unknown_variadics.insert(variadic);
                continue;
            };

            let tys = f(values);

            for ty in &tys {
                if !types.contains(ty) {
                    eprintln!("ty: {}", ty.to_token_stream().to_string());
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

        let mut iter = unknown_variadics.into_iter();
        if let Some(variadic) = iter.next() {
            type_mapping.add_variadic_type(variadic, types.clone());
        }

        if let Some(next_unknown) = iter.next() {
            return Err(syn::Error::new_spanned(
                &next_unknown,
                format!(
                    "cannot have more than one unknown variadic: `{}`",
                    &next_unknown
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
    fn test_solve_1() {
        // SetId = (u8 | u16 | u32)
        let base_set_id = SetId::default();
        let mut type_union_set = TypeUnionSet::new(
            base_set_id,
            indexset! { syn::parse_quote!(T) },
            indexset! { syn::parse_quote!(..A) },
        );

        // now set the T = u8:
        type_union_set.set_generic_type_equal_to(syn::parse_quote!(T), syn::parse_quote!(u8));

        // because T is fixed to u8, the only valid mapping is:
        // T = u8, A = (u16 | u32)

        let mut expected = UnresolvedTypeMapping::new();

        expected.add_generic_type(syn::parse_quote!(T), indexset! { syn::parse_quote!(u8) });
        expected.add_variadic_type(
            syn::parse_quote!(..A),
            indexset! { syn::parse_quote!(u16), syn::parse_quote!(u32) },
        );

        assert_eq!(
            type_union_set
                .solve(|_| indexset! { syn::parse_quote!(u8), syn::parse_quote!(u16), syn::parse_quote!(u32) })
                .unwrap(),
            expected
        );
    }

    #[test]
    fn test_solve_variadic_must_be_base_set() {
        // SetId = (u8 | u16 | u32)
        let base_set_id = SetId::default();
        let type_union_set = TypeUnionSet::new(
            base_set_id,
            indexset! {},
            indexset! { syn::parse_quote!(..A) },
        );

        let mut expected = UnresolvedTypeMapping::new();

        expected.add_variadic_type(
            syn::parse_quote!(..A),
            indexset! { syn::parse_quote!(u8), syn::parse_quote!(u16), syn::parse_quote!(u32) },
        );

        assert_eq!(
            type_union_set
                .solve(|_| indexset! { syn::parse_quote!(u8), syn::parse_quote!(u16), syn::parse_quote!(u32) })
                .unwrap(),
            expected
        );
    }

    #[test]
    fn test_solve_generic() {
        // SetId = (u8 | u16 | u32)
        let base_set_id = SetId::default();
        let mut type_union_set = TypeUnionSet::new(
            base_set_id,
            indexset! { syn::parse_quote!(T) },
            indexset! { syn::parse_quote!(..A) },
        );

        type_union_set.set_generic_type_equal_to(
            syn::parse_quote!(T),
            syn::parse_quote!(MyWrapper<type_union!(u8 | u16 | u32)>),
        );

        let mut expected = UnresolvedTypeMapping::new();

        expected.add_variadic_type(
            syn::parse_quote!(..A),
            indexset! { syn::parse_quote!(u8), syn::parse_quote!(u16), syn::parse_quote!(u32) },
        );

        assert_eq!(
            type_union_set
                .solve(|_| indexset! { syn::parse_quote!(u8), syn::parse_quote!(u16), syn::parse_quote!(u32) })
                .unwrap(),
            expected
        );
    }
}
