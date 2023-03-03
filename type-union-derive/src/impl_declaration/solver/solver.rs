use indexmap::{IndexMap, IndexSet};
use quote::ToTokens;

use super::{SetId, TypeMapping, TypeUnionSet, UnresolvedTypeMapping};
use crate::impl_declaration::{EitherType, GenericType, Generics};
use crate::input::TypeUnion;
use crate::utils::{is_macro, Context, LooksLike};

#[derive(Debug, Clone)]
pub struct TypeSolver {
    sets: IndexMap<SetId, IndexSet<syn::Type>>,
    next_set_id: usize,
    generics: Generics,
    type_union_sets: Vec<TypeUnionSet>,
    generic_type_values: IndexMap<GenericType, syn::Type>,
}

pub trait SetCollection {
    fn add_set(&mut self, set: IndexSet<syn::Type>) -> SetId;

    fn get_set(&self, id: &SetId) -> &IndexSet<syn::Type>;
}

impl<T: SetCollection> SetCollection for &mut T {
    fn add_set(&mut self, set: IndexSet<syn::Type>) -> SetId {
        (**self).add_set(set)
    }

    fn get_set(&self, id: &SetId) -> &IndexSet<syn::Type> {
        (**self).get_set(id)
    }
}

impl SetCollection for TypeSolver {
    fn add_set(&mut self, set: IndexSet<syn::Type>) -> SetId {
        // ensure that the set is not already added
        if let Some(id) = self
            .sets
            .iter()
            .find_map(|(id, values)| if values == &set { Some(*id) } else { None })
        {
            return id;
        }

        let id = SetId {
            id: self.next_set_id,
        };

        self.sets.insert(id, set);
        self.next_set_id += 1;

        id
    }

    fn get_set(&self, id: &SetId) -> &IndexSet<syn::Type> {
        &self.sets[id]
    }
}

impl TypeSolver {
    pub fn new(generics: Generics) -> Self {
        Self {
            sets: IndexMap::new(),
            next_set_id: 0,
            generics,
            type_union_sets: Vec::new(),
            generic_type_values: IndexMap::new(),
        }
    }

    fn add_type_union_set(&mut self, set: TypeUnionSet) {
        self.type_union_sets.push(set);
    }

    pub fn add_union_constraint(
        &mut self,
        expected: &TypeUnion<EitherType>,
        declaration: &TypeUnion<syn::Type>,
    ) -> syn::Result<()> {
        // a set with all the types in the declaration:
        let mut concrete_types = declaration.iter_types().cloned().collect::<IndexSet<_>>();

        let mut variadics = IndexSet::new();
        let mut concrete = IndexSet::new();
        let mut generics = IndexSet::new();

        // iterate over the template and partition the types into three sets:
        // - variadics (..A, ..B, ..)
        // - generics like A, B, C, T
        // - concrete types like u8, u16 or u32

        let expected = self.generics.resolve(expected.clone());

        for ty in expected.iter_types().cloned() {
            match ty {
                EitherType::Concrete(ty) => {
                    if let Some(value) = concrete_types.shift_take(&*ty) {
                        concrete.insert(value);
                    } else {
                        // error type not found in declaration, but required in template
                        return Err(syn::Error::new_spanned(
                            declaration,
                            format!("expected declaration to have `{}`", ty.to_token_stream()),
                        ));
                    }
                }
                EitherType::Generic(generic) => {
                    generics.insert(generic.clone());
                }
                EitherType::Variadic(variadic) => {
                    variadics.insert(variadic.clone());
                }
            }
        }

        // NOTE: a variadic must have a size of at least 1 and each generic is exactly one type
        //       so this is a lower bound for the number of types that are required
        if variadics.len() + generics.len() > concrete_types.len() {
            return Err(syn::Error::new_spanned(
                declaration,
                "not enough types in declaration to fill template",
            ));
        }

        let set = self.add_set(concrete_types);

        self.add_type_union_set(TypeUnionSet::new(set, generics.clone(), variadics.clone()));

        Ok(())
    }

    /// Sets the generic type equal to the given type.
    fn set_generic_type_equal_to(&mut self, generic: GenericType, ty: syn::Type) {
        self.generic_type_values.insert(generic, ty);
    }

    pub fn add_type_constraint(
        &mut self,
        template: &syn::Type,
        declaration: &syn::Type,
    ) -> syn::Result<()> {
        match (template, declaration) {
            (
                syn::Type::Macro(syn::TypeMacro { mac: templ_mac }),
                syn::Type::Macro(syn::TypeMacro { mac: decl_mac }),
            ) if is_macro(templ_mac, "type_union") && is_macro(decl_mac, "type_union") => {
                let templ_type_union: TypeUnion<EitherType> = TypeUnion::parse_macro(templ_mac)?;
                let decl_type_union: TypeUnion<syn::Type> = TypeUnion::parse_macro(decl_mac)?;

                self.add_union_constraint(&templ_type_union, &decl_type_union)?;
            }
            // check if one of the types is a generic (e.g. T, A, B, C), then the generic must be equal
            // to the other type
            (syn::Type::Path(syn::TypePath { path, qself: None }), other)
            | (other, syn::Type::Path(syn::TypePath { path, qself: None }))
                if self.generics.is_generic(path) =>
            {
                let generic = self.generics.get_generic(path).unwrap();

                self.set_generic_type_equal_to(generic, other.clone());
            }
            (
                syn::Type::Path(syn::TypePath {
                    path: templ_path,
                    qself: Some(templ_qself),
                }),
                syn::Type::Path(syn::TypePath {
                    path: decl_path,
                    qself: Some(decl_qself),
                }),
                // the paths store the trait cast/associated type, which should look equal
                // for example:
                // <T as SomeTrait>::SomeType
                // <U as SomeTrait>::SomeType
                //
                // will have the paths:
                // SomeTrait::SomeType
                // SomeTrait::SomeType
            ) if templ_path.looks_like_with(decl_path, &self.generics) => {
                // both have a qself:
                // <T as SomeTrait>::SomeType
                // <U as SomeTrait>::SomeType

                // the position should always be equal when they look alike?
                assert_eq!(templ_qself.position, decl_qself.position);

                // if the paths have generics, then constrain them:
                if let Some((
                    syn::PathSegment {
                        arguments: syn::PathArguments::AngleBracketed(templ_args),
                        ..
                    },
                    syn::PathSegment {
                        arguments: syn::PathArguments::AngleBracketed(decl_args),
                        ..
                    },
                )) = templ_path
                    .segments
                    .iter()
                    .zip(decl_path.segments.iter())
                    .rev()
                    .next()
                {
                    // the paths have generics, so constrain them
                    for (templ, decl) in templ_args
                        .args
                        .iter()
                        .zip(decl_args.args.iter())
                        .filter_map(|(left, right)| {
                            if let (
                                syn::GenericArgument::Type(left),
                                syn::GenericArgument::Type(right),
                            ) = (left, right)
                            {
                                Some((left, right))
                            } else {
                                None
                            }
                        })
                    {
                        self.add_type_constraint(templ, decl)?;
                    }
                }

                self.add_type_constraint(templ_qself.ty.as_ref(), decl_qself.ty.as_ref())?;
            }
            (
                syn::Type::Path(syn::TypePath {
                    path: templ_path,
                    qself: None,
                }),
                syn::Type::Path(syn::TypePath {
                    path: decl_path,
                    qself: None,
                }),
            ) => {
                // TODO: should this be called with context?!
                if templ_path.looks_like_with(decl_path, &self.generics) {
                    // the paths look similar
                    // call this function on the generics of the paths
                    //
                    // e.g. `Vec<T>` = `Vec<u8>` -> `T` = `u8
                    let last_templ_segment = templ_path.segments.last().unwrap();
                    let last_decl_segment = decl_path.segments.last().unwrap();

                    if let (
                        syn::PathArguments::AngleBracketed(templ_args),
                        syn::PathArguments::AngleBracketed(decl_args),
                    ) = (&last_templ_segment.arguments, &last_decl_segment.arguments)
                    {
                        for (templ_arg, decl_arg) in
                            templ_args.args.iter().zip(decl_args.args.iter())
                        {
                            if let (
                                syn::GenericArgument::Type(templ_arg),
                                syn::GenericArgument::Type(decl_arg),
                            ) = (templ_arg, decl_arg)
                            {
                                self.add_type_constraint(templ_arg, decl_arg)?;
                            }
                        }
                    }
                } else {
                    if templ_path != decl_path {
                        return Err(syn::Error::new_spanned(
                            decl_path,
                            format!("expected type to be '{}'", templ_path.into_token_stream()),
                        ));
                    }
                }
            }
            (_, _) => {
                return Err(syn::Error::new_spanned(template, "unsupported type"));
            }
        }

        Ok(())
    }

    pub fn solve(mut self) -> syn::Result<Vec<TypeMapping>> {
        let mut sets = self.type_union_sets.clone();

        for set in &mut sets {
            for (generic, ty) in self.generic_type_values.clone() {
                if !set.has_generic_type(&generic) {
                    continue;
                }

                set.set_generic_type_equal_to(generic, ty);
            }
        }

        for i in 0..sets.len() {
            let mut others = sets;
            let mut set = others.remove(i);
            set.update_with(&mut self, &mut others);
            others.insert(i, set);
            sets = others;
        }

        let mut unresolved_type_mapping = UnresolvedTypeMapping::new();

        // if there are not sets, then only add the generic types for which the values are known
        if sets.is_empty() {
            let mut type_mapping = TypeMapping::new();
            for (generic, ty) in self.generic_type_values {
                type_mapping.add_generic_type(generic, ty);
            }

            return Ok(vec![type_mapping]);
        }

        for type_union_set in sets {
            let type_mapping = type_union_set.solve(|id| self.get_set(&id).clone())?;

            unresolved_type_mapping = unresolved_type_mapping.merge(type_mapping);
        }

        Ok(unresolved_type_mapping.resolve())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use indexmap::indexset;
    use pretty_assertions::assert_eq;

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
    fn test_solve_type_param_mapping() {
        let mut solver = TypeSolver::new(Generics::new(syn::parse_quote!(<T, U>)));

        solver
            .add_union_constraint(
                &syn::parse_quote!(T | U | String),
                &syn::parse_quote!(u64 | u32 | String),
            )
            .unwrap();

        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {
                    syn::parse_quote!(T) => syn::parse_quote!(u64),
                    syn::parse_quote!(U) => syn::parse_quote!(u32)
                },
                variadics => {},
            }]
        );
    }

    #[test]
    fn test_solve_type_param_variadic_mapping() {
        let mut solver = TypeSolver::new(Generics::new(syn::parse_quote!(<T, U>)));

        solver
            .add_union_constraint(
                &syn::parse_quote!(T | U | ..A),
                &syn::parse_quote!(u8 | u16 | u32 | u64),
            )
            .unwrap();

        assert_eq!(
            solver.solve().unwrap(),
            vec![
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u8),
                        syn::parse_quote!(U) => syn::parse_quote!(u16)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u32),
                            syn::parse_quote!(u64),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u8),
                        syn::parse_quote!(U) => syn::parse_quote!(u32)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u16),
                            syn::parse_quote!(u64),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u8),
                        syn::parse_quote!(U) => syn::parse_quote!(u64)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u16),
                            syn::parse_quote!(u32),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u16),
                        syn::parse_quote!(U) => syn::parse_quote!(u32)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u8),
                            syn::parse_quote!(u64),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u16),
                        syn::parse_quote!(U) => syn::parse_quote!(u64)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u8),
                            syn::parse_quote!(u32),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u32),
                        syn::parse_quote!(U) => syn::parse_quote!(u64)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u8),
                            syn::parse_quote!(u16),
                        }
                    },
                },
            ]
        );
    }

    #[test]
    fn test_solve_display_impl() {
        let mut solver = TypeSolver::new(Generics::new(Default::default()));

        solver
            .add_union_constraint(
                &syn::parse_quote!(..A),
                &syn::parse_quote!(u8 | u16 | u32 | u64),
            )
            .unwrap();

        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {},
                variadics => {
                    syn::parse_quote!(..A) => indexset! {
                        syn::parse_quote!(u8),
                        syn::parse_quote!(u16),
                        syn::parse_quote!(u32),
                        syn::parse_quote!(u64),
                    }
                },
            }]
        );
    }

    #[test]
    fn test_solve_from_ty_impl() {
        let mut solver = TypeSolver::new(Generics::new(syn::parse_quote!(<T>)));

        solver
            .add_union_constraint(
                &syn::parse_quote!(T | ..A),
                &syn::parse_quote!(u8 | u16 | u32 | u64),
            )
            .unwrap();

        assert_eq!(
            solver.solve().unwrap(),
            vec![
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u8)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u16),
                            syn::parse_quote!(u32),
                            syn::parse_quote!(u64),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u16)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u8),
                            syn::parse_quote!(u32),
                            syn::parse_quote!(u64),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u32)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u8),
                            syn::parse_quote!(u16),
                            syn::parse_quote!(u64),
                        }
                    },
                },
                type_mapping! {
                    types => {
                        syn::parse_quote!(T) => syn::parse_quote!(u64)
                    },
                    variadics => {
                        syn::parse_quote!(..A) => indexset! {
                            syn::parse_quote!(u8),
                            syn::parse_quote!(u16),
                            syn::parse_quote!(u32),
                        }
                    },
                },
            ]
        );
    }

    #[test]
    fn test_solve_from_union_impl() {
        // impl<..A, ..B> ::core::convert::From<(..A)> for (..A | ..B) { ... }
        let mut solver = TypeSolver::new(Generics::new(Default::default()));

        solver
            .add_union_constraint(
                &syn::parse_quote!(..A | ..B),
                &syn::parse_quote!(u8 | u16 | u32 | u64),
            )
            .unwrap();

        solver
            .add_type_constraint(
                &syn::parse_quote!(type_union!(..A)),
                &syn::parse_quote!(type_union!(u16 | u32)),
            )
            .unwrap();

        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {},
                variadics => {
                    syn::parse_quote!(..A) => indexset! {
                        syn::parse_quote!(u16),
                        syn::parse_quote!(u32),
                    },
                    syn::parse_quote!(..B) => indexset! {
                        syn::parse_quote!(u8),
                        syn::parse_quote!(u64),
                    },
                },
            },]
        );
    }

    #[test]
    fn test_solve_partial_eq_union_impl() {
        // impl<..A, ..B> ::core::cmp::PartialEq<(..A)> for (..A | ..B) { ... }
        let mut solver = TypeSolver::new(Generics::new(Default::default()));

        solver
            .add_union_constraint(&syn::parse_quote!(..A | ..B), &syn::parse_quote!(u8 | u16))
            .unwrap();

        solver
            .add_union_constraint(
                &syn::parse_quote!(..A),
                &syn::parse_quote!(u8 | u16 | String),
            )
            .unwrap();

        let error = solver.solve().expect_err("should fail");

        assert_eq!(
            &error.to_string(),
            "type is not possible for the variadic `..A`"
        );

        // impl<..A, ..B, ..C> ::core::cmp::PartialEq<(..A | ..C)> for (..A | ..B) { ... }
        let mut solver = TypeSolver::new(Generics::new(Default::default()));

        solver
            .add_union_constraint(&syn::parse_quote!(..A | ..B), &syn::parse_quote!(u8 | u16))
            .unwrap();

        solver
            .add_union_constraint(
                &syn::parse_quote!(..A | ..C),
                &syn::parse_quote!(u8 | u16 | String),
            )
            .unwrap();

        // this configuration is not possible, because for
        // (..A | ..B) = (u8 | u16)
        // (..A | ..C) = (u8 | u16 | String)
        // (..A) could be either (u8) or (u16)

        let error = solver.solve().expect_err("should fail");

        // TODO: add dedicated error message for an ambiguous variadic?
        assert_eq!(
            &error.to_string(),
            "cannot have more than one unknown variadic: `..B`"
        );

        // impl<..A, ..B, ..C> ::core::cmp::PartialEq<(..A | ..C)> for (..A | ..B) { ... }
        let mut solver = TypeSolver::new(Generics::new(Default::default()));

        solver
            .add_union_constraint(&syn::parse_quote!(..A | ..B), &syn::parse_quote!(u8 | u16))
            .unwrap();

        solver
            .add_union_constraint(
                &syn::parse_quote!(..A | ..C),
                &syn::parse_quote!(u8 | usize | String),
            )
            .unwrap();

        // the difference in this test is that ..A can only be u8
        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {},
                variadics => {
                    syn::parse_quote!(..A) => indexset! {
                        syn::parse_quote!(u8),
                    },
                    syn::parse_quote!(..B) => indexset! {
                        syn::parse_quote!(u16),
                    },
                    syn::parse_quote!(..C) => indexset! {
                        syn::parse_quote!(usize),
                        syn::parse_quote!(String),
                    },
                },
            },]
        )
    }

    #[test]
    fn test_solve_impl_on_variant() {
        // impl<T, anyA> TryFrom<(T | ..A)> for T {
        // with TryFrom<(u8 | u16 | u32)> for u8
        let mut solver = TypeSolver::new(Generics::new(syn::parse_quote!(<T, anyA>)));

        solver
            .add_union_constraint(
                &syn::parse_quote!(T | ..A),
                &syn::parse_quote!(u8 | u16 | u32),
            )
            .unwrap();

        solver
            .add_type_constraint(&syn::parse_quote!(T), &syn::parse_quote!(u8))
            .unwrap();

        // T must be u8 => ..A = (u16 | u32)

        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {
                    syn::parse_quote!(T) => syn::parse_quote!(u8),
                },
                variadics => {
                    syn::parse_quote!(..A) => indexset! {
                        syn::parse_quote!(u16),
                        syn::parse_quote!(u32),
                    },
                },
            },]
        );
    }

    /// This is the same as [`test_solve_impl_on_variant`], but the order of the
    /// constraints are reversed.
    ///
    /// So first the constraint `T = u8` is added, and then `(T | ..A) = (u8 | u16 | u32)`
    #[test]
    fn test_solve_impl_on_variant_reverse() {
        // impl<T, anyA> TryFrom<(T | ..A)> for T {
        // with TryFrom<(u8 | u16 | u32)> for u8
        let mut solver = TypeSolver::new(Generics::new(syn::parse_quote!(<T, anyA>)));

        solver
            .add_type_constraint(&syn::parse_quote!(T), &syn::parse_quote!(u8))
            .unwrap();

        solver
            .add_union_constraint(
                &syn::parse_quote!(T | ..A),
                &syn::parse_quote!(u8 | u16 | u32),
            )
            .unwrap();

        // T must be u8 => ..A = (u16 | u32)

        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {
                    syn::parse_quote!(T) => syn::parse_quote!(u8),
                },
                variadics => {
                    syn::parse_quote!(..A) => indexset! {
                        syn::parse_quote!(u16),
                        syn::parse_quote!(u32),
                    },
                },
            },]
        );
    }

    #[test]
    fn test_add_qself_constraint() {
        let mut solver = TypeSolver::new(Generics::new(syn::parse_quote!(<T>)));

        solver
            .add_type_constraint(
                &syn::parse_quote!(<T as Trait>::Item),
                &syn::parse_quote!(<u8 as Trait>::Item),
            )
            .unwrap();

        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {
                    syn::parse_quote!(T) => syn::parse_quote!(u8),
                },
                variadics => {
                },
            }]
        );

        // test constraint on generic args:
        let mut solver = TypeSolver::new(Generics::new(syn::parse_quote!(<T>)));

        solver
            .add_type_constraint(
                &syn::parse_quote!(<u8 as Trait>::Value<T>),
                &syn::parse_quote!(<u8 as Trait>::Value<u16>),
            )
            .unwrap();

        assert_eq!(
            solver.solve().unwrap(),
            vec![type_mapping! {
                types => {
                    syn::parse_quote!(T) => syn::parse_quote!(u16),
                },
                variadics => {
                },
            }]
        );
    }
}
