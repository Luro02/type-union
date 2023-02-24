use std::collections::HashMap;

use syn::fold::Fold;
use syn::parse::ParseStream;
use syn::Token;

use crate::impl_declaration::{EitherType, Generics, TypeMapping};
use crate::input::{TypeSignature, TypeUnion, TypeUnionMatch};
use crate::utils::{is_macro, resolve_type_name, PunctuatedExt};

pub struct Folder<'a> {
    pub extra_mapping: HashMap<syn::Type, syn::Type>,
    pub type_mapping: TypeMapping,
    pub errors: Vec<syn::Error>,
    pub trait_path: &'a syn::Path,
    pub signature: &'a TypeSignature,
    pub generics: &'a Generics,
}

fn parse_promote_macro(mac: &syn::Macro, folder: &mut dyn Fold) -> syn::Result<syn::Expr> {
    let expr_type = mac.parse_body::<syn::ExprType>()?;

    let expr = folder.fold_expr(*expr_type.expr);
    let ty = folder.fold_type(*expr_type.ty);
    // TODO: error if ty is not supported?
    let variant_name = resolve_type_name(&ty).unwrap();

    Ok(syn::parse_quote! {
        Self::#variant_name( #expr )
    })
}

fn parse_optional_arg_macro(
    mac: &syn::Macro,
    folder: &mut dyn Fold,
) -> syn::Result<(syn::Ident, syn::Type)> {
    let (ident, ty) = mac.parse_body_with(|stream: ParseStream<'_>| {
        let ident = stream.parse::<syn::Ident>()?;
        let _eq_token = stream.parse::<Token![=]>()?;
        let ty = stream.parse::<syn::Type>()?;

        Ok((ident, ty))
    })?;

    Ok((folder.fold_ident(ident), folder.fold_type(ty)))
}

impl<'a> Fold for Folder<'a> {
    fn fold_type_path(&mut self, mut ty_path: syn::TypePath) -> syn::TypePath {
        // TODO: this does add a fallback for the case where one refers to a variadic type.
        // For example the variadic ..A might be referred to in the impl as A::Item.
        //
        // The type of A::Item is not known, but A is not in the final code. So as a fallback
        // add a mapping of A to the first possible type of ..A.
        // TODO: this will be very inefficient
        for (variadic, possible_values) in self.type_mapping.iter_variadics() {
            let ident = variadic.ident();
            let value = possible_values.iter().next().unwrap();

            self.extra_mapping
                .insert(syn::parse_quote! { #ident }, syn::parse_quote!(#value));
        }

        ty_path = syn::fold::fold_type_path(self, ty_path);

        for (variadic, _) in self.type_mapping.iter_variadics() {
            let ident = variadic.ident();

            self.extra_mapping.remove(&syn::parse_quote!(#ident));
        }

        ty_path
    }

    fn fold_type(&mut self, ty: syn::Type) -> syn::Type {
        if let Some(ty) = self
            .type_mapping
            .get(&ty)
            .or_else(|| self.extra_mapping.get(&ty))
            .cloned()
        {
            return ty;
        }

        syn::fold::fold_type(self, ty)
    }

    fn fold_macro(&mut self, mac: syn::Macro) -> syn::Macro {
        let mut mac = mac;

        if is_macro(&mac, "type_union") {
            if let Ok(type_union) = TypeUnion::parse_macro(&mac) {
                let resolved_type_union = type_union.flat_map_types(|ty| match ty {
                    EitherType::Concrete(ty) => {
                        vec![self.fold_type(*ty)]
                    }
                    EitherType::Generic(ty) => {
                        vec![self.fold_type(ty.to_type())]
                    }
                    EitherType::Variadic(ty) => {
                        if let Some(possible_types) = self.type_mapping.get_variadic(&ty).cloned() {
                            possible_types
                                .into_iter()
                                .map(|ty| self.fold_type(ty))
                                .collect()
                        } else {
                            // TODO: error?
                            vec![]
                        }
                    }
                });

                mac.tokens = resolved_type_union.to_macro_tokens();
                return mac;
            } else {
                // TODO: error about invalid syntax
            }
        }

        if is_macro(&mac, "match_type_union") {
            if let Ok(type_union_match) = mac.parse_body::<TypeUnionMatch<EitherType>>() {
                let folded_expr = self.fold_expr_type(type_union_match.expr_ty);
                let mut result: TypeUnionMatch<syn::Type> = TypeUnionMatch::new(folded_expr);

                for arm in type_union_match.arms {
                    match &arm.ty {
                        EitherType::Concrete(ty) => {
                            let ty = self.fold_type(*(ty.clone()));
                            result.arms.push(arm.clone().map_ty(|_| ty));
                        }
                        EitherType::Generic(ty) => {
                            let ty = self.fold_type(ty.to_type());

                            result.arms.push(arm.clone().map_ty(|_| ty));
                        }
                        EitherType::Variadic(ty) => {
                            if let Some(possible_types) = self.type_mapping.get_variadic(ty) {
                                for concrete_ty in possible_types {
                                    result
                                        .arms
                                        .push(arm.clone().map_ty(|_| concrete_ty.clone()));
                                }
                            }
                        }
                    }
                }

                mac.tokens = result.into_macro_tokens();
                return mac;
            } else {
                // TODO: add an error?
            }
        }

        mac
    }

    fn fold_where_clause(&mut self, mut where_clause: syn::WhereClause) -> syn::WhereClause {
        where_clause.predicates = where_clause
            .predicates
            .into_iter()
            .flat_map(|predicate| match predicate {
                syn::WherePredicate::Type(mut ty) => {
                    let mut predicates = Vec::new();

                    // TODO: move this to Generics?

                    // TODO: what about bounds on concrete types? (I think they are lost)

                    // add a bound for the generic type to concrete type
                    if let Some(concrete_ty) = self.type_mapping.get(&ty.bounded_ty) {
                        ty.bounded_ty = concrete_ty.clone();
                        ty.bounds = ty.bounds.map(|bound| self.fold_type_param_bound(bound));
                        predicates.push(syn::WherePredicate::Type(ty));

                        return predicates;
                    }

                    // repeat the bound for all possible types of the variadic.
                    //
                    // For example for when ..A is { usize, u8, u16 } and the bound is
                    // A : fmt::Display,, then this will add the following bounds:
                    // - usize : fmt::Display
                    // - u8 : fmt::Display
                    // - u16 : fmt::Display

                    // TODO: allow the inverse: String: From<A> where anyA
                    // TODO: resolve From<String>: A or From<A>: String and the likes

                    // on the left-side of the bound is always a type and on the right-side is a trait:
                    // where
                    //   <type>: <trait>
                    //
                    // the variadic can appear as the type:
                    // where
                    //   A: <trait>
                    // in which case the trait is repeated for all possible types of the variadic.
                    //
                    // it can also appear inside the type as a generic parameter:
                    // where
                    //   MyStruct<A>: <trait>
                    //
                    // This is not supported yet, because of this case:
                    // where
                    //   MyStruct<A, B>: <trait>
                    //
                    // (where A and B are variadics)
                    // ^ TODO: write issue

                    if let syn::Type::Path(syn::TypePath { path, .. }) = &ty.bounded_ty {
                        if let Some(possible_types) = self
                            .generics
                            .get_variadic(path)
                            .and_then(|variadic| self.type_mapping.get_variadic(&variadic))
                        {
                            for concrete_ty in possible_types.clone() {
                                let bounded_ty = ty.bounded_ty.clone();

                                self.extra_mapping
                                    .insert(bounded_ty.clone(), concrete_ty.clone());

                                let result = self.fold_predicate_type(ty.clone());

                                let removed_value = self.extra_mapping.remove(&ty.bounded_ty);
                                debug_assert_eq!(removed_value, Some(concrete_ty));
                                predicates.push(syn::WherePredicate::Type(result));
                            }
                        }
                    }

                    predicates
                }
                _ => vec![predicate],
            })
            .collect();

        where_clause
    }

    fn fold_item_impl(&mut self, mut result: syn::ItemImpl) -> syn::ItemImpl {
        result.attrs = result
            .attrs
            .into_iter()
            .map(|attr| self.fold_attribute(attr))
            .collect();

        // only keep the lifetimes and const generics:
        result.generics.params = result.generics.params.filter_map(|p| {
            // TODO: only remove our special generics/variadics
            if matches!(p, syn::GenericParam::Type(_)) {
                None
            } else {
                Some(p)
            }
        });

        result.generics.where_clause = result
            .generics
            .where_clause
            .map(|where_clause| self.fold_where_clause(where_clause));

        if let Some((bang, path, for_token)) = result.trait_ {
            result.trait_ = Some((bang, self.fold_path(path), for_token));
        }

        result.self_ty = Box::new(self.fold_type(*result.self_ty));

        result.items = result
            .items
            .into_iter()
            .map(|item| self.fold_impl_item(item))
            .collect::<Vec<_>>();

        result
    }

    fn fold_expr(&mut self, expr: syn::Expr) -> syn::Expr {
        let syn::Expr::Macro(expr_mac) = expr else {
            return syn::fold::fold_expr(self, expr);
        };

        // special syntax for promoting a type to a type-union
        if is_macro(&expr_mac.mac, "__promote") {
            match parse_promote_macro(&expr_mac.mac, self) {
                Ok(value) => return value,
                Err(err) => {
                    self.errors.push(err);
                    return syn::parse_quote!(::core::unimplemented!());
                }
            }
        }

        syn::fold::fold_expr(self, syn::Expr::Macro(expr_mac))
    }

    fn fold_impl_item_type(&mut self, mut item_ty: syn::ImplItemType) -> syn::ImplItemType {
        if let syn::Type::Macro(syn::TypeMacro { mac }) = &item_ty.ty {
            if is_macro(&mac, "optional_arg") {
                if let Ok((ident, ty)) = parse_optional_arg_macro(&mac, self) {
                    let arg = self.signature.get_binding(&ident).cloned().unwrap_or(ty);

                    item_ty.ty = arg;
                } else {
                    // TODO: error
                }
            } else if is_macro(&mac, "required_arg") {
                if let Ok(ident) = mac.parse_body::<syn::Ident>() {
                    // TODO: return error instead of unwrap
                    let arg = self.signature.get_binding(&ident).cloned().unwrap();

                    item_ty.ty = arg;
                } else {
                    // TODO: error
                }
            }
        }

        item_ty
    }
}
