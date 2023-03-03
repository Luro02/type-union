use std::mem;

use indexmap::IndexMap;
use syn::fold::Fold;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::Token;

use crate::input::TypeUnion;
use crate::utils::{Context, LooksLike, PunctuatedExt};

/// Represents a higher ranked type bound a la `for<T in Self>`.
#[derive(Debug, Clone, PartialEq)]
struct HigherRankedTypeBound {
    _for_token: Token![for],
    _lt_token: Token![<],
    // constraints are not supported, so instead of `syn::TypeParam`,
    // just `syn::Ident` is used.
    ty_param: syn::Ident,
    _in_token: Token![in],
    target_ty: syn::Type,
    _gt_token: Token![>],
}

impl HigherRankedTypeBound {
    pub fn resolve(&mut self, current_ty: syn::Type, new_ty: syn::Type) -> syn::Result<()> {
        let mut replacer = TypeReplacer { current_ty, new_ty };

        self.target_ty = replacer.fold_type(self.target_ty.clone());

        Ok(())
    }

    pub fn ident(&self) -> &syn::Ident {
        &self.ty_param
    }

    pub fn types(&self) -> impl Iterator<Item = syn::Type> + '_ {
        [&self.target_ty].into_iter().flat_map(|ty| {
            if let syn::Type::Macro(syn::TypeMacro { mac }) = ty {
                if let Ok(type_union) = TypeUnion::<syn::Type>::parse_macro(&mac) {
                    return type_union.iter_types().cloned().collect::<Vec<_>>();
                }
            }

            unreachable!("target type should have been resolved to a type union")
        })
    }
}

impl Parse for HigherRankedTypeBound {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            _for_token: input.parse()?,
            _lt_token: input.parse()?,
            ty_param: input.parse()?,
            _in_token: input.parse()?,
            target_ty: {
                // for now, only `Self` is supported
                let ty: Token![Self] = input.parse()?;

                syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: syn::parse_quote_spanned! { ty.span() => Self },
                })
            },
            _gt_token: input.parse()?,
        })
    }
}

impl Context for HigherRankedTypeBound {
    fn is_generic(&self, path: &syn::Path) -> bool {
        if let Some(ident) = path.get_ident() {
            ident == &self.ty_param
        } else {
            false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSignature {
    hrt_bound: Option<HigherRankedTypeBound>,
    path: syn::Path,
    // optionally one can specify associated types:
    // Iterator<Item = u8>
    bindings: IndexMap<syn::Ident, syn::Type>,
    args: Vec<syn::GenericArgument>,
    self_ty: Option<(Token![for], syn::Type)>,
}

struct TypeReplacer {
    current_ty: syn::Type,
    new_ty: syn::Type,
}

impl Fold for TypeReplacer {
    fn fold_type(&mut self, mut ty: syn::Type) -> syn::Type {
        if ty == self.current_ty {
            ty = self.new_ty.clone();
        } else {
            ty = syn::fold::fold_type(self, ty);
        }

        ty
    }
}

impl TypeSignature {
    #[must_use]
    pub fn new(path: syn::Path, self_ty: Option<syn::Type>) -> syn::Result<Self> {
        let mut result = Self::try_from(path)?;

        result.self_ty = self_ty.map(|ty| (<Token![for]>::default(), ty));

        Ok(result)
    }

    pub fn types(&self) -> impl Iterator<Item = &syn::Type> + '_ {
        self.args.iter().filter_map(|arg| match arg {
            syn::GenericArgument::Type(ty) => Some(ty),
            _ => None,
        })
    }

    #[must_use]
    pub fn get_binding(&self, ident: &syn::Ident) -> Option<&syn::Type> {
        self.bindings.get(ident)
    }

    pub fn self_ty(&self) -> Option<&syn::Type> {
        self.self_ty.as_ref().map(|(_, ty)| ty)
    }

    /// Resolves the type signature by replacing all instances of `current_ty` with `new_ty`.
    pub fn resolve(&mut self, current_ty: syn::Type, new_ty: syn::Type) -> syn::Result<()> {
        self.hrt_bound = self
            .hrt_bound
            .take()
            .map(|mut bound| {
                bound.resolve(current_ty.clone(), new_ty.clone())?;
                Ok::<_, syn::Error>(bound)
            })
            .transpose()?;

        let mut replacer = TypeReplacer { current_ty, new_ty };

        let mut path: syn::Path = syn::parse_quote!(placeholder);
        mem::swap(&mut path, &mut self.path);
        self.path = replacer.fold_path(path);

        self.bindings = mem::take(&mut self.bindings)
            .into_iter()
            .map(|(ident, ty)| (ident, replacer.fold_type(ty)))
            .collect();
        self.args = mem::take(&mut self.args)
            .into_iter()
            .map(|value| replacer.fold_generic_argument(value))
            .collect();
        let (for_token, self_ty) = self
            .self_ty
            .take()
            // by default the self_ty is `Self`
            .unwrap_or_else(|| (Default::default(), syn::parse_quote!(Self)));
        self.self_ty = Some((for_token, replacer.fold_type(self_ty)));

        Ok(())
    }

    #[must_use]
    pub fn unfold(self) -> Vec<Self> {
        let mut result = Vec::new();

        if let Some(bound) = &self.hrt_bound {
            result.extend(bound.types().map(|ty| {
                let mut signature = self.clone();
                signature.hrt_bound = None;
                signature
                    .resolve(
                        syn::Type::Path(syn::TypePath {
                            qself: None,
                            path: syn::Path::from(bound.ident().clone()),
                        }),
                        ty,
                    )
                    .unwrap();

                signature
            }));
        } else {
            result.push(self);
        }

        result
    }
}

impl LooksLike for TypeSignature {
    fn looks_like_with(&self, other: &Self, ctx: &dyn Context) -> bool {
        let ctx = (ctx, (self.hrt_bound.as_ref(), other.hrt_bound.as_ref()));
        self.path.looks_like_with(&other.path, &ctx)
            && self.self_ty.looks_like_with(&other.self_ty, &ctx)
        // TODO: compare bindings and args?
    }
}

impl LooksLike<syn::Path> for TypeSignature {
    fn looks_like_with(&self, other: &syn::Path, ctx: &dyn Context) -> bool {
        self.path.looks_like_with(other, ctx)
    }
}

impl Parse for TypeSignature {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut hrt_bound = None;
        if input.peek(Token![for]) {
            hrt_bound = Some(input.parse()?);
        }
        let path = input.parse::<syn::Path>()?;

        let mut result = Self::try_from(path)?;
        // check if the trait is implemented for a specific type
        if input.peek(Token![for]) {
            let _for = input.parse::<Token![for]>()?;
            let ty = input.parse::<syn::Type>()?;
            result.self_ty = Some((_for, ty));
        }

        result.hrt_bound = hrt_bound;

        Ok(result)
    }
}

impl TryFrom<syn::Path> for TypeSignature {
    type Error = syn::Error;

    fn try_from(mut path: syn::Path) -> Result<Self, Self::Error> {
        let mut bindings = IndexMap::new();
        let mut args = Vec::new();

        if let Some(segment) = path.segments.last_mut() {
            match &mut segment.arguments {
                syn::PathArguments::Parenthesized(args) => {
                    return Err(syn::Error::new_spanned(args, "parenthesized arguments are not supported, use angular brackets instead `<` `>`"));
                }
                syn::PathArguments::AngleBracketed(angle_args) => {
                    for arg in angle_args
                        .args
                        .drain_filter(|e| matches!(e, syn::GenericArgument::Binding(_)))
                    {
                        let syn::GenericArgument::Binding(binding) = arg else {
                            unreachable!("must be a binding");
                        };

                        if let Some(_ty) = bindings.insert(binding.ident, binding.ty) {
                            return Err(syn::Error::new_spanned(
                                &segment.arguments,
                                "duplicate binding in arguments",
                            ));
                        }
                    }

                    args = angle_args.args.iter().cloned().collect();
                }
                syn::PathArguments::None => {
                    // nothing to do
                }
            }

            // if angle bracketed is now empty, make it None
            // this is necessary, because otherwise the path will be something like Iterator<>
            // which is not equal to Iterator
            if segment.arguments.is_empty() {
                segment.arguments = syn::PathArguments::None;
            }
        }

        Ok(Self {
            hrt_bound: None,
            path,
            bindings,
            args,
            self_ty: None,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_looks_like_hrt_bound() {
        let mut signature: TypeSignature = syn::parse_quote!(for<T in Self> From<T> for Self);
        signature
            .resolve(
                syn::parse_quote!(Self),
                syn::parse_quote!(type_union!(u8 | u16)),
            )
            .unwrap();

        // the signature should now only look like `From<u8> for (u8 | u16)` and `From<u16> for (u8 | u16)`

        let other_signature: TypeSignature = syn::parse_quote!(From<u8> for type_union!(u8 | u16));
        assert!(signature.looks_like(&other_signature));

        let other_signature: TypeSignature = syn::parse_quote!(From<u16> for type_union!(u8 | u16));
        assert!(signature.looks_like(&other_signature));

        let other_signature: TypeSignature = syn::parse_quote!(From<T> for type_union!(T | ..A));
        assert!(signature.looks_like(&other_signature));

        let other_signature: TypeSignature = syn::parse_quote!(From<U> for type_union!(U | ..A));
        assert!(signature.looks_like(&other_signature));
    }

    #[test]
    fn test_looks_like_path() {
        let signature = TypeSignature::new(
            syn::parse_quote!(Copy),
            Some(syn::parse_quote!(type_union!(u8 | u16))),
        )
        .unwrap();

        let trait_path: syn::Path = syn::parse_quote!(Copy);
        assert!(signature.looks_like(&trait_path));
    }

    #[test]
    fn test_looks_like_impl_non_type_union() {
        let mut impl_signature: TypeSignature = syn::parse_quote!(TryFrom<Self> for u8);
        let trait_signature: TypeSignature = syn::parse_quote!(TryFrom<type_union!(T | ..A)> for T);

        impl_signature
            .resolve(
                syn::parse_quote!(Self),
                syn::parse_quote!(type_union!(u8 | u16)),
            )
            .unwrap();

        let true_type: syn::Type = syn::parse_quote!(type_union!(u8 | u16));
        let template_type: syn::Type = syn::parse_quote!(type_union!(T | ..A));
        assert_eq!(true_type.looks_like(&template_type), true);

        let ctx: syn::Generics = syn::parse_quote!(<T, anyA>);

        assert_eq!(
            trait_signature
                .self_ty()
                .looks_like_with(&impl_signature.self_ty().cloned(), &ctx),
            true
        );

        assert_eq!(impl_signature.looks_like_with(&trait_signature, &ctx), true);
    }

    #[test]
    fn test_signature_looks_like() {
        let a: TypeSignature =
            syn::parse_quote!(TryFrom<type_union!(..A)> for MyWrapper<type_union!(..A)>);
        let b: TypeSignature = syn::parse_quote!(TryFrom<type_union!(u8 | u16 | u32)> for MyWrapper<type_union!(u8 | u16 | u32)>);

        assert_eq!(a.looks_like(&b), true);

        // a can be implemented for any type unions variant
        let a: TypeSignature = syn::parse_quote!(TryFrom<type_union!(T | ..A)> for T);
        let b: TypeSignature =
            syn::parse_quote!(TryFrom<type_union!(..A)> for MyWrapper<type_union!(..A)>);

        assert_eq!(a.looks_like(&b), false);
    }

    #[ignore = "too complicated to implement for now, see #27"]
    #[test]
    fn test_looks_like_non_variant() {
        let template: TypeSignature = syn::parse_quote!(TryFrom<type_union!(T | ..A)> for T);
        let true_signature: TypeSignature = syn::parse_quote!(TryFrom<type_union!(u8 | u16 | u32)> for MyWrapper<type_union!(u8 | u16 | u32)>);

        // T must be a variant of the type union, for them to look like each other

        let ctx: syn::Generics = syn::parse_quote!(<T, anyA>);
        assert_eq!(template.looks_like_with(&true_signature, &ctx), false);
    }
}
