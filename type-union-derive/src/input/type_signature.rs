use indexmap::IndexMap;
use syn::parse::{Parse, ParseStream};

use crate::utils::{LooksLike, PunctuatedExt};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSignature {
    path: syn::Path,
    // optionally one can specify associated types:
    // Iterator<Item = u8>
    bindings: IndexMap<syn::Ident, syn::Type>,
    args: Vec<syn::GenericArgument>,
}

impl TypeSignature {
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
}

impl LooksLike for TypeSignature {
    fn looks_like(&self, other: &Self) -> bool {
        self.path.looks_like(&other.path)
        // TODO: compare bindings and args?
    }
}

impl LooksLike<syn::Path> for TypeSignature {
    fn looks_like(&self, other: &syn::Path) -> bool {
        if let Ok(other) = Self::try_from(other.clone()) {
            self.looks_like(&other)
        } else {
            false
        }
    }
}

impl Parse for TypeSignature {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Self::try_from(input.parse::<syn::Path>()?)
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
            path,
            bindings,
            args,
        })
    }
}
