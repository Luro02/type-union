use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Token;

use crate::impl_declaration::Template;
use crate::input::TypeUnionDefinition;
use crate::utils::{LooksLike, PeekAfter};

enum Definition {
    Declaration(TypeUnionDefinition),
    TraitImpl(Template),
}

impl Parse for Definition {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let forked = input.fork();
        if (&forked).peek_after(syn::Attribute::parse_outer, Token![impl]) {
            Ok(Self::TraitImpl(input.parse()?))
        } else {
            Ok(Self::Declaration(input.parse()?))
        }
    }
}

pub struct DefineTypeUnion {
    punctuated: Punctuated<Definition, Token![;]>,
}

impl DefineTypeUnion {
    fn declarations(&self) -> impl Iterator<Item = &TypeUnionDefinition> + '_ {
        self.punctuated.iter().filter_map(|d| match d {
            Definition::Declaration(decl) => Some(decl),
            _ => None,
        })
    }

    fn impls(&self) -> impl Iterator<Item = &Template> + '_ {
        self.punctuated.iter().filter_map(|d| match d {
            Definition::TraitImpl(v) => Some(v),
            _ => None,
        })
    }

    fn overrides_builtin_impl(&self, path: &syn::Path) -> bool {
        for trait_impl in self.impls() {
            // TODO: improve this comparison
            if trait_impl.trait_path().looks_like(path) {
                return true;
            }
        }

        false
    }
}

impl Parse for DefineTypeUnion {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            punctuated: input.parse_terminated(Definition::parse)?,
        })
    }
}

impl ToTokens for DefineTypeUnion {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for declaration in self.declarations() {
            tokens.append_all(quote!(
                #declaration
            ));
        }

        let mut errors = Vec::new();
        for trait_impl in Template::built_in_impls() {
            if self.overrides_builtin_impl(trait_impl.trait_path()) {
                continue;
            }

            // TODO: duplicate code between the two loops!
            for decl in self.declarations() {
                if !decl.impl_attr().has_trait(trait_impl.trait_path()) {
                    continue;
                }

                match trait_impl.try_apply(decl) {
                    Ok(output) => {
                        errors.clear();
                        tokens.append_all(output);
                    }
                    Err(error) => errors.push(error),
                }
            }
        }

        for trait_impl in self.impls() {
            for decl in self.declarations() {
                if !decl.impl_attr().has_trait(trait_impl.trait_path()) {
                    continue;
                }

                match trait_impl.try_apply(decl) {
                    Ok(output) => {
                        errors.clear();
                        tokens.append_all(output);
                    }
                    Err(error) => errors.push(error),
                }
            }
        }

        // TODO: make this a trait extension on syn::Error a la syn::Error::combine_all()
        if let Some(error) = errors.into_iter().reduce(|mut acc, error| {
            acc.combine(error);
            acc
        }) {
            tokens.append_all(error.into_compile_error());
        }
    }
}
