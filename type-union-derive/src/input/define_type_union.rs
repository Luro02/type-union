use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::Token;

use crate::impl_declaration::Template;
use crate::input::TypeUnionDefinition;
use crate::utils::{ErrorExt, PeekAfter};

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

    /// Checks if the given path overrides a given impl.
    ///
    /// Returns `true` if the given path would override an `impl`, otherwise `false`.
    #[must_use]
    fn overrides_impl(&self, trait_impl: &Template) -> bool {
        self.impls()
            .any(|template| trait_impl.does_override(template))
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
        let mut templates = Template::built_in_impls();

        templates.retain(|trait_impl| !self.overrides_impl(trait_impl));

        for trait_impl in templates.iter().chain(self.impls()) {
            for decl in self
                .declarations()
                // skip all decls for which the trait does not apply
                .filter(|decl| trait_impl.does_apply(decl.type_union(), decl.impl_attr()))
            {
                match trait_impl.try_apply(decl.type_union(), decl.impl_attr()) {
                    Ok(output) => {
                        errors.clear();
                        tokens.append_all(output);
                    }
                    Err(error) => errors.push(error),
                }
            }
        }

        let mut errors = errors.into_iter();
        if let Some(error) = errors.next() {
            tokens.append_all(error.combine_all(errors).into_compile_error());
        }
    }
}
