use std::collections::HashSet;

use proc_macro2::{Ident, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parenthesized, Token, Type};

use crate::utils::{resolve_type_name, resolve_type_union_name, ParseStreamExt};

/// A [`TypeUnion`] input looks like this: `A | B | C`, with optional `()`, `(A | B | C)`
#[derive(Debug, Clone)]
pub struct TypeUnion {
    _paren_token: Option<syn::token::Paren>,
    punctuated: Punctuated<Type, Token![|]>,
}

impl TypeUnion {
    const ERROR_MISSING_PARENS: &str = "missing parens around type union";

    /// Returns the name of the type that type union resolves to.
    ///
    /// There are no anonymous enums in the rust language for now, so
    /// each type union resolves to some concrete type: `(A | B | C)` -> `Abc`.
    ///
    /// ## Note
    ///
    /// It is not guranteed that the returned value of the function is the same across
    /// different versions.
    #[must_use]
    pub fn ident(&self) -> syn::Ident {
        resolve_type_union_name(self.punctuated.iter())
    }

    pub fn types(&self) -> impl Iterator<Item = &Type> + '_ {
        self.punctuated.iter()
    }

    #[must_use]
    pub fn is_parenthesized(&self) -> bool {
        self._paren_token.is_some()
    }

    /// Checks if the parens are present.
    ///
    /// ## Errors
    ///
    /// If the parens are not present an error is returned.
    pub fn has_parens_or_err(&self) -> syn::Result<()> {
        if !self.is_parenthesized() {
            return Err(syn::Error::new_spanned(
                &self.punctuated,
                Self::ERROR_MISSING_PARENS,
            ));
        }

        Ok(())
    }
}

fn parse_type(visited: &mut HashSet<Ident>, input: ParseStream<'_>) -> syn::Result<Type> {
    let ty = input.parse::<Type>()?;
    let ident = resolve_type_name(&ty).ok_or_else(|| input.error("could not resolve type name"))?;

    if visited.contains(&ident) {
        // TODO: add more information to error? like the location?
        // TODO: write a ui test?
        return Err(input.error("duplicate type name"));
    } else {
        visited.insert(ident);
    }

    Ok(ty)
}

impl Parse for TypeUnion {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut visited = HashSet::new();

        // reassign input to local variable, so the borrowed &content does
        // live long enough
        let mut input = input;

        let content;
        Ok(Self {
            _paren_token: {
                if input.peek(syn::token::Paren) {
                    let res = parenthesized!(content in input);
                    input = &content;
                    Some(res)
                } else {
                    None
                }
            },
            punctuated: input.parse_terminated2(|input| parse_type(&mut visited, input))?,
        })
    }
}

impl ToTokens for TypeUnion {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self.ident())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = syn::parse_str::<TypeUnion>("A | B | C").unwrap();
        assert_eq!(input.ident().to_string(), "Abc");
        let mut iter = input.types();
        assert_eq!(iter.next(), Some(&syn::parse_str("A").unwrap()));
        assert_eq!(iter.next(), Some(&syn::parse_str("B").unwrap()));
        assert_eq!(iter.next(), Some(&syn::parse_str("C").unwrap()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_parse_paren() {
        let input = syn::parse_str::<TypeUnion>("(A | B | C)").unwrap();
        assert_eq!(input.is_parenthesized(), true);
        assert_eq!(input.ident().to_string(), "Abc");
        let mut iter = input.types();
        assert_eq!(iter.next(), Some(&syn::parse_str("A").unwrap()));
        assert_eq!(iter.next(), Some(&syn::parse_str("B").unwrap()));
        assert_eq!(iter.next(), Some(&syn::parse_str("C").unwrap()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_error_duplicate_variant() {
        let input = syn::parse_str::<TypeUnion>("A | B | A");
        assert_eq!(
            input.err().map(|e| e.to_string()),
            Some("unexpected end of input, duplicate type name".to_string())
        );
    }
}
