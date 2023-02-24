use std::fmt::Debug;
use std::hash::Hash;

use indexmap::IndexSet;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{parenthesized, Token};

use crate::utils::{self, resolve_type_union_name, PunctuatedSet};

pub trait Type: Parse + ToTokens + Clone + Hash + Eq {}

impl<T: Parse + ToTokens + Clone + Hash + Eq> Type for T {}

/// A [`TypeUnion`] input looks like this: `A | B | C`, with optional `()`, `(A | B | C)`
#[derive(Debug, Clone)]
pub struct TypeUnion<T: Type> {
    _paren_token: Option<syn::token::Paren>,
    // `Token![|]` in combination with generics and derive is not allowed, see:
    // https://github.com/rust-lang/rust/issues/50676
    //
    // Therefore the concrete type is used here
    punctuated: PunctuatedSet<T, syn::token::Or>,
}

impl<T: Type> TypeUnion<T> {
    const ERROR_MISSING_PARENS: &str = "missing parens around type union";

    pub fn parse_macro(mac: &syn::Macro) -> syn::Result<Self> {
        if !utils::is_macro(mac, "type_union") {
            return Err(syn::Error::new_spanned(
                &mac.path,
                "expected `type_union` macro",
            ));
        }

        mac.parse_body()
    }

    pub fn iter_types(&self) -> impl Iterator<Item = &T> + '_ {
        self.punctuated.iter()
    }

    #[must_use]
    pub fn map_types<U: Type, F: FnMut(T) -> U>(self, f: F) -> TypeUnion<U> {
        TypeUnion {
            _paren_token: self._paren_token,
            punctuated: self.punctuated.into_iter().map(f).collect(),
        }
    }

    #[must_use]
    pub fn flat_map_types<I, U: Type, F: FnMut(T) -> I>(self, f: F) -> TypeUnion<U>
    where
        I: IntoIterator<Item = U>,
    {
        TypeUnion {
            _paren_token: self._paren_token,
            punctuated: self
                .punctuated
                .into_iter()
                .flat_map(f)
                .collect::<IndexSet<_>>()
                .into_iter()
                .collect(),
        }
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

    #[must_use]
    pub fn span(&self) -> Span {
        self.punctuated.span()
    }

    #[must_use]
    pub fn to_macro_tokens(&self) -> TokenStream {
        let mut tokens = TokenStream::new();

        let or_tokens: TokenStream = self
            .punctuated
            .iter()
            .map(|ty| ty.to_token_stream())
            .intersperse_with(|| <Token![|]>::default().to_token_stream())
            .collect();

        if let Some(paren) = self._paren_token {
            paren.surround(&mut tokens, |tokens| {
                tokens.append_all(or_tokens);
            });
        } else {
            tokens.append_all(or_tokens)
        }

        tokens
    }

    #[must_use]
    pub fn to_macro_type(&self) -> syn::Type {
        syn::Type::Macro(syn::TypeMacro {
            mac: syn::Macro {
                path: syn::parse_quote!(type_union),
                bang_token: Default::default(),
                delimiter: syn::MacroDelimiter::Paren(Default::default()),
                tokens: self.to_macro_tokens(),
            },
        })
    }
}

impl TypeUnion<syn::Type> {
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
    pub fn to_type(&self) -> syn::Type {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path::from(resolve_type_union_name(self.punctuated.iter())),
        })
    }
}

impl<T: Type + Hash + Eq> Parse for TypeUnion<T> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
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
            punctuated: {
                let punctuated: Punctuated<_, _> = input.parse_terminated(T::parse)?;

                if punctuated.is_empty() {
                    return Err(syn::Error::new_spanned(
                        punctuated,
                        "type union must not be empty",
                    ));
                }

                PunctuatedSet::try_from(punctuated)?
            },
        })
    }
}

impl ToTokens for TypeUnion<syn::Type> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.to_type().to_tokens(tokens)
    }
}

impl<T: Type> PartialEq for TypeUnion<T> {
    fn eq(&self, other: &Self) -> bool {
        self.punctuated == other.punctuated
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::impl_declaration::EitherType;

    use pretty_assertions::assert_eq;
    use quote::quote;

    #[test]
    fn test_parse() {
        let input: TypeUnion<syn::Type> = syn::parse_quote!(A | B | C);
        let mut iter = input.iter_types();
        assert_eq!(iter.next(), Some(&syn::parse_quote!(A)));
        assert_eq!(iter.next(), Some(&syn::parse_quote!(B)));
        assert_eq!(iter.next(), Some(&syn::parse_quote!(C)));
        assert_eq!(iter.next(), None);

        let input = syn::parse_str::<TypeUnion<EitherType>>("..A | ..B").unwrap();

        let mut iter = input.iter_types();
        assert_eq!(iter.next(), Some(&syn::parse_quote!(..A)));
        assert_eq!(iter.next(), Some(&syn::parse_quote!(..B)));
        assert_eq!(iter.next(), None);

        let input: TypeUnion<EitherType> = syn::parse_quote!(..A | ..B);

        let mut iter = input.iter_types();
        assert_eq!(iter.next(), Some(&syn::parse_quote!(..A)));
        assert_eq!(iter.next(), Some(&syn::parse_quote!(..B)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_parse_paren() {
        let input = syn::parse_str::<TypeUnion<syn::Type>>("(A | B | C)").unwrap();
        assert_eq!(input.is_parenthesized(), true);
        let mut iter = input.iter_types();
        assert_eq!(iter.next(), Some(&syn::parse_str("A").unwrap()));
        assert_eq!(iter.next(), Some(&syn::parse_str("B").unwrap()));
        assert_eq!(iter.next(), Some(&syn::parse_str("C").unwrap()));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_error_duplicate_variant() {
        let input = syn::parse_str::<TypeUnion<syn::Type>>("A | B | A");
        assert_eq!(
            input.err().map(|e| e.to_string()),
            Some("input must not contain duplicate values".to_string())
        );
    }

    #[test]
    fn test_to_macro_tokens() {
        let input: TypeUnion<syn::Type> = syn::parse_quote!(A | B | C);

        assert_eq!(
            input.to_macro_tokens().to_string(),
            quote!(A | B | C).to_string()
        );
    }
}
