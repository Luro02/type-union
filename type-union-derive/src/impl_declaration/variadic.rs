use std::borrow::Borrow;
use std::fmt;

use proc_macro2::{Ident, TokenStream};
use quote::{quote_spanned, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::Token;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Variadic {
    dots_token: Token![..],
    ident: syn::Ident,
}

impl Variadic {
    #[must_use]
    pub fn ident(&self) -> &syn::Ident {
        &self.ident
    }
}

impl Borrow<syn::Ident> for Variadic {
    fn borrow(&self) -> &syn::Ident {
        self.ident()
    }
}

impl TryFrom<syn::Ident> for Variadic {
    type Error = syn::Error;

    fn try_from(ident: syn::Ident) -> Result<Self, Self::Error> {
        if !ident.to_string().starts_with("any") {
            return Err(syn::Error::new_spanned(
                ident,
                "not a variadic, which must start with either .. or any",
            ));
        }

        // NOTE: it seems to be impossible to adjust the span of the ident
        let ident = Ident::new(&ident.to_string()[3..], ident.span());

        Ok(Self {
            dots_token: Default::default(),
            ident,
        })
    }
}

impl Parse for Variadic {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Token![..]) {
            Ok(Self {
                dots_token: input.parse()?,
                ident: input.parse()?,
            })
        } else {
            // anyT or not a variadic
            let ident = input.parse::<syn::Ident>()?;

            Self::try_from(ident)
        }
    }
}

impl fmt::Debug for Variadic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Variadic(..{})", self.ident)
    }
}

impl ToTokens for Variadic {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        tokens.append_all(quote_spanned!(ident.span() => ..#ident))
    }
}

impl fmt::Display for Variadic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "..{}", self.ident)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::{assert_eq, assert_ne};
    use quote::quote;

    #[test]
    fn test_to_tokens() {
        let param: Variadic = syn::parse_quote!(..T);

        assert_eq!(quote!(#param).to_string(), ".. T");
    }

    #[test]
    fn test_ident() {
        let param: Variadic = syn::parse_quote!(..T);
        assert_eq!(param.ident().to_string(), "T");

        let param: Variadic = syn::parse_quote!(anyT);
        assert_eq!(param.ident().to_string(), "T");
    }

    #[test]
    fn test_equality() {
        let dots_a: Variadic = syn::parse_quote!(..A);
        let dots_b: Variadic = syn::parse_quote!(..B);
        let any_a: Variadic = syn::parse_quote!(anyA);
        let any_b: Variadic = syn::parse_quote!(anyB);

        // ..A == ..A
        assert_eq!(dots_a, dots_a);
        // ..A != ..B
        assert_ne!(dots_a, dots_b);

        // anyA != anyB
        assert_ne!(any_a, any_b);
        // ..A != anyB
        assert_ne!(dots_a, any_b);

        // anyA == ..A
        assert_eq!(any_a, dots_a);
        // ..A == anyA
        assert_eq!(dots_a, any_a);
    }
}
