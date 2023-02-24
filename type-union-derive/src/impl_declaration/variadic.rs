use std::fmt;

use proc_macro2::{Ident, TokenStream};
use quote::{quote_spanned, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::Token;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Variadic {
    pub(crate) dots_token: Option<Token![..]>,
    pub(crate) ident: syn::Ident,
}

impl Variadic {
    pub fn try_from_path(path: &syn::Path) -> Option<Self> {
        // TODO: how can the ident look like? is anyT allowed?
        path.get_ident().map(|ident| Self {
            dots_token: None,
            ident: ident.clone(),
        })
    }

    pub fn ident(&self) -> &syn::Ident {
        &self.ident
    }
}

impl Parse for Variadic {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Token![..]) {
            Ok(Self {
                dots_token: Some(input.parse()?),
                ident: input.parse()?,
            })
        } else {
            // anyT or not a variadic
            let ident = input.parse::<syn::Ident>()?;

            if !ident.to_string().starts_with("any") {
                return Err(syn::Error::new_spanned(
                    ident,
                    "not a variadic, which must start with either .. or any",
                ));
            }

            // TODO: adjust span
            let ident = Ident::new(&ident.to_string()[3..], ident.span());

            Ok(Self {
                dots_token: None,
                ident,
            })
        }
    }
}

impl fmt::Debug for Variadic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Variadic({}{})",
            self.dots_token.as_ref().map_or("any", |_| ".."),
            self.ident
        )
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
        write!(
            f,
            "{}{}",
            self.dots_token.as_ref().map_or("any", |_| ".."),
            self.ident
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;
    use quote::quote;

    #[test]
    fn test_to_tokens() {
        let param: Variadic = syn::parse_quote!(..T);

        assert_eq!(quote!(#param).to_string(), ".. T");
    }
}
