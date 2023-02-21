use std::fmt;

use proc_macro2::{Ident, TokenStream};
use quote::{quote_spanned, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::Token;

// TODO: rename to variadic

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct WildcardParam {
    pub(crate) dots_token: Option<Token![..]>,
    pub(crate) ident: syn::Ident,
}

impl WildcardParam {
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

impl Parse for WildcardParam {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(Token![..]) {
            Ok(Self {
                dots_token: Some(input.parse()?),
                ident: input.parse()?,
            })
        } else {
            // anyT or not a wildcard
            let ident = input.parse::<syn::Ident>()?;

            if !ident.to_string().starts_with("any") {
                return Err(syn::Error::new_spanned(
                    ident,
                    "not a wildcard param, which must start with either .. or any",
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

impl fmt::Debug for WildcardParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "WildcardParam({}{})",
            self.dots_token.as_ref().map_or("any", |_| ".."),
            self.ident
        )
    }
}

impl ToTokens for WildcardParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        tokens.append_all(quote_spanned!(ident.span() => ..#ident))
    }
}

impl fmt::Display for WildcardParam {
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
        let param: WildcardParam = syn::parse_quote!(..T);

        assert_eq!(quote!(#param).to_string(), ".. T");
    }
}
