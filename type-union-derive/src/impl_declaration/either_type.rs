use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Token;

use crate::impl_declaration::{GenericType, WildcardParam};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EitherType {
    Generic(GenericType),
    Concrete(Box<syn::Type>),
    Wildcard(WildcardParam),
}

impl EitherType {
    pub fn is_wildcard(&self) -> bool {
        matches!(self, Self::Wildcard(_))
    }
}

impl Parse for EitherType {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        // look for anyT
        let forked_input = input.fork();
        if let Ok(ident) = forked_input.parse::<syn::Ident>() {
            if ident.to_string().starts_with("any") {
                return input.parse().map(Self::Wildcard);
            }
        }

        let lookahead = input.lookahead1();
        if lookahead.peek(Token![..]) {
            input.parse().map(Self::Wildcard)
        } else {
            input.parse().map(|ty| Self::Concrete(Box::new(ty)))
        }
    }
}

impl ToTokens for EitherType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Concrete(concrete_type) => concrete_type.to_tokens(tokens),
            Self::Generic(generic_type) => generic_type.to_tokens(tokens),
            Self::Wildcard(wildcard_type) => wildcard_type.to_tokens(tokens),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse() {
        let input = quote::quote! {
            ..T
        };
        let expected = EitherType::Wildcard(WildcardParam {
            dots_token: Some(Token![..](proc_macro2::Span::call_site())),
            ident: syn::Ident::new("T", proc_macro2::Span::call_site()),
        });
        let actual = syn::parse2::<EitherType>(input).unwrap();
        assert_eq!(expected, actual);
    }
}
