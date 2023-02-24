use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::Token;

use crate::impl_declaration::{GenericType, Variadic};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EitherType {
    Generic(GenericType),
    Concrete(Box<syn::Type>),
    Variadic(Variadic),
}

impl Parse for EitherType {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        // look for anyT
        let forked_input = input.fork();
        if let Ok(ident) = forked_input.parse::<syn::Ident>() {
            if ident.to_string().starts_with("any") {
                return input.parse().map(Self::Variadic);
            }
        }

        let lookahead = input.lookahead1();
        if lookahead.peek(Token![..]) {
            input.parse().map(Self::Variadic)
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
            Self::Variadic(wildcard_type) => wildcard_type.to_tokens(tokens),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_from_variadic() {
        let input = quote::quote! {
            ..T
        };

        let variadic: Variadic = syn::parse_quote!(..T);
        assert_eq!(
            EitherType::Variadic(variadic),
            syn::parse2::<EitherType>(input).unwrap()
        );
    }
}
