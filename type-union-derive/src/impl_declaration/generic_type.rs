use std::fmt;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericType {
    ident: syn::Ident,
}

impl GenericType {
    pub fn to_type(&self) -> syn::Type {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path::from(self.ident.clone()),
        })
    }
}

impl Parse for GenericType {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self::from(input.parse::<syn::TypeParam>()?))
    }
}

impl From<syn::TypeParam> for GenericType {
    fn from(syn::TypeParam { ident, .. }: syn::TypeParam) -> Self {
        Self { ident }
    }
}

impl ToTokens for GenericType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
    }
}

impl fmt::Display for GenericType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn test_display() {
        let generic_type: GenericType = syn::parse_quote!(T);

        assert_eq!(generic_type.to_string(), "T");

        let generic_type: GenericType = syn::parse_quote!(Key);

        assert_eq!(generic_type.to_string(), "Key");
    }
}
