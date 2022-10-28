use quote::quote;
use syn::punctuated::Punctuated;
use syn::{Ident, Path, Token};

use crate::utils::{Attribute, Parenthesized, ParseablePunctuated};

pub type ImplAttr = Attribute<Token![impl], Parenthesized<ParseablePunctuated<Ident, Token![,]>>>;

impl ImplAttr {
    const DERIVES: [(&str, &str); 4] = [
        ("Copy", "::core::marker::Copy"),
        ("Clone", "::core::clone::Clone"),
        ("Debug", "::core::fmt::Debug"),
        ("PartialEq", "::core::cmp::PartialEq"),
    ];

    pub fn traits(&self) -> impl Iterator<Item = &Ident> + '_ {
        self.inner.inner.iter()
    }

    #[must_use]
    pub fn has<T>(&self, ty: &T) -> bool
    where
        Ident: PartialEq<T>,
    {
        self.traits().any(|t| t == ty)
    }

    #[must_use]
    pub fn derives(&self) -> syn::Attribute {
        let mut traits: Punctuated<Path, Token![,]> = Punctuated::new();

        for (trait_name, path) in Self::DERIVES {
            if self.has(&trait_name) {
                traits.push(syn::parse_str::<Path>(path).unwrap());
            }
        }

        syn::Attribute {
            pound_token: self._pound_token,
            style: syn::AttrStyle::Outer,
            bracket_token: self._bracket_token,
            path: syn::parse_str("derive").unwrap(),
            tokens: quote!((#traits)),
        }
    }
}
