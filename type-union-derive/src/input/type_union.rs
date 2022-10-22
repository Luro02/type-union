use std::collections::HashSet;

use proc_macro2::{Ident, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{Token, Type};

use crate::utils::{resolve_type_name, resolve_type_union_name, ParseStreamExt};

pub struct TypeUnionInput {
    punctuated: Punctuated<Type, Token![|]>,
}

impl TypeUnionInput {
    #[must_use]
    pub fn ident(&self) -> syn::Ident {
        resolve_type_union_name(self.punctuated.iter())
    }

    #[must_use]
    pub fn types(&self) -> impl Iterator<Item = &Type> + '_ {
        self.punctuated.iter()
    }
}

fn parse_type(visited: &mut HashSet<Ident>, input: ParseStream<'_>) -> syn::Result<Type> {
    let ty = input.parse::<Type>()?;
    let ident = resolve_type_name(&ty).ok_or_else(|| input.error("could not resolve type name"))?;

    if visited.contains(&ident) {
        return Err(input.error("duplicate type name"));
    }

    Ok(ty)
}

impl Parse for TypeUnionInput {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut visited = HashSet::new();

        Ok(Self {
            punctuated: input.parse_terminated2(|input| parse_type(&mut visited, input))?,
        })
    }
}

impl ToTokens for TypeUnionInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(self.ident())
    }
}
