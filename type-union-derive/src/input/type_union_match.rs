use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{braced, parenthesized, Expr, Token, Type};

use crate::utils::{assert_ident_is_type, resolve_type_union_name};

#[derive(Debug, Clone)]
pub struct TypeUnionMatchArm {
    ident: Ident,
    _colon_token: Token![:],
    _borrow_token: Option<Token![&]>,
    ty: Type,
    _fat_arrow_token: Token![=>],
    body: Expr,
}

impl Parse for TypeUnionMatchArm {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            ident: input.parse()?,
            _colon_token: input.parse()?,
            _borrow_token: input.parse()?,
            ty: input.parse()?,
            _fat_arrow_token: input.parse()?,
            body: input.parse()?,
        })
    }
}

impl ToTokens for TypeUnionMatchArm {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        let ty = &self.ty;
        let body = &self.body;

        let assertion = assert_ident_is_type(ident, &{
            if self._borrow_token.is_some() {
                Type::Reference(syn::parse2(quote!(&#ty)).unwrap())
            } else {
                ty.clone()
            }
        });

        tokens.append_all(quote! {
            This :: #ty ( #ident ) => {
                let #ident = #assertion;
                #body
            }
        });
    }
}

pub struct TypeUnionMatch {
    ident: Ident,
    _colon_token: Token![:],
    _borrow_token: Option<Token![&]>,
    _paren_token: syn::token::Paren,
    types: Punctuated<Type, Token![|]>,
    _brace_token: syn::token::Brace,
    arms: Punctuated<TypeUnionMatchArm, Token![,]>,
}

impl Parse for TypeUnionMatch {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        let other_content;
        Ok(Self {
            ident: input.parse()?,
            _colon_token: input.parse()?,
            _borrow_token: input.parse()?,
            _paren_token: parenthesized!(other_content in input),
            types: other_content.parse_terminated(Type::parse)?,
            _brace_token: braced!(content in input),
            arms: content.parse_terminated(TypeUnionMatchArm::parse)?,
        })
    }
}

impl ToTokens for TypeUnionMatch {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        let arms = self.arms.iter();

        let real_type = resolve_type_union_name(self.types.iter());
        let ty_path = Type::Path(syn::parse2(quote!(#real_type)).unwrap());
        let assertion = assert_ident_is_type(ident, &{
            if self._borrow_token.is_some() {
                Type::Reference(syn::parse2(quote!(&#ty_path)).unwrap())
            } else {
                ty_path
            }
        });

        tokens.append_all(quote! {
            {
                let #ident = #assertion;

                use #real_type as This;
                match #ident {
                    #(#arms),*
                }
            }
        });
    }
}
