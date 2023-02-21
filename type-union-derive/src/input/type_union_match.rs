use proc_macro2::{Ident, TokenStream};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{braced, Token};

use crate::input::TypeUnion;
use crate::utils::{assert_expr_is_type, is_macro};

#[derive(Debug, Clone)]
pub struct TypedMatchArm<T> {
    ident: Ident,
    _colon_token: syn::token::Colon,        // :
    _borrow_token: Option<syn::token::And>, // &
    _mut_token: Option<syn::token::Mut>,    // mut
    pub ty: T,
    _fat_arrow_token: syn::token::FatArrow, // =>
    pub body: syn::Expr,
}

impl<T> TypedMatchArm<T> {
    pub fn map_ty<O, F: FnOnce(T) -> O>(self, f: F) -> TypedMatchArm<O> {
        TypedMatchArm {
            ident: self.ident,
            _colon_token: self._colon_token,
            _borrow_token: self._borrow_token,
            _mut_token: self._mut_token,
            ty: f(self.ty),
            _fat_arrow_token: self._fat_arrow_token,
            body: self.body,
        }
    }
}

impl<T: ToTokens> TypedMatchArm<T> {
    pub fn into_macro_tokens(self) -> TokenStream {
        let mut result = TokenStream::new();

        self.ident.to_tokens(&mut result);
        self._colon_token.to_tokens(&mut result);
        self._borrow_token.to_tokens(&mut result);
        self._mut_token.to_tokens(&mut result);
        self.ty.to_tokens(&mut result);
        self._fat_arrow_token.to_tokens(&mut result);
        self.body.to_tokens(&mut result);

        result
    }
}

impl<T: Parse> Parse for TypedMatchArm<T> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            ident: input.parse()?,
            _colon_token: input.parse()?,
            _borrow_token: input.parse()?,
            _mut_token: input.parse()?,
            ty: input.parse()?,
            _fat_arrow_token: input.parse()?,
            body: input.parse()?,
        })
    }
}

impl ToTokens for TypedMatchArm<syn::Type> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        let ty = &self.ty;
        let body = &self.body;

        let assertion = assert_expr_is_type(
            &syn::Expr::Path(syn::ExprPath {
                attrs: vec![],
                qself: None,
                path: syn::Path::from(ident.clone()),
            }),
            &{
                if self._borrow_token.is_some() && self._mut_token.is_some() {
                    syn::Type::Reference(syn::parse_quote!(& mut #ty))
                } else if self._borrow_token.is_some() {
                    syn::Type::Reference(syn::parse_quote!(& #ty))
                } else {
                    ty.clone()
                }
            },
        );

        tokens.append_all(quote_spanned! { body.span() =>
            This :: #ty ( #ident ) => {
                let #ident = #assertion;
                #body
            }
        });
    }
}

pub struct TypeUnionMatch<T> {
    pub expr_ty: syn::ExprType,
    _brace_token: syn::token::Brace, // {}
    pub arms: Punctuated<TypedMatchArm<T>, Token![,]>,
}

impl<T> TypeUnionMatch<T> {
    pub fn new(expr_ty: syn::ExprType) -> Self {
        Self {
            expr_ty,
            _brace_token: syn::token::Brace::default(),
            arms: Punctuated::new(),
        }
    }

    fn resolve_type(ty: &syn::Type) -> syn::Result<syn::Type> {
        if let syn::Type::Macro(syn::TypeMacro { mac }) = ty {
            if is_macro(mac, "type_union") {
                return Ok(mac.parse_body::<TypeUnion<syn::Type>>()?.to_type());
            }
        } else if let syn::Type::Reference(syn::TypeReference { elem, .. }) = ty {
            return Self::resolve_type(elem.as_ref());
        }

        Err(syn::Error::new_spanned(ty, "unsupported type"))
    }
}

impl<T: ToTokens> TypeUnionMatch<T> {
    #[must_use]
    pub fn into_macro_tokens(self) -> TokenStream {
        let mut result = TokenStream::new();
        result.append_all(self.expr_ty.into_token_stream());
        self._brace_token.surround(&mut result, |result| {
            for pair in self.arms.into_pairs() {
                match pair {
                    syn::punctuated::Pair::Punctuated(arm, punct) => {
                        result.append_all(arm.into_macro_tokens());
                        punct.to_tokens(result);
                    }
                    syn::punctuated::Pair::End(arm) => {
                        result.append_all(arm.into_macro_tokens());
                    }
                }
            }
        });

        result
    }
}

impl<T> Parse for TypeUnionMatch<T>
where
    TypedMatchArm<T>: Parse,
{
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        Ok(Self {
            expr_ty: input.parse()?,
            _brace_token: braced!(content in input),
            arms: content.parse_terminated(TypedMatchArm::parse)?,
        })
    }
}

impl ToTokens for TypeUnionMatch<syn::Type> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let expr = &self.expr_ty.expr;
        let ty = &self.expr_ty.ty;
        let resolved_ty = Self::resolve_type(self.expr_ty.ty.as_ref());
        let Ok(resolved_ty) = resolved_ty else {
            tokens.append_all(resolved_ty.unwrap_err().into_compile_error());
            return;
        };

        let assertion = assert_expr_is_type(expr, ty);
        let arms = self.arms.iter();

        let result = quote! {
            {
                let __expr = #assertion;

                use #resolved_ty as This;

                match __expr {
                    #(#arms),*
                }
            }
        };

        // TODO: can (u64 | u32) be parsed as a TypeGroup?
        tokens.append_all(result);
    }
}
