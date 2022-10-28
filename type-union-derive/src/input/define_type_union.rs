use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use syn::parse::{Parse, ParseStream, Peek};
use syn::punctuated::Punctuated;
use syn::{bracketed, Ident, ItemImpl, Token, Type, TypeParamBound};

use crate::input::{TypeUnion, TypeUnionDeclaration};
use crate::utils::unique_pairs;

enum Definition {
    Declaration(TypeUnionDeclaration),
    Impl(Implementation),
}

pub trait PeekAfter {
    fn peek_after<T, P: Peek>(&self, f: fn(_: ParseStream<'_>) -> syn::Result<T>, token: P)
        -> bool;
}

impl PeekAfter for ParseStream<'_> {
    // TODO: this is an anti-pattern
    fn peek_after<T, P: Peek>(
        &self,
        f: fn(_: ParseStream<'_>) -> syn::Result<T>,
        token: P,
    ) -> bool {
        let clone = self.fork();
        let _ = f(&clone);
        clone.peek(token)
    }
}

pub struct Implementation {
    attr: DeclareImplAttr,
    item_impl: ItemImpl,
}

impl Implementation {
    pub fn item_impl(&self) -> &ItemImpl {
        &self.item_impl
    }

    pub fn alias(&self) -> &Ident {
        &self.attr.alias
    }
}

#[derive(Debug)]
pub struct DeclareImplAttr {
    _pound_token: Token![#],
    _bracket_token: syn::token::Bracket,
    _ident: syn::Ident,
    _eq_token: Token![=],
    alias: syn::Ident,
}

impl Parse for DeclareImplAttr {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        Ok(Self {
            _pound_token: input.parse()?,
            _bracket_token: bracketed!(content in input),
            _ident: {
                let ident = content.parse()?;

                if &ident != "declare_impl" {
                    return Err(content.error("expected `declare_impl`"));
                }

                ident
            },
            _eq_token: content.parse()?,
            alias: content.parse()?,
        })
    }
}

impl Parse for Implementation {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            attr: dbg!(input.parse()?),
            item_impl: input.parse()?,
        })
    }
}

fn is_impl(input: ParseStream<'_>) -> bool {
    input.parse::<DeclareImplAttr>().is_ok() && !input.peek(Token![enum])
}

impl Parse for Definition {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let forked = input.fork();
        if dbg!((&forked).peek_after(syn::Attribute::parse_outer, Token![impl])) {
            Ok(Self::Impl(input.parse()?))
        } else {
            Ok(Self::Declaration(input.parse()?))
        }
    }
}

pub struct DefineTypeUnion {
    punctuated: Punctuated<Definition, Token![;]>,
}

impl DefineTypeUnion {
    pub fn declarations(&self) -> impl Iterator<Item = &TypeUnionDeclaration> + '_ {
        self.punctuated.iter().filter_map(|d| match d {
            Definition::Declaration(decl) => Some(decl),
            _ => None,
        })
    }

    pub fn impls(&self) -> impl Iterator<Item = &Implementation> + '_ {
        self.punctuated.iter().filter_map(|d| match d {
            Definition::Impl(v) => Some(v),
            _ => None,
        })
    }
}

impl Parse for DefineTypeUnion {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self {
            punctuated: input.parse_terminated(Definition::parse)?,
        })
    }
}

#[must_use]
fn generate_partial_eq_impl(
    a: &TypeUnionDeclaration,
    b: &TypeUnionDeclaration,
) -> proc_macro2::TokenStream {
    let mut mapping: Vec<((Ident, Ident), Type)> = Vec::new();

    for (variant, ty) in a.variants() {
        for (other_variant, other_ty) in b.variants() {
            if &ty == &other_ty {
                mapping.push(((variant.clone(), other_variant), ty.clone()));
            }
        }
    }

    if !mapping.is_empty() {
        let a_ident = a.ident();
        let b_ident = b.ident();

        let a_variants = mapping.iter().map(|((a, _), _)| a);
        let b_variants = mapping.iter().map(|((_, b), _)| b);

        return quote! {
            impl ::core::cmp::PartialEq<#a_ident> for #b_ident {
                fn eq(&self, other: &#a_ident) -> bool {
                    use #a_ident as ThisB;
                    use #b_ident as ThisA;

                    match (self, other) {
                        #(
                            (
                                ThisA::#a_variants (a),
                                ThisB::#b_variants (b)
                            ) => {
                                a.eq(b)
                            }
                        )*
                        _ => false,
                    }
                }
            }

            impl PartialEq<#b_ident> for #a_ident {
                fn eq(&self, other: &#b_ident) -> bool {
                    other.eq(self)
                }
            }
        };
    }

    quote!()
}

impl ToTokens for DefineTypeUnion {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut with_partialeqs = Vec::new();
        for declaration in self.declarations() {
            if declaration.has_trait(&"PartialEq") {
                with_partialeqs.push(declaration);
            }

            tokens.append_all(quote!(
                #declaration
            ));
        }

        for (a, b) in unique_pairs(with_partialeqs) {
            tokens.append_all(generate_partial_eq_impl(a, b));
        }

        for impl_ in self.impls() {
            let template = TemplateImpl::from(impl_);

            for decl in self.declarations() {
                if decl.has_trait(template.alias()) {
                    let t = template
                        .apply(&decl)
                        .unwrap_or_else(|e| e.to_compile_error());

                    tokens.append_all(t);
                }
            }
        }
    }
}

// TODO: how about supporting:
/*
impl<#[distinct_repeat] T> Iterator for type_union!(T)
where
    T: Iterator,
{
    // TODO: this would declare a new type_union?
    // TODO: what about the case where T::Item must not be a type_union, because all variants are the same?
    // TODO: this macro knows about T, but not about T::Item?
    #[declare]
    type Item = type_union!(<T as Iterator>::Item);

    fn next(&mut self) -> Option<Self::Item> {
        match_type_union!(self: &(T) {
            value: &T => {
                value.next().map(Self::Item::from)
            },
        });
    }
}
*/

pub struct TemplateImpl<'a> {
    item: &'a ItemImpl,
    alias: &'a Ident,
}

impl<'a> From<&'a Implementation> for TemplateImpl<'a> {
    fn from(impl_: &'a Implementation) -> Self {
        Self {
            item: impl_.item_impl(),
            alias: impl_.alias(),
        }
    }
}

pub trait OptionExt<T> {
    #[must_use]
    fn to_tokens(&self) -> TokenStream;
}

impl<T> OptionExt<T> for Option<T>
where
    T: ToTokens,
{
    fn to_tokens(&self) -> TokenStream {
        match self {
            Some(v) => quote!(#v),
            None => TokenStream::new(),
        }
    }
}

pub trait GenericsExt {
    #[must_use]
    fn constraints<T>(&self, ty: &T) -> Vec<TypeParamBound>
    where
        Ident: PartialEq<T>;
}

impl GenericsExt for syn::Generics {
    fn constraints<T>(&self, ident: &T) -> Vec<TypeParamBound>
    where
        Ident: PartialEq<T>,
    {
        let mut result = Vec::new();

        for param in &self.params {
            if let syn::GenericParam::Type(ty_param) = param {
                if &ty_param.ident == ident {
                    result.extend(ty_param.bounds.clone());
                }
            }
        }

        for predicate in self.where_clause.iter().flat_map(|v| v.predicates.iter()) {
            if let syn::WherePredicate::Type(ty_param) = predicate {
                // TODO: what about `u16: From<anyT>`?
                if let Type::Path(ty_path) = &ty_param.bounded_ty {
                    if ty_path.path.get_ident().map_or(false, |i| i == ident) {
                        result.extend(ty_param.bounds.clone());
                    }
                }
            }
        }

        result
    }
}

pub trait IdentExt {
    #[must_use]
    fn ident(&self) -> Option<&Ident>;
}

impl IdentExt for syn::Path {
    fn ident(&self) -> Option<&Ident> {
        self.get_ident()
    }
}

impl<'a> TemplateImpl<'a> {
    pub fn alias(&self) -> &Ident {
        self.alias
    }

    // TODO: finish this up, so anyT syntax works
    pub fn apply(&self, ty: &TypeUnionDeclaration) -> syn::Result<TokenStream> {
        // TODO: defaultness, unsafety tokens
        let ItemImpl {
            attrs,
            defaultness,
            unsafety,
            impl_token,
            generics,
            trait_,
            self_ty,
            items,
            ..
        } = &self.item;

        let trait_ = {
            if let Some((bang, path, for_token)) = trait_ {
                let bang = bang.map_or_else(|| quote!(), |b| quote!(#b));

                quote!(#bang #path #for_token)
            } else {
                return Err(syn::Error::new_spanned(
                    &impl_token,
                    "missing trait for impl",
                ));
            }
        };

        let self_ty = {
            if let Type::Macro(self_ty) = &**self_ty {
                self_ty.clone().mac
            } else {
                return Err(syn::Error::new_spanned(
                    &self_ty,
                    "trait must be implemented on type_union macro invocation",
                ));
            }
        };

        // check that the macro invocation is of the correct macro:
        if self_ty.path.segments.last().map(|s| s.ident.to_string())
            != Some("type_union".to_string())
        {
            // TODO: this might be missing the span!
            return Err(syn::Error::new_spanned(
                &self_ty.path.segments.last(),
                "trait must be implemented on type_union macro invocation",
            ));
        }

        // TODO: return error instead
        let type_union = syn::parse2::<TypeUnion>(self_ty.tokens)?;

        let generics = quote!();
        // TODO: zip will stop if left or right iter returns none, so it might accept too much input?
        for (lty, rty) in type_union.types().zip(ty.type_union().types()) {
            if let Type::Path(path) = lty {
                if path.path.segments.last().map(|s| s.ident.to_string())
                    == Some("anyT".to_string())
                {
                    // TODO: all further rty could be ignored now?
                    // TODO: what about constraints on anyT?
                    // A proc-macro has no information about which traits a type implements?
                    // -> implementing custom traits will be opt-in for types, so #[impl(Iterator)] for example must be used
                    // then this function can just assume that the traits are satisified

                    // TODO: emit constraints on remaining rty?
                    break;
                }
            }

            if lty != rty {
                // TODO: improve error message
                return Err(syn::Error::new_spanned(rty, "lty and rty are different"));
            }
        }

        let ident = ty.ident();
        Ok(dbg!(quote! {
            #impl_token #trait_ #ident {
                #(#items)*
            }
        }))
    }
}
