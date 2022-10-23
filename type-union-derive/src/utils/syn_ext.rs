use std::ops::{Deref, DerefMut};

use syn::parse::{Parse, ParseBuffer, ParseStream};
use syn::punctuated::Punctuated;
use syn::{bracketed, parenthesized, AttrStyle, Token};

pub trait ParseStreamExt<'a> {
    /// # Why does this exist?
    ///
    /// <https://github.com/dtolnay/syn/issues/697>
    fn parse_terminated2<T, P: Parse, F>(&self, parser: F) -> syn::Result<Punctuated<T, P>>
    where
        for<'b> F: FnMut(ParseStream<'b>) -> syn::Result<T>;
}

impl<'a> ParseStreamExt<'a> for ParseBuffer<'a> {
    fn parse_terminated2<T, P: Parse, F>(&self, parser: F) -> syn::Result<Punctuated<T, P>>
    where
        for<'b> F: FnMut(ParseStream<'b>) -> syn::Result<T>,
    {
        <ParseStream<'_> as ParseStreamExt<'_>>::parse_terminated2(&self, parser)
    }
}

impl<'a> ParseStreamExt<'a> for ParseStream<'a> {
    fn parse_terminated2<T, P: Parse, F>(&self, mut parser: F) -> syn::Result<Punctuated<T, P>>
    where
        for<'b> F: FnMut(ParseStream<'b>) -> syn::Result<T>,
    {
        // NOTE: source code has been copied from Punctuated::parse_terminated_with
        let mut punctuated = Punctuated::new();

        loop {
            if self.is_empty() {
                break;
            }

            let value = parser(self)?;
            punctuated.push_value(value);
            if self.is_empty() {
                break;
            }
            let punct = self.parse()?;
            punctuated.push_punct(punct);
        }

        Ok(punctuated)
    }
}

pub struct Attribute<P, T> {
    pub _pound_token: Token![#],
    pub _style: AttrStyle,
    pub _bracket_token: syn::token::Bracket,
    pub _path: P,
    pub inner: T,
}

pub struct Parenthesized<T> {
    _paren_token: syn::token::Paren,
    pub inner: T,
}

impl<T: Parse> Parse for Parenthesized<T> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        Ok(Self {
            _paren_token: parenthesized!(content in input),
            inner: content.parse()?,
        })
    }
}

impl<P: Parse, T: Parse> Parse for Attribute<P, T> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let brack_content;
        Ok(Self {
            _pound_token: input.parse()?,
            _style: {
                if input.peek(Token![!]) {
                    AttrStyle::Inner(input.parse::<Token![!]>()?)
                } else {
                    AttrStyle::Outer
                }
            },
            _bracket_token: bracketed!(brack_content in input),
            _path: brack_content.parse()?,
            inner: brack_content.parse()?,
        })
    }
}

pub struct ParseablePunctuated<T, P>(pub Punctuated<T, P>);

impl<T, P> Deref for ParseablePunctuated<T, P> {
    type Target = Punctuated<T, P>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, P> DerefMut for ParseablePunctuated<T, P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Parse, P: Parse> Parse for ParseablePunctuated<T, P> {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        Ok(Self(input.parse_terminated(T::parse)?))
    }
}
