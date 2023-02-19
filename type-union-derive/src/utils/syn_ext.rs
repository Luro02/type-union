use quote::ToTokens;
use syn::parse::{Parse, ParseBuffer, ParseStream, Peek};
use syn::punctuated::Punctuated;

pub trait ErrorExt {
    fn with_spans<I>(self, spans: I) -> Self
    where
        I: IntoIterator,
        I::Item: ToTokens;
}

impl ErrorExt for syn::Error {
    fn with_spans<I>(self, spans: I) -> Self
    where
        I: IntoIterator,
        I::Item: ToTokens,
    {
        let message = &self.to_string();

        let mut result = self;

        for item in spans {
            result.combine(syn::Error::new_spanned(item, message));
        }

        result
    }
}

pub trait PunctuatedExt<T, P> {
    #[must_use]
    fn filter<F>(self, predicate: F) -> Punctuated<T, P>
    where
        F: FnMut(&T) -> bool;

    #[must_use]
    fn map<R, F: FnMut(T) -> R>(self, f: F) -> Punctuated<R, P>;

    #[must_use]
    fn filter_map<R, F: FnMut(T) -> Option<R>>(self, f: F) -> Punctuated<R, P>;
}

impl<T, P: Default> PunctuatedExt<T, P> for Punctuated<T, P> {
    fn map<R, F: FnMut(T) -> R>(self, mut f: F) -> Punctuated<R, P> {
        self.into_iter().map(|t| f(t)).collect()
    }

    fn filter<F>(self, predicate: F) -> Punctuated<T, P>
    where
        F: FnMut(&T) -> bool,
    {
        self.into_iter().filter(predicate).collect()
    }

    fn filter_map<R, F: FnMut(T) -> Option<R>>(self, f: F) -> Punctuated<R, P> {
        self.into_iter().filter_map(f).collect()
    }
}

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

pub trait PeekAfter {
    fn peek_after<T, P: Peek>(&self, f: fn(_: ParseStream<'_>) -> syn::Result<T>, token: P)
        -> bool;
}

impl PeekAfter for ParseStream<'_> {
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
