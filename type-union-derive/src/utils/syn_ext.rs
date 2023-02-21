use std::mem;

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

pub struct DrainFilter<'a, T, P, F> {
    punctuated: &'a mut Punctuated<T, P>,
    filter: F,
    index: usize,
}

impl<'a, T, P: Default, F: FnMut(&mut T) -> bool> Iterator for DrainFilter<'a, T, P, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.punctuated.len() {
            return None;
        }

        let should_remove = {
            let item = self.punctuated.get_mut(self.index).unwrap();
            (self.filter)(item)
        };

        let mut result = None;
        if should_remove {
            // the element is removed and returned
            result = Some(self.punctuated.remove(self.index));

            // the index must not be incremented, because
            // the next element will now be at the index of the removed element
        } else {
            // the element is kept => increment the index
            self.index += 1;
        }

        result
    }
}

// TODO: test the extension methods?
pub trait PunctuatedExt<T, P> {
    #[must_use]
    fn get_mut(&mut self, index: usize) -> Option<&mut T>;

    fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&T) -> bool;

    /// Removes all elements for which the provided function returns `true`.
    fn drain_filter<F>(&mut self, f: F) -> DrainFilter<'_, T, P, F>
    where
        F: FnMut(&mut T) -> bool;

    /// Removes the element at the given index and returns it.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    fn remove(&mut self, index: usize) -> T;

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
    fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.iter_mut().nth(index)
    }

    fn drain_filter<F>(&mut self, f: F) -> DrainFilter<'_, T, P, F>
    where
        F: FnMut(&mut T) -> bool,
    {
        DrainFilter {
            punctuated: self,
            filter: f,
            index: 0,
        }
    }

    fn remove(&mut self, index: usize) -> T {
        let mut result = None;

        // TODO: improve performance
        *self = mem::take(self)
            .into_pairs()
            .enumerate()
            .filter_map(|(i, pair)| {
                if i == index {
                    result = Some(pair.into_value());
                    None
                } else {
                    Some(pair)
                }
            })
            .collect();

        result.expect("index out of bounds")
    }

    fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
        *self = mem::take(self)
            .into_pairs()
            .filter(|pair| f(pair.value()))
            .collect();
    }

    // TODO: does map, filter and filter_map preserve the original punctuation?
    fn map<R, F: FnMut(T) -> R>(self, f: F) -> Punctuated<R, P> {
        self.into_iter().map(f).collect()
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
