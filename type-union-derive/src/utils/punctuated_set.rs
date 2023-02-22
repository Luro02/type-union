use std::hash::Hash;

use indexmap::IndexMap;
use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::punctuated::{Pair, Punctuated};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PunctuatedSet<T, P>
where
    T: Hash + Eq,
{
    inner: IndexMap<T, P>,
    last: Option<Box<T>>,
}

impl<T, P> TryFrom<Punctuated<T, P>> for PunctuatedSet<T, P>
where
    T: Hash + Eq + ToTokens,
{
    type Error = syn::Error;

    fn try_from(punctuated: Punctuated<T, P>) -> syn::Result<Self> {
        let mut result = Self::new();

        for pair in punctuated.into_pairs() {
            if result.contains_value(pair.value()) {
                return Err(syn::Error::new_spanned(
                    pair.value(),
                    "input must not contain duplicate values",
                ));
            }

            result.push_pair(pair);
        }

        Ok(result)
    }
}

impl<T, P> PunctuatedSet<T, P>
where
    T: Hash + Eq,
{
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[allow(dead_code)] // will be used in the future
    pub fn sort_by<F>(&mut self, mut cmp: F)
    where
        F: FnMut(&T, &T) -> std::cmp::Ordering,
        P: Default,
    {
        let has_last = self.last.is_some();
        if has_last {
            // temporarily add a trailing punctuation, so self.last is empty
            self.push_punct(P::default());
        }
        assert!(self.last.is_none());

        self.inner.sort_by(|k1, _, k2, _| cmp(k1, k2));

        if has_last {
            // remove the trailing punctuation and restore self.last
            self.last = Some(Box::new(self.inner.pop().unwrap().0));
        }
    }

    #[must_use]
    fn contains_value(&self, value: &T) -> bool {
        self.inner.contains_key(value) || self.last.as_deref() == Some(value)
    }

    fn is_empty_or_trailing(&self) -> bool {
        self.last.is_none()
    }

    fn push_punct(&mut self, punctuation: P) {
        assert!(
            self.last.is_some(),
            "PunctuatedSet::push_punct: cannot push punctuation if Punctuated is empty or already has trailing punctuation",
        );

        let last = self.last.take().unwrap();
        self.inner.insert(*last, punctuation);
    }

    fn push_value(&mut self, value: T) {
        assert!(
            self.is_empty_or_trailing(),
            "PunctuatedSet::push_value: cannot push value if Punctuated is missing trailing punctuation",
        );

        assert!(
            !self.contains_value(&value),
            "PunctuatedSet::push_value: cannot push duplicate value"
        );

        self.last = Some(Box::new(value));
    }

    fn push_pair(&mut self, pair: Pair<T, P>) {
        assert!(
            !self.contains_value(&pair.value()),
            "PunctuatedSet::push_pair: cannot push duplicate value"
        );
        match pair {
            Pair::Punctuated(value, punct) => {
                self.push_value(value);
                self.push_punct(punct);
            }
            Pair::End(value) => self.push_value(value),
        }
    }

    pub fn push(&mut self, value: T)
    where
        P: Default,
    {
        if !self.is_empty_or_trailing() {
            self.push_punct(Default::default());
        }

        self.push_value(value);
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.pairs().map(Pair::into_value)
    }

    fn pairs(&self) -> impl Iterator<Item = Pair<&'_ T, &'_ P>> + '_ {
        self.inner
            .iter()
            .map(|(value, punctuation)| Pair::Punctuated(value, punctuation))
            .chain(self.last.as_deref().map(Pair::End))
    }
}

pub struct IntoIter<T, P> {
    inner: indexmap::map::IntoIter<T, P>,
    last: Option<T>,
}

impl<T, P> Iterator for IntoIter<T, P> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((item, _)) = self.inner.next() {
            Some(item)
        } else if let Some(last) = self.last.take() {
            Some(last)
        } else {
            None
        }
    }
}

impl<T, P> IntoIterator for PunctuatedSet<T, P>
where
    T: Hash + Eq,
{
    type Item = T;
    type IntoIter = IntoIter<T, P>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.inner.into_iter(),
            last: self.last.map(|last| *last),
        }
    }
}

impl<T, P> Default for PunctuatedSet<T, P>
where
    T: Hash + Eq,
{
    fn default() -> Self {
        Self {
            inner: Default::default(),
            last: None,
        }
    }
}

// TODO: implement TryFromIterator instead? .map_types might introduce duplicates
impl<T, P> FromIterator<T> for PunctuatedSet<T, P>
where
    T: Hash + Eq,
    P: Default,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut result = Self::new();

        for item in iter {
            result.push(item);
        }

        result
    }
}

impl<T, P> FromIterator<Pair<T, P>> for PunctuatedSet<T, P>
where
    T: Hash + Eq,
{
    fn from_iter<I: IntoIterator<Item = Pair<T, P>>>(iter: I) -> Self {
        let mut result = Self::new();
        result.extend(iter);
        result
    }
}

impl<T, P> Extend<Pair<T, P>> for PunctuatedSet<T, P>
where
    T: Hash + Eq,
{
    fn extend<I: IntoIterator<Item = Pair<T, P>>>(&mut self, iter: I) {
        for pair in iter {
            self.push_pair(pair);
        }
    }
}

impl<T, P> ToTokens for PunctuatedSet<T, P>
where
    T: Hash + Eq + ToTokens,
    P: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(self.pairs())
    }
}
