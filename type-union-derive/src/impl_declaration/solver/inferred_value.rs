use std::hash::Hash;
use std::mem;

use indexmap::IndexSet;

#[derive(Debug, Clone)]
pub enum InferredValue<V> {
    /// The value must be this one and cannot be anything else.
    Fixed(V),
    /// The value can be any of these values.
    Any(IndexSet<V>),
    /// The value is right now unknown.
    Unknown,
}

impl<V: Hash + Eq> InferredValue<V> {
    pub fn as_ref(&self) -> InferredValue<&V> {
        match self {
            Self::Fixed(value) => InferredValue::Fixed(value),
            Self::Any(values) => InferredValue::Any(values.iter().collect()),
            Self::Unknown => InferredValue::Unknown,
        }
    }

    pub fn map<U, F>(self, mut f: F) -> InferredValue<U>
    where
        F: FnMut(V) -> U,
        U: Hash + Eq,
    {
        match self {
            Self::Fixed(value) => InferredValue::Fixed(f(value)),
            Self::Any(values) => InferredValue::Any(values.into_iter().map(f).collect()),
            Self::Unknown => InferredValue::Unknown,
        }
    }

    /// Returns `true` if the value is unknown, `false` otherwise.
    pub const fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    pub fn take(&mut self) -> InferredValue<V> {
        mem::replace(self, InferredValue::Unknown)
    }
}

impl<V: Hash + Eq> InferredValue<&V> {
    pub fn cloned(self) -> InferredValue<V>
    where
        V: Clone,
    {
        match self {
            Self::Fixed(value) => InferredValue::Fixed(value.clone()),
            Self::Any(values) => InferredValue::Any(values.into_iter().cloned().collect()),
            Self::Unknown => InferredValue::Unknown,
        }
    }
}
