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
    /// No value can be inferred, because of conflicting conditions.
    Invalid,
}

impl<V: Hash + Eq> InferredValue<V> {
    #[must_use]
    pub fn from_possible_values(values: IndexSet<V>) -> Self {
        match values.len() {
            0 => Self::Invalid,
            1 => Self::Fixed(values.into_iter().next().unwrap()),
            _ => Self::Any(values),
        }
    }

    pub fn take(&mut self) -> InferredValue<V> {
        mem::replace(self, InferredValue::Unknown)
    }
}
