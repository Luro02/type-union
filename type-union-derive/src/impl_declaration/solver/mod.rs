mod inferred_value;
mod solver;
mod type_mapping;
mod type_union_set;
mod unresolved_type_mapping;

pub use inferred_value::*;
pub use solver::*;
pub use type_mapping::*;
pub use type_union_set::*;
pub use unresolved_type_mapping::*;

use indexmap::{IndexMap, IndexSet};
use std::hash::Hash;
use std::ops::{Add, AddAssign};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct SetId {
    id: usize,
}

impl Add<usize> for SetId {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self { id: self.id + rhs }
    }
}

impl AddAssign<usize> for SetId {
    fn add_assign(&mut self, rhs: usize) {
        *self = *self + rhs;
    }
}

pub trait IndexMapExt<K, V> {
    fn insert_or_intersection(&mut self, key: K, value: V);
}

impl<K, V> IndexMapExt<K, IndexSet<V>> for IndexMap<K, IndexSet<V>>
where
    K: Eq + Hash,
    V: Eq + Hash,
{
    // TODO: use this more
    fn insert_or_intersection(&mut self, key: K, value: IndexSet<V>) {
        if let Some(existing) = self.get_mut(&key) {
            existing.retain(|v| value.contains(v));
        } else {
            self.insert(key, value);
        }
    }
}
