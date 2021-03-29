use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    mem::swap,
};

pub(crate) trait ConsistentMerge: Sized {
    type Output;
    fn consistent_merge(self, other: Self) -> Option<Self::Output>;
}

impl<K: Eq + Hash, V: PartialEq> ConsistentMerge for HashMap<K, V> {
    type Output = Self;

    fn consistent_merge(self, other: Self) -> Option<Self> {
        let mut small = self;
        let mut large = other;
        if small.len() > large.len() {
            swap(&mut small, &mut large);
        }
        for (key, value) in small {
            match large.entry(key) {
                Entry::Occupied(e) => {
                    if e.get() != &value {
                        // inconsistent
                        return None;
                    }
                },
                Entry::Vacant(e) => {
                    e.insert(value);
                },
            }
        }
        Some(large)
    }
}

impl<K: Eq + Hash + Clone, V: Clone + PartialEq> ConsistentMerge
    for &HashMap<K, V>
{
    type Output = HashMap<K, V>;

    fn consistent_merge(self, other: Self) -> Option<Self::Output> {
        let (small, mut large) = if self.len() > other.len() {
            (other, self.clone())
        } else {
            (self, other.clone())
        };
        for (key, value) in small {
            match large.entry(key.clone()) {
                Entry::Occupied(e) => {
                    if e.get() != value {
                        // inconsistent
                        return None;
                    }
                },
                Entry::Vacant(e) => {
                    e.insert(value.clone());
                },
            }
        }
        Some(large)
    }
}
