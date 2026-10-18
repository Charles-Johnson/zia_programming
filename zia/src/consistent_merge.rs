use std::{collections::hash_map::Entry, mem::swap};

use crate::{
    mixed_concept::ConceptId, nester::SharedReference,
    substitute::Substitutions, variable_mask_list::VariableMask,
};

pub trait ConsistentMerge<SR>: Sized {
    type Output;
    fn consistent_merge(self, other: Self) -> Option<Self::Output>;
}

impl<CI: ConceptId, SR: SharedReference> ConsistentMerge<SR>
    for Substitutions<CI, SR>
{
    type Output = Self;

    fn consistent_merge(self, other: Self) -> Option<Self::Output> {
        let mut small = self;
        let mut large = other;
        if small.len() > large.len() {
            swap(&mut small, &mut large);
        }
        for (key, value) in small {
            match large.entry(key) {
                Entry::Occupied(e) => {
                    if e.get().key() != value.key() {
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
impl<CI: ConceptId, SR: SharedReference> ConsistentMerge<SR>
    for VariableMask<CI, SR>
{
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
                    if e.get().key() != value.key() {
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
