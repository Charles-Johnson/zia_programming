use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait MixedConcept: Clone + Copy + Debug + Display + Eq + Hash {
    fn uncommitted(id: usize) -> Self;
}
