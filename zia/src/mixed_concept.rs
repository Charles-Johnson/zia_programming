use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait ConceptId: Clone + Copy + Debug + Display + Eq + Hash {}
pub trait MixedConcept: ConceptId {
    fn uncommitted(id: usize) -> Self;
}
