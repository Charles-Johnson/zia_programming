mod basic_composition;
mod basic_existence;
mod basic_inference;
mod basic_reduction;
mod basic_rule;
mod existence_inference_rule;
mod inference_rule;

use crate::concepts::Concept;

pub fn check_order(concepts: &[Concept]) -> Vec<Concept> {
    concepts.iter().enumerate().map(|(i, c)| {
        assert_eq!(i, c.id());
        c.clone()
    }).collect::<Vec<Concept>>()
}
