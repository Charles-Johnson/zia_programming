mod basic_comparison;
mod basic_composition;
mod basic_existence;
mod basic_inference;
mod basic_reduction;
mod basic_rule;
mod comparison_existence_implication_rule;
mod existence_inference_rule;
mod inference_rule;
mod nested_composition_rule;

use crate::{
    concepts::{Concept, ConceptTrait},
    mock_snap_shot::ConceptId,
    multi_threaded::{MultiThreadedReductionReason, MultiThreadedSyntaxTree},
};

pub fn check_order(concepts: &[Concept<ConceptId>]) -> Vec<Concept<ConceptId>> {
    concepts
        .iter()
        .enumerate()
        .map(|(i, c)| {
            assert_eq!(i, c.id());
            c.clone()
        })
        .collect::<Vec<Concept<ConceptId>>>()
}

type Syntax = MultiThreadedSyntaxTree<ConceptId>;
type ReductionReason = MultiThreadedReductionReason<Syntax>;
