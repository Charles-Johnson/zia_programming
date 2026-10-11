mod basic_comparison;
mod basic_composition;
mod basic_existence;
mod basic_inference;
mod basic_reduction;
mod basic_rule;
mod comparison_existence_implication_rule;
mod existence_inference_rule;
mod implied_reduction_via_implication_chain;
mod inference_rule;
mod inferred_negation;
mod nested_composition_rule;
mod not;

use std::{collections::HashSet, sync::Arc};

use dashmap::DashMap;

use crate::{
    ast::SyntaxKey,
    concepts::{Concept, ConceptTrait},
    context_delta::NestedDelta,
    context_search::{ContextReferences, ContextSearch},
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        ArcFamily, MTContextCache, MTContextSearch, MTReductionReason,
        MTSyntaxTree,
    },
};

pub fn check_order(concepts: &[Concept<usize>]) -> Vec<Concept<ConceptId>> {
    concepts
        .iter()
        .enumerate()
        .map(|(i, c)| {
            assert_eq!(i, c.id());
            c.clone().into()
        })
        .collect::<Vec<Concept<ConceptId>>>()
}
type ContextSearchTest<'a, 'b> =
    ContextSearch<'a, 'b, MockSnapShot, ConceptId, ArcFamily>;
type Syntax = MTSyntaxTree<ConceptId>;
type ReductionReason = MTReductionReason<ConceptId>;
fn new_context_search_test<'a, 'b>(
    snap_shot: &'a MockSnapShot,
    bound_variables: &'b HashSet<SyntaxKey<ConceptId>>,
) -> ContextSearchTest<'a, 'b> {
    let delta = NestedDelta::<ConceptId, _>::default();
    let cache = MTContextCache::default();

    MTContextSearch::from(ContextReferences {
        snap_shot,
        delta: delta.into(),
        cache: &cache,
        bound_variable_syntax: bound_variables,
        half_generalisation_cache: Arc::new(DashMap::new()),
    })
}
