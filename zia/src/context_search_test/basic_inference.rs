use super::Syntax;
use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextReferences, ContextSearch},
    context_search_test::ReductionReason,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{MultiThreadedContextCache, SharedDirectConceptDelta},
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

#[test]
fn basic_inference() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::<usize, SharedDirectConceptDelta>::default();
    let cache = MultiThreadedContextCache::default();
    let bound_variables = hashset! {};
    let context_search = ContextSearch::from(ContextReferences {
        snap_shot: &snapshot,
        delta: &delta,
        cache: &cache,
        bound_variable_syntax: &bound_variables,
    });
    let true_syntax = || Syntax::from("true").bind_nonquantifier_concept(1);
    let result_syntax = || Syntax::from("b").bind_nonquantifier_concept(3);
    let reduction_reason = ReductionReason::Inference {
        implication: context_search.to_ast(5),
        reason: ReductionReason::Explicit.into(),
    };
    assert_eq!(
        context_search.reduce(&result_syntax().into()),
        Some((true_syntax().into(), reduction_reason.clone()))
    );

    assert_eq!(
        context_search.recursively_reduce(&result_syntax().into()),
        (true_syntax().into(), Some(reduction_reason))
    );
}

fn labels() -> HashMap<ConceptId, &'static str> {
    hashmap! {
        0 => "implication",
        1 => "true",
        2 => "a",
        3 => "b",
    }
}

fn concepts() -> [Concept<ConceptId>; 10] {
    let mut implication_concept = (ConcreteConceptType::Implication, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut condition_concept: Concept<ConceptId> =
        (SpecificPart::default(), 2).into();
    let mut result_concept = (SpecificPart::default(), 3).into();
    condition_concept.make_reduce_to(&mut true_concept);
    let mut implies_result_concept = Concept::composition_of(
        4,
        &mut implication_concept,
        &mut result_concept,
    );
    let mut condition_implies_result_concept = Concept::composition_of(
        5,
        &mut condition_concept,
        &mut implies_result_concept,
    );
    condition_implies_result_concept.make_reduce_to(&mut true_concept);
    [
        implication_concept,
        true_concept,
        condition_concept,
        result_concept,
        implies_result_concept,
        condition_implies_result_concept,
        (ConcreteConceptType::Associativity, 6).into(),
        (ConcreteConceptType::Left, 7).into(),
        (ConcreteConceptType::Precedence, 8).into(),
        (ConcreteConceptType::Right, 9).into(),
    ]
}
