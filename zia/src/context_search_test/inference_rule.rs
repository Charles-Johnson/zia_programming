use super::Syntax;
use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta::NestedContextDelta,
    context_search::{ContextReferences, ContextSearch},
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        MultiThreadedContextCache, SharedContextDelta, SharedDirectConceptDelta,
    },
};
use maplit::{hashmap, hashset};
use pretty_assertions::assert_eq;
use std::collections::HashMap;

fn concepts() -> [Concept<usize>; 15] {
    let mut implication_concept = (ConcreteConceptType::Implication, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut concept_a = (SpecificPart::default(), 3).into();
    let mut example_concept = (SpecificPart::default(), 7).into();
    let mut variable_concept = (SpecificPart::free_variable(), 9).into();
    let mut concept_b = (SpecificPart::default(), 10).into();
    let mut cause_concept =
        Concept::composition_of(4, &mut concept_a, &mut variable_concept);
    let mut result_concept =
        Concept::composition_of(2, &mut variable_concept, &mut concept_b);
    let mut implies_result_concept = Concept::composition_of(
        5,
        &mut implication_concept,
        &mut result_concept,
    );
    let mut cause_implies_result_concept = Concept::composition_of(
        6,
        &mut cause_concept,
        &mut implies_result_concept,
    );
    cause_implies_result_concept.make_reduce_to(&mut true_concept);
    let mut example_composition =
        Concept::composition_of(8, &mut concept_a, &mut example_concept);
    example_composition.make_reduce_to(&mut true_concept);
    [
        implication_concept,
        true_concept,
        result_concept,
        concept_a,
        cause_concept,
        implies_result_concept,
        cause_implies_result_concept,
        example_concept,
        example_composition,
        variable_concept,
        concept_b,
        (ConcreteConceptType::Precedence, 11).into(),
        (ConcreteConceptType::Associativity, 12).into(),
        (ConcreteConceptType::Left, 13).into(),
        (ConcreteConceptType::Right, 14).into(),
    ]
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "=>",
        1 => "true",
        3 => "a",
        7 => "example",
        9 => "_x_",
        10 => "b",
        11 => "prec",
        12 => "assoc",
        13 => "left",
        14 => "right"
    }
}

#[test]
fn inference_rule() {
    let context_cache = MultiThreadedContextCache::default();
    let context_delta = NestedContextDelta::<
        _,
        SharedDirectConceptDelta<ConceptId>,
        _,
    >::default();
    let context_snap_shot = MockSnapShot::new_test_case(&concepts(), &labels());
    let bound_variable_syntax = hashset! {};
    let context_search = ContextSearch::from(ContextReferences {
        snap_shot: &context_snap_shot,
        delta: SharedContextDelta(context_delta.into()),
        cache: &context_cache,
        bound_variable_syntax: &bound_variable_syntax,
    });
    let example_syntax =
        Syntax::new_pair(context_search.to_ast(&7), context_search.to_ast(&10));
    let true_syntax = context_search.to_ast(&1);
    assert_eq!(
        context_search.reduce(&example_syntax.into()).unwrap().0,
        true_syntax,
    );
}
