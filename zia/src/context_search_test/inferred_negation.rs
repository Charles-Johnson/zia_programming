use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta::NestedDelta,
    context_search::ContextReferences,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        MTContextSearch, MultiThreadedContextCache, SharedContextDelta,
        SharedDirectConceptDelta,
    },
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

fn concepts() -> [Concept<usize>; 14] {
    let mut implication_concept = (ConcreteConceptType::Implication, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut concept_a = (SpecificPart::default(), 3).into();
    let mut concept_b = (SpecificPart::default(), 7).into();
    let mut reduction_concept = (ConcreteConceptType::Reduction, 4).into();
    let mut false_concept = (ConcreteConceptType::False, 12).into();
    let mut reduces_to_false =
        Concept::composition_of(13, &mut reduction_concept, &mut false_concept);
    let mut result_concept =
        Concept::composition_of(2, &mut concept_b, &mut reduces_to_false);
    let mut implies_result_concept = Concept::composition_of(
        5,
        &mut implication_concept,
        &mut result_concept,
    );
    let mut cause_implies_result_concept =
        Concept::composition_of(6, &mut concept_a, &mut implies_result_concept);
    concept_a.make_reduce_to(&mut true_concept); // a -> true
    cause_implies_result_concept.make_reduce_to(&mut true_concept); // a => b -> false
    [
        implication_concept,
        true_concept,
        result_concept,
        concept_a,
        reduction_concept,
        implies_result_concept,
        cause_implies_result_concept,
        concept_b,
        (ConcreteConceptType::Precedence, 8).into(),
        (ConcreteConceptType::Associativity, 9).into(),
        (ConcreteConceptType::Left, 10).into(),
        (ConcreteConceptType::Right, 11).into(),
        false_concept,
        reduces_to_false,
    ]
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {}
}

#[test]
fn inferred_negation() {
    let context_cache = MultiThreadedContextCache::default();
    let context_delta =
        NestedDelta::<_, SharedDirectConceptDelta<ConceptId>, _, _>::default();
    let context_snap_shot = MockSnapShot::new_test_case(&concepts(), &labels());
    let bound_variable_syntax = hashset! {};
    let context_search = MTContextSearch::from(ContextReferences {
        snap_shot: &context_snap_shot,
        delta: SharedContextDelta(context_delta.into()),
        cache: &context_cache,
        bound_variable_syntax: &bound_variable_syntax,
    });
    let (reduction, _) = context_search
        .find_examples_of_inferred_reduction(&context_search.to_ast(&7))
        .unwrap();
    assert_eq!(reduction.get_concept().unwrap(), 12.into());
}
