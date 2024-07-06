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

fn concepts() -> [Concept<usize>; 17] {
    let mut implication_concept = (ConcreteConceptType::Implication, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut variable = (SpecificPart::free_variable(), 3).into();
    let mut not = (SpecificPart::default(), 7).into();
    let mut reduction_concept = (ConcreteConceptType::Reduction, 4).into();
    let mut false_concept = (ConcreteConceptType::False, 12).into();
    let mut reduces_to_false =
        Concept::composition_of(13, &mut reduction_concept, &mut false_concept);
    let mut result_concept =
        Concept::composition_of(2, &mut variable, &mut reduces_to_false);
    let mut implies_result_concept = Concept::composition_of(
        5,
        &mut implication_concept,
        &mut result_concept,
    );
    let mut not_variable = Concept::composition_of(16, &mut not, &mut variable);
    let mut cause_implies_result_concept = Concept::composition_of(
        6,
        &mut not_variable,
        &mut implies_result_concept,
    );
    let mut concept_c = (SpecificPart::default(), 14).into();
    let mut not_concept_c =
        Concept::composition_of(15, &mut not, &mut concept_c);
    not_concept_c.make_reduce_to(&mut true_concept); // not c -> true
    cause_implies_result_concept.make_reduce_to(&mut true_concept); // not _x_ => _x_ -> false
    [
        implication_concept,
        true_concept,
        result_concept,
        variable,
        reduction_concept,
        implies_result_concept,
        cause_implies_result_concept,
        not,
        (ConcreteConceptType::Precedence, 8).into(),
        (ConcreteConceptType::Associativity, 9).into(),
        (ConcreteConceptType::Left, 10).into(),
        (ConcreteConceptType::Right, 11).into(),
        false_concept,
        reduces_to_false,
        concept_c,
        not_concept_c,
        not_variable,
    ]
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "=>",
        1 => "true",
        3 => "_x_",
        7 => "not",
        4 => "->",
        12 => "false",
        14 => "c"
    }
}

#[test]
fn not() {
    let context_cache = MultiThreadedContextCache::default();
    let context_delta =
        NestedDelta::<_, SharedDirectConceptDelta<ConceptId>, _>::default();
    let context_snap_shot = MockSnapShot::new_test_case(&concepts(), &labels());
    let bound_variable_syntax = hashset! {};
    let context_search = MTContextSearch::from(ContextReferences {
        snap_shot: &context_snap_shot,
        delta: SharedContextDelta(context_delta.into()),
        cache: &context_cache,
        bound_variable_syntax: &bound_variable_syntax,
    });
    let (reduction, _) = context_search
        .find_examples_of_inferred_reduction(&context_search.to_ast(&14))
        .expect("Examples should be found");
    assert_eq!(reduction.get_concept().unwrap(), 12.into());
}
