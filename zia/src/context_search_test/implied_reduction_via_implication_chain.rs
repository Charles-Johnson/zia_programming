use crate::{
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta::NestedDelta,
    context_search::ContextReferences,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        MTContextCache, MTContextSearch, SharedContextDelta,
        SharedDirectConceptDelta,
    },
};
use maplit::{hashmap, hashset};
use pretty_assertions::assert_eq;
use std::collections::HashMap;

fn concepts() -> [Concept<usize>; 26] {
    let mut implication_concept = (ConcreteConceptType::Implication, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut concept_a = (SpecificPart::default(), 3).into();
    let mut example_concept = (SpecificPart::default(), 7).into();
    let mut variable_concept = (SpecificPart::free_variable(), 9).into();
    let mut concept_b = (SpecificPart::default(), 10).into();
    let mut cause_concept =
        Concept::composition_of(4, &mut concept_a, &mut variable_concept);
    let mut b_x =
        Concept::composition_of(15, &mut concept_b, &mut variable_concept);
    let mut not = (SpecificPart::default(), 16).into();
    let mut result_concept = Concept::composition_of(2, &mut not, &mut b_x);
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

    let mut another_variable = (SpecificPart::free_variable(), 17).into();
    let mut not_another_variable =
        Concept::composition_of(18, &mut not, &mut another_variable);
    let mut false_concept =
        (SpecificPart::Concrete(ConcreteConceptType::False), 19).into();
    let mut reduction_concept =
        (SpecificPart::Concrete(ConcreteConceptType::Reduction), 20).into();
    let mut reduces_to_false =
        Concept::composition_of(21, &mut reduction_concept, &mut false_concept);
    let mut another_variable_reduces_to_false = Concept::composition_of(
        22,
        &mut another_variable,
        &mut reduces_to_false,
    );
    let mut implies_another_variable_reduces_to_false = Concept::composition_of(
        23,
        &mut implication_concept,
        &mut another_variable_reduces_to_false,
    );
    let mut not_another_variable_implies_another_variable_reduces_to_false =
        Concept::composition_of(
            24,
            &mut not_another_variable,
            &mut implies_another_variable_reduces_to_false,
        );
    not_another_variable_implies_another_variable_reduces_to_false
        .make_reduce_to(&mut true_concept);
    let b_c = Concept::composition_of(25, &mut concept_b, &mut example_concept);
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
        (ConcreteConceptType::Precedes, 11).into(),
        (ConcreteConceptType::Associativity, 12).into(),
        (ConcreteConceptType::Left, 13).into(),
        (ConcreteConceptType::Right, 14).into(),
        b_x,
        not,
        another_variable,
        not_another_variable,
        false_concept,
        reduction_concept,
        reduces_to_false,
        another_variable_reduces_to_false,
        implies_another_variable_reduces_to_false,
        not_another_variable_implies_another_variable_reduces_to_false,
        b_c,
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
        14 => "right",
        16 => "not",
        17 => "_y_",
        19 => "false",
        20 => "->",
    }
}

#[test]
fn inference_rule() {
    let context_cache = MTContextCache::default();
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
    let false_syntax = context_search.to_ast(&19);
    assert_eq!(
        context_search
            .find_examples_of_inferred_reduction(&context_search.to_ast(&25))
            .unwrap()
            .0,
        false_syntax,
    );
}
