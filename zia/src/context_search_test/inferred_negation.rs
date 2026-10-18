use crate::{
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_search_test::new_context_search_test,
    mock_snap_shot::MockSnapShot,
};
use maplit::hashmap;
use std::collections::{HashMap, HashSet};

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
        (ConcreteConceptType::Precedes, 8).into(),
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
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let bound_variables = HashSet::new();
    let context_search = new_context_search_test(&snapshot, &bound_variables);
    let (reduction, _) = context_search
        .find_examples_of_inferred_reduction(&context_search.to_ast(&7))
        .unwrap();
    assert_eq!(reduction.get_concept().unwrap(), 12.into());
}
