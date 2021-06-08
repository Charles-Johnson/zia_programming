use crate::{
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{
        Comparison, ComparisonReason, ContextSearch, ReductionReason,
    },
    snap_shot::{mock::MockSnapShot, Reader as SnapShotReader},
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

#[test]
fn basic_comparison() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let bound_variables = hashset! {};
    let context_search = ContextSearch::<MockSnapShot>::from((
        &snapshot,
        &delta,
        &cache,
        &bound_variables,
    ));
    let left_syntax = context_search.to_ast(1);
    let right_syntax = context_search.to_ast(2);
    let another_syntax = context_search.to_ast(8);

    assert_eq!(
        context_search.compare(&left_syntax, &right_syntax),
        (
            Comparison::GreaterThan,
            ComparisonReason::Reduction {
                reason: Some(ReductionReason::Explicit),
                reversed_reason: None
            }
        )
    );

    assert_eq!(
        context_search.compare(&right_syntax, &left_syntax),
        (
            Comparison::LessThan,
            ComparisonReason::Reduction {
                reason: None,
                reversed_reason: Some(ReductionReason::Explicit)
            }
        )
    );

    assert_eq!(
        context_search.compare(&left_syntax, &left_syntax),
        (Comparison::EqualTo, ComparisonReason::SameSyntax)
    );

    assert_eq!(
        context_search.compare(&another_syntax, &right_syntax),
        (
            Comparison::LessThanOrEqualTo,
            ComparisonReason::Reduction {
                reason: Some(ReductionReason::Explicit),
                reversed_reason: None
            }
        )
    );

    assert_eq!(
        context_search.compare(&right_syntax, &another_syntax),
        (
            Comparison::GreaterThanOrEqualTo,
            ComparisonReason::Reduction {
                reason: None,
                reversed_reason: Some(ReductionReason::Explicit)
            }
        )
    );
    assert_eq!(
        context_search.compare(&left_syntax, &another_syntax),
        (
            Comparison::Incomparable,
            ComparisonReason::Reduction {
                reason: None,
                reversed_reason: None
            }
        )
    )
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => ">",
        1 => "2",
        2 => "1",
        5 => "true",
        6 => "assoc",
        7 => "right",
        8 => "3",
        10 => "false"
    }
}

fn concepts() -> [Concept<usize>; 11] {
    let mut greater_than_concept = (ConcreteConceptType::GreaterThan, 0).into();
    let mut left_concept = (SpecificPart::default(), 1).into();
    let mut right_concept = (SpecificPart::default(), 2).into();
    let mut is_greater_than_right_concept = Concept::composition_of(
        3,
        &mut greater_than_concept,
        &mut right_concept,
    );
    let mut left_is_greater_than_right_concept = Concept::composition_of(
        4,
        &mut left_concept,
        &mut is_greater_than_right_concept,
    );
    let mut true_concept = (ConcreteConceptType::True, 5).into();
    left_is_greater_than_right_concept.make_reduce_to(&mut true_concept);
    let assoc_concept = (ConcreteConceptType::Associativity, 6).into();
    let right_id_concept = (ConcreteConceptType::Right, 7).into();
    let mut another_concept = (SpecificPart::default(), 8).into();
    let mut another_concept_is_greater_than_right_concept =
        Concept::composition_of(
            9,
            &mut another_concept,
            &mut is_greater_than_right_concept,
        );
    let mut false_concept = (ConcreteConceptType::False, 10).into();
    another_concept_is_greater_than_right_concept
        .make_reduce_to(&mut false_concept);
    [
        greater_than_concept,
        left_concept,
        right_concept,
        is_greater_than_right_concept,
        left_is_greater_than_right_concept,
        true_concept,
        assoc_concept,
        right_id_concept,
        another_concept,
        another_concept_is_greater_than_right_concept,
        false_concept,
    ]
}
