use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{
        Comparison, ComparisonReason, ContextSearch, ReductionReason,
    },
    snap_shot::{mock::MockSnapShot, Reader as SnapShotReader},
};
use maplit::hashmap;
use std::collections::HashMap;

#[test]
fn basic_comparison() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<MockSnapShot>::from((&snapshot, &delta, &cache));
    let left_syntax = || SyntaxTree::new_concept(1);
    let right_syntax = || SyntaxTree::new_concept(2);
    let another_syntax = || SyntaxTree::new_concept(8);

    assert_eq!(
        context_search.compare(&left_syntax().into(), &right_syntax().into()),
        (
            Comparison::GreaterThan,
            ComparisonReason::Reduction {
                reason: Some(ReductionReason::Explicit),
                reversed_reason: None
            }
        )
    );

    assert_eq!(
        context_search.compare(&right_syntax().into(), &left_syntax().into()),
        (
            Comparison::LessThan,
            ComparisonReason::Reduction {
                reason: None,
                reversed_reason: Some(ReductionReason::Explicit)
            }
        )
    );

    assert_eq!(
        context_search.compare(&left_syntax().into(), &left_syntax().into()),
        (Comparison::EqualTo, ComparisonReason::SameSyntax)
    );

    assert_eq!(
        context_search
            .compare(&another_syntax().into(), &right_syntax().into()),
        (
            Comparison::LessThanOrEqualTo,
            ComparisonReason::Reduction {
                reason: Some(ReductionReason::Explicit),
                reversed_reason: None
            }
        )
    );

    assert_eq!(
        context_search
            .compare(&right_syntax().into(), &another_syntax().into()),
        (
            Comparison::GreaterThanOrEqualTo,
            ComparisonReason::Reduction {
                reason: None,
                reversed_reason: Some(ReductionReason::Explicit)
            }
        )
    );
    // TODO: find out why this test is non-deterministic
    assert_eq!(
        context_search.compare(&left_syntax().into(), &another_syntax().into()),
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

fn concepts() -> [Concept; 11] {
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
