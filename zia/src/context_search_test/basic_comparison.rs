use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{
        Comparison, ComparisonReason, ContextCache, ContextSearch,
        ReductionReason,
    },
    context_search_test::check_order,
    snap_shot::Reader as SnapShotReader,
};

struct BasicComparisonSnapShot {
    concepts: Vec<Concept>,
}

const CONCEPT_LEN: usize = 11;

impl Default for BasicComparisonSnapShot {
    fn default() -> Self {
        let mut greater_than_concept = (SpecificPart::Concrete, 0).into();
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
        let mut true_concept = (SpecificPart::Concrete, 5).into();
        left_is_greater_than_right_concept.make_reduce_to(&mut true_concept);
        let assoc_concept = (SpecificPart::Concrete, 6).into();
        let right_id_concept = (SpecificPart::Concrete, 7).into();
        let mut another_concept = (SpecificPart::default(), 8).into();
        let mut another_concept_is_greater_than_right_concept =
            Concept::composition_of(
                9,
                &mut another_concept,
                &mut is_greater_than_right_concept,
            );
        let mut false_concept = (SpecificPart::Concrete, 10).into();
        another_concept_is_greater_than_right_concept
            .make_reduce_to(&mut false_concept);
        let concepts: [_; CONCEPT_LEN] = [
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
        ];
        BasicComparisonSnapShot {
            concepts: check_order(&concepts),
        }
    }
}

impl SnapShotReader for BasicComparisonSnapShot {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        self.concepts.get(concept_id)
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            ">" => Some(0),
            "2" => Some(1),
            "1" => Some(2),
            "true" => Some(5),
            "assoc" => Some(6),
            "right" => Some(7),
            "3" => Some(8),
            "false" => Some(10),
            _ => None,
        }
    }

    fn lowest_unoccupied_concept_id(&self, _delta: &ContextDelta) -> usize {
        self.concepts.len()
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            0 => Some(">".into()),
            1 => Some("2".into()),
            2 => Some("1".into()),
            5 => Some("true".into()),
            6 => Some("assoc".into()),
            7 => Some("right".into()),
            8 => Some("3".into()),
            10 => Some("false".into()),
            _ => None,
        }
    }

    fn greater_than_id() -> usize {
        0
    }

    fn assoc_id() -> usize {
        6
    }

    fn left_id() -> usize {
        CONCEPT_LEN
    }

    fn precedence_id() -> usize {
        CONCEPT_LEN + 1
    }

    fn right_id() -> usize {
        7
    }

    fn true_id() -> usize {
        5
    }

    fn reduction_id() -> usize {
        CONCEPT_LEN + 2
    }

    fn exists_such_that_id() -> usize {
        CONCEPT_LEN + 3
    }

    fn implication_id() -> usize {
        CONCEPT_LEN + 4
    }

    fn false_id() -> usize {
        10
    }
}

#[test]
fn basic_comparison() {
    let snapshot = BasicComparisonSnapShot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicComparisonSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
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
