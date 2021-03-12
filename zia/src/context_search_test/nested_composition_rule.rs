use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{ContextSearch, ReductionReason},
    snap_shot::{mock::MockSnapShot, Reader as SnapShotReader},
};
use maplit::hashmap;
use std::collections::HashMap;

const CONCEPT_LENGTH: usize = 8;

fn concepts() -> [Concept; CONCEPT_LENGTH] {
    let mut concrete_concept = (ConcreteConceptType::True, 0).into();
    let mut left_concept = (SpecificPart::default(), 2).into();
    let mut right_left_concept = (SpecificPart::default(), 3).into();
    let mut right_right_concept_variable = (SpecificPart::variable(), 4).into();
    let mut right_composite_concept = Concept::composition_of(
        5,
        &mut right_left_concept,
        &mut right_right_concept_variable,
    );
    let mut composite_concept = Concept::composition_of(
        1,
        &mut left_concept,
        &mut right_composite_concept,
    );
    let assoc_concept = (ConcreteConceptType::Associativity, 6).into();
    let right_id_concept = (ConcreteConceptType::Right, 7).into();
    composite_concept.make_reduce_to(&mut concrete_concept);
    [
        concrete_concept,
        composite_concept,
        left_concept,
        right_left_concept,
        right_right_concept_variable,
        right_composite_concept,
        assoc_concept,
        right_id_concept,
    ]
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "concrete",
        2 => "left",
        3 => "right_left",
        4 => "_y_"
    }
}

#[test]
fn basic_rule() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<MockSnapShot>::from((&snapshot, &delta, &cache));
    let concrete_syntax =
        || SyntaxTree::from("concrete").bind_nonquantifier_concept(0);
    let left_syntax = || SyntaxTree::from("left").bind_nonquantifier_concept(2);
    let right_left_syntax =
        SyntaxTree::from("right_left").bind_nonquantifier_concept(3);
    let left_and_right_left_and_random_syntax = SyntaxTree::new_pair(
        left_syntax(),
        SyntaxTree::new_pair(right_left_syntax, SyntaxTree::from("random")),
    )
    .into();

    assert_eq!(context_search.to_ast(0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(2), left_syntax().into());

    let reduction_reason = ReductionReason::Rule {
        generalisation: context_search.to_ast(1),
        variable_mask: hashmap! {4 => SyntaxTree::from("random").into()},
        reason: ReductionReason::Explicit.into(),
    };

    assert_eq!(
        context_search.reduce(&left_and_right_left_and_random_syntax),
        Some((concrete_syntax().into(), reduction_reason.clone()))
    );

    assert_eq!(
        context_search
            .recursively_reduce(&left_and_right_left_and_random_syntax),
        (concrete_syntax().into(), Some(reduction_reason))
    );
}
