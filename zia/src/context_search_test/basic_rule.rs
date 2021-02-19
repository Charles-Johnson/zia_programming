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

#[test]
fn basic_rule() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<MockSnapShot>::from((&snapshot, &delta, &cache));
    let concrete_syntax = || SyntaxTree::from("concrete").bind_nonquantifier_concept(0);
    let left_syntax = || SyntaxTree::from("left").bind_nonquantifier_concept(2);
    let left_and_random_syntax =
        SyntaxTree::new_pair(left_syntax(), SyntaxTree::from("random")).into();

    assert_eq!(context_search.to_ast(0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(2), left_syntax().into());

    let reduction_reason = ReductionReason::Rule {
        generalisation: context_search.to_ast(1),
        variable_mask: hashmap! {3 => SyntaxTree::from("random").into()},
        reason: ReductionReason::Explicit.into(),
    };

    assert_eq!(
        context_search.reduce(&left_and_random_syntax),
        Some((concrete_syntax().into(), reduction_reason.clone()))
    );

    assert_eq!(
        context_search.recursively_reduce(&left_and_random_syntax),
        (concrete_syntax().into(), Some(reduction_reason))
    );
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "concrete",
        2 => "left",
    }
}

fn concepts() -> [Concept; 4] {
    let mut concrete_concept = (ConcreteConceptType::True, 0).into();
    let mut left_concept = (SpecificPart::default(), 2).into();
    let mut right_concept_variable = (SpecificPart::variable(), 3).into();
    let mut composite_concept = Concept::composition_of(
        1,
        &mut left_concept,
        &mut right_concept_variable,
    );
    composite_concept.make_reduce_to(&mut concrete_concept);
    [concrete_concept, composite_concept, left_concept, right_concept_variable]
}
