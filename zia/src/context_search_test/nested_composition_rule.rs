use super::Syntax;
use crate::{
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_search_test::{new_context_search_test, ReductionReason},
    mock_snap_shot::MockSnapShot,
};
use assert_matches::assert_matches;
use maplit::hashmap;
use std::collections::{HashMap, HashSet};

fn concepts() -> [Concept<usize>; 8] {
    let mut concrete_concept = (ConcreteConceptType::True, 0).into();
    let mut left_concept = (SpecificPart::default(), 2).into();
    let mut right_left_concept = (SpecificPart::default(), 3).into();
    let mut right_right_concept_variable =
        (SpecificPart::free_variable(), 4).into();
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
    let bound_variables = HashSet::new();
    let context_search = new_context_search_test(&snapshot, &bound_variables);
    let concrete_syntax =
        || Syntax::from("concrete").bind_nonquantifier_concept(0);
    let left_syntax =
        Syntax::from("left").bind_nonquantifier_concept(2).share();
    let right_left_syntax =
        Syntax::from("right_left").bind_nonquantifier_concept(3);
    let left_and_right_left_and_random_syntax = Syntax::new_pair(
        left_syntax.clone(),
        Syntax::new_pair(
            right_left_syntax.share(),
            Syntax::from("random").into(),
        )
        .into(),
    )
    .into();

    assert_eq!(context_search.to_ast(&0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(&2), left_syntax);

    let reduction_reason = ReductionReason::Rule {
        generalisation: context_search.to_ast(&1),
        variable_mask: hashmap! {4.into() => Syntax::from("random").into()},
        reason: ReductionReason::Explicit.into(),
    };

    assert_matches!(
        context_search.reduce(&left_and_right_left_and_random_syntax),
        Some((syntax, reason)) => {
            assert_eq!(syntax.key(), concrete_syntax().key());
            assert_eq!(reason, reduction_reason);
        }
    );

    assert_eq!(
        context_search
            .recursively_reduce(&left_and_right_left_and_random_syntax),
        (concrete_syntax().into(), Some(reduction_reason))
    );
}
