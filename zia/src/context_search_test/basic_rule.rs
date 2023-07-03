use super::Syntax;
use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta::NestedContextDelta,
    context_search::{ContextReferences, ContextSearch},
    context_search_test::ReductionReason,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        MultiThreadedContextCache, SharedContextDelta, SharedDirectConceptDelta,
    },
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

#[test]
fn basic_rule() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta =
    NestedContextDelta::<ConceptId, SharedDirectConceptDelta<ConceptId>, SharedContextDelta<ConceptId>>::default(
        );
    let cache = MultiThreadedContextCache::default();
    let bound_variables = hashset! {};
    let context_search = ContextSearch::from(ContextReferences {
        snap_shot: &snapshot,
        delta: SharedContextDelta(delta.into()),
        cache: &cache,
        bound_variable_syntax: &bound_variables,
    });
    let concrete_syntax =
        Syntax::from("concrete").bind_nonquantifier_concept(0).share();
    let left_syntax =
        Syntax::from("left").bind_nonquantifier_concept(2).share();
    let left_and_random_syntax =
        Syntax::new_pair(left_syntax.clone(), Syntax::from("random").into())
            .into();

    assert_eq!(context_search.to_ast(&0), concrete_syntax);
    assert_eq!(context_search.to_ast(&2), left_syntax);

    let reduction_reason = ReductionReason::Rule {
        generalisation: context_search.to_ast(&1),
        variable_mask: hashmap! {3.into() => Syntax::from("random").into()},
        reason: ReductionReason::Explicit.into(),
    };

    assert_eq!(
        context_search.reduce(&left_and_random_syntax),
        Some((concrete_syntax.clone(), reduction_reason.clone()))
    );

    assert_eq!(
        context_search.recursively_reduce(&left_and_random_syntax),
        (concrete_syntax, Some(reduction_reason))
    );
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "concrete",
        2 => "left",
    }
}

fn concepts() -> [Concept<usize>; 4] {
    let mut concrete_concept = (ConcreteConceptType::True, 0).into();
    let mut left_concept = (SpecificPart::default(), 2).into();
    let mut right_concept_variable = (SpecificPart::free_variable(), 3).into();
    let mut composite_concept = Concept::composition_of(
        1,
        &mut left_concept,
        &mut right_concept_variable,
    );
    composite_concept.make_reduce_to(&mut concrete_concept);
    [concrete_concept, composite_concept, left_concept, right_concept_variable]
}
