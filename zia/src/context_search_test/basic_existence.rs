use super::Syntax;
use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta::NestedDelta,
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
fn basic_existence() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = NestedDelta::<
        ConceptId,
        SharedDirectConceptDelta<ConceptId>,
        SharedContextDelta<ConceptId>,
    >::default();
    let cache = MultiThreadedContextCache::default();
    let variable_syntax = Syntax::from("_x_").share();
    let bound_variables = hashset! {variable_syntax.clone()};
    let context_search = ContextSearch::from(ContextReferences {
        snap_shot: &snapshot,
        delta: SharedContextDelta(delta.into()),
        cache: &cache,
        bound_variable_syntax: &bound_variables,
    });
    let exists_such_that_syntax = context_search.to_ast(&0);
    let variable_exists_such_that_variable_is_true_syntax = context_search
        .combine(
            &context_search
                .combine(&variable_syntax, &exists_such_that_syntax)
                .share(),
            &variable_syntax,
        )
        .into();
    let true_syntax = context_search.to_ast(&1);
    assert_eq!(
        context_search
            .reduce(&variable_exists_such_that_variable_is_true_syntax),
        Some((
            true_syntax,
            ReductionReason::Existence {
                generalisation: variable_syntax.clone(),
                substitutions: hashmap! {variable_syntax => context_search.to_ast(&2)},
            }
        ))
    );
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "exists_such_that",
        1 => "true",
        2 => "a",
    }
}

fn concepts() -> [Concept<usize>; 3] {
    let exists_such_that_concept =
        (ConcreteConceptType::ExistsSuchThat, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut abstract_concept: Concept<usize> =
        (SpecificPart::default(), 2).into();
    abstract_concept.make_reduce_to(&mut true_concept);
    [exists_such_that_concept, true_concept, abstract_concept]
}
