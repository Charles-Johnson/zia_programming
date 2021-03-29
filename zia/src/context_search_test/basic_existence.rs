use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{ContextSearch, ReductionReason},
    snap_shot::{mock::MockSnapShot, Reader as SnapShotReader},
};
use maplit::hashmap;
use std::{collections::HashMap, sync::Arc};

#[test]
fn basic_existence() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<MockSnapShot>::from((&snapshot, &delta, &cache));
    let exists_such_that_syntax = context_search.to_ast(0);
    let variable_syntax = context_search.to_ast(3);
    let variable_exists_such_that_variable_is_true_syntax = context_search
        .combine(
            &context_search.combine(&variable_syntax, &exists_such_that_syntax),
            &variable_syntax,
        )
        .into();
    let true_syntax = context_search.to_ast(1);
    assert_eq!(
        context_search
            .reduce(&variable_exists_such_that_variable_is_true_syntax),
        Some((
            true_syntax.clone(),
            ReductionReason::Existence {
                generalisation: variable_syntax.clone(),
                substitutions: hashmap! {variable_syntax => context_search.to_ast(2)},
                reduction: true_syntax,
                reduction_reason: ReductionReason::Explicit.into(),
            }
        ))
    );
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "exists_such_that",
        1 => "true",
        2 => "a",
        3 => "_x_"
    }
}

fn concepts() -> [Concept; 4] {
    let exists_such_that_concept =
        (ConcreteConceptType::ExistsSuchThat, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut abstract_concept: Concept = (SpecificPart::default(), 2).into();
    let variable_concept: Concept = (SpecificPart::variable(), 3).into();
    abstract_concept.make_reduce_to(&mut true_concept);
    [exists_such_that_concept, true_concept, abstract_concept, variable_concept]
}
