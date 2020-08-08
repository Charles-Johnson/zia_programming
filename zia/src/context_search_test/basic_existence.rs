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
fn basic_existence() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<MockSnapShot>::from((&snapshot, &delta, &cache));
    let exists_such_that_syntax =
        SyntaxTree::from("exists_such_that").bind_concept(0);
    let variable_syntax = || SyntaxTree::from("_x_").bind_concept(3);
    let variable_exists_such_that_variable_is_true_syntax =
        SyntaxTree::new_pair(
            variable_syntax(),
            SyntaxTree::new_pair(exists_such_that_syntax, variable_syntax()),
        )
        .into();

    assert_eq!(
        context_search
            .reduce(&variable_exists_such_that_variable_is_true_syntax),
        Some((
            SyntaxTree::from("true").bind_concept(1).into(),
            ReductionReason::Existence {
                example: context_search.to_ast(2),
                reason: ReductionReason::Explicit.into(),
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
    let variable_concept = (SpecificPart::variable(), 3).into();
    abstract_concept.make_reduce_to(&mut true_concept);
    [exists_such_that_concept, true_concept, abstract_concept, variable_concept]
}
