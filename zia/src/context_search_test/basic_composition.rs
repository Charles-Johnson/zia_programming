use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::ContextSearch,
    snap_shot::{mock::MockSnapShot, Reader as SnapShotReader},
};
use maplit::hashmap;
use std::collections::HashMap;

#[test]
fn basic_composition() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<MockSnapShot>::from((&snapshot, &delta, &cache));
    let left_syntax = SyntaxTree::from("b").bind_nonquantifier_concept(1);
    let right_syntax = SyntaxTree::from("c").bind_nonquantifier_concept(2);
    let composite_syntax = SyntaxTree::from("a")
        .bind_nonquantifier_concept(0)
        .bind_pair(left_syntax.clone(), right_syntax.clone());
    let composite_syntax = || composite_syntax.clone();
    let left_syntax = || left_syntax.clone();
    let right_syntax = || right_syntax.clone();

    assert_eq!(
        context_search
            .contract_pair(&left_syntax().into(), &right_syntax().into()),
        composite_syntax().into()
    );

    assert_eq!(
        context_search.expand(&SyntaxTree::from("a").into()),
        composite_syntax().into()
    );

    assert_eq!(context_search.to_ast(0), composite_syntax().into());
    assert_eq!(context_search.to_ast(1), left_syntax().into());
    assert_eq!(context_search.to_ast(2), right_syntax().into());
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "a",
        1 => "b",
        2 => "c",
    }
}

fn concepts() -> [Concept; 3] {
    let mut left_concept = (SpecificPart::default(), 1).into();
    let mut right_concept = (SpecificPart::default(), 2).into();
    let composite_concept =
        Concept::composition_of(0, &mut left_concept, &mut right_concept);
    [composite_concept, left_concept, right_concept]
}
