use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::ContextSearch,
    mock_snap_shot::{ConceptId, MockSnapShot},
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

#[test]
fn basic_composition() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let bound_variables = hashset! {};
    let context_search = ContextSearch::<MockSnapShot>::from((
        &snapshot,
        &delta,
        &cache,
        &bound_variables,
    ));
    let left_syntax =
        SyntaxTree::from("b").bind_nonquantifier_concept(1).share();
    let right_syntax =
        SyntaxTree::from("c").bind_nonquantifier_concept(2).share();
    let composite_syntax = SyntaxTree::from("a")
        .bind_nonquantifier_concept(0)
        .bind_pair(left_syntax.clone(), right_syntax.clone())
        .share();

    assert_eq!(
        context_search.contract_pair(&left_syntax, &right_syntax),
        composite_syntax
    );

    assert_eq!(
        context_search.expand(&SyntaxTree::from("a").into()),
        composite_syntax
    );

    assert_eq!(context_search.to_ast(0), composite_syntax);
    assert_eq!(context_search.to_ast(1), left_syntax);
    assert_eq!(context_search.to_ast(2), right_syntax);
}

fn labels() -> HashMap<ConceptId, &'static str> {
    hashmap! {
        0 => "a",
        1 => "b",
        2 => "c",
    }
}

fn concepts() -> [Concept<ConceptId>; 3] {
    let mut left_concept = (SpecificPart::default(), 1).into();
    let mut right_concept = (SpecificPart::default(), 2).into();
    let composite_concept =
        Concept::composition_of(0, &mut left_concept, &mut right_concept);
    [composite_concept, left_concept, right_concept]
}
