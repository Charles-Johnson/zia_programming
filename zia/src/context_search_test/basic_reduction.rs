use crate::{
    ast::{MultiThreadedSyntaxTree, SyntaxTree},
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{ContextReferences, ContextSearch, ReductionReason},
    mock_snap_shot::{ConceptId, MockSnapShot},
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

type Syntax = MultiThreadedSyntaxTree;

#[test]
fn basic_reduction() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let bound_variables = hashset! {};
    let context_search =
        ContextSearch::<MockSnapShot, Syntax>::from(ContextReferences {
            snap_shot: &snapshot,
            delta: &delta,
            cache: &cache,
            bound_variable_syntax: &bound_variables,
        });
    let abstract_syntax =
        || Syntax::from("abstract").bind_nonquantifier_concept(1);
    let concrete_syntax =
        || Syntax::from("concrete").bind_nonquantifier_concept(0);

    assert_eq!(
        context_search.recursively_reduce(&abstract_syntax().into()),
        (concrete_syntax().into(), Some(ReductionReason::Explicit))
    );
    assert_eq!(
        context_search.recursively_reduce(&concrete_syntax().into()),
        (concrete_syntax().into(), None)
    );

    assert_eq!(
        context_search.reduce(&abstract_syntax().into()),
        Some((concrete_syntax().into(), ReductionReason::Explicit))
    );
    assert_eq!(context_search.reduce(&concrete_syntax().into()), None);

    assert_eq!(
        context_search.expand(&abstract_syntax().into()),
        abstract_syntax().into()
    );
    assert_eq!(
        context_search.expand(&concrete_syntax().into()),
        concrete_syntax().into()
    );

    assert_eq!(context_search.to_ast(0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(1), abstract_syntax().into());
}

fn labels() -> HashMap<ConceptId, &'static str> {
    hashmap! {
        0 => "concrete",
        1 => "abstract",
    }
}

fn concepts() -> [Concept<ConceptId>; 2] {
    let mut concrete_concept = (ConcreteConceptType::True, 0).into();
    let mut abstract_concept: Concept<ConceptId> =
        (SpecificPart::default(), 1).into();
    abstract_concept.make_reduce_to(&mut concrete_concept);
    [concrete_concept, abstract_concept]
}
