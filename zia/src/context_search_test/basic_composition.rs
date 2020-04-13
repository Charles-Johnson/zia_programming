use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    snap_shot::Reader as SnapShotReader,
};
use lazy_static::lazy_static;

#[derive(Default)]
struct BasicCompositionSnapShot;

lazy_static! {
    static ref CONCEPTS: (Concept, Concept, Concept) = {
        let mut composite_concept: Concept =
            (SpecificPart::default(), 0).into();
        let mut left_concept: Concept = (SpecificPart::default(), 1).into();
        let mut right_concept: Concept = (SpecificPart::default(), 2).into();
        composite_concept
            .make_composition_of(&mut left_concept, &mut right_concept);
        (composite_concept, left_concept, right_concept)
    };
    static ref SYNTAX: [SyntaxTree; 3] = {
        let left_syntax = SyntaxTree::from("b").bind_concept(1);
        let right_syntax = SyntaxTree::from("c").bind_concept(2);
        let composite_syntax = SyntaxTree::from("a")
            .bind_concept(0)
            .bind_pair(left_syntax.clone(), right_syntax.clone());
        [composite_syntax, left_syntax, right_syntax]
    };
}

impl SnapShotReader for BasicCompositionSnapShot {
    fn read_concept(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Concept {
        let (composite_concept, left_concept, right_concept) = CONCEPTS.clone();
        match concept_id {
            0 => composite_concept,
            1 => left_concept,
            2 => right_concept,
            _ => panic!("No concepts with id: {}", concept_id),
        }
    }

    fn true_id() -> usize {
        unimplemented!()
    }

    fn implication_id() -> usize {
        unimplemented!()
    }

    fn precedence_id() -> usize {
        unimplemented!()
    }

    fn greater_than_id() -> usize {
        unimplemented!()
    }

    fn default_id() -> usize {
        unimplemented!()
    }

    fn false_id() -> usize {
        unimplemented!()
    }

    fn reduction_id() -> usize {
        unimplemented!()
    }

    fn assoc_id() -> usize {
        unimplemented!()
    }

    fn right_id() -> usize {
        unimplemented!()
    }

    fn left_id() -> usize {
        unimplemented!()
    }

    fn exists_such_that_id() -> usize {
        unimplemented!()
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "a" => Some(0),
            "b" => Some(1),
            "c" => Some(2),
            _ => None,
        }
    }

    fn has_variable(&self, _delta: &ContextDelta, _variable_id: usize) -> bool {
        false
    }

    fn concept_len(&self, _delta: &ContextDelta) -> usize {
        3
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            0 => Some("a".into()),
            1 => Some("b".into()),
            2 => Some("c".into()),
            _ => None,
        }
    }
}

#[test]
fn basic_composition() {
    let snapshot = BasicCompositionSnapShot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicCompositionSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
    let [composite_syntax, left_syntax, right_syntax] = SYNTAX.clone();
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