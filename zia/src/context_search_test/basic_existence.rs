use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    snap_shot::Reader as SnapShotReader,
};
use lazy_static::lazy_static;

#[derive(Default)]
struct BasicExistenceSnapShot;

const CONCEPTS_LEN: usize = 4;

lazy_static! {
    static ref CONCEPTS: [Concept; CONCEPTS_LEN] = {
        let exists_such_that_concept = (SpecificPart::Concrete, 0).into();
        let mut true_concept = (SpecificPart::Concrete, 1).into();
        let mut abstract_concept: Concept = (SpecificPart::default(), 2).into();
        let variable_concept = (SpecificPart::default(), 3).into();
        abstract_concept.make_reduce_to(&mut true_concept);
        [
            exists_such_that_concept,
            true_concept,
            abstract_concept,
            variable_concept,
        ]
    };
    static ref EXISTS_SUCH_THAT_SYNTAX: SyntaxTree =
        SyntaxTree::from("exists_such_that").bind_concept(0);
    static ref TRUTH_SYNTAX: SyntaxTree =
        SyntaxTree::from("true").bind_concept(1);
    static ref VARIABLE_SYNTAX: SyntaxTree =
        SyntaxTree::from("_x_").bind_concept(3);
}

impl SnapShotReader for BasicExistenceSnapShot {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        CONCEPTS.get(concept_id)
    }

    fn has_variable(&self, _delta: &ContextDelta, _variable_id: usize) -> bool {
        false
    }

    fn lowest_unoccupied_concept_id(&self, _delta: &ContextDelta) -> usize {
        CONCEPTS_LEN
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            0 => Some("exists_such_that".into()),
            1 => Some("true".into()),
            3 => Some("_x_".into()),
            _ => None,
        }
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "exists_such_that" => Some(0),
            "true" => Some(1),
            "_x_" => Some(3),
            _ => None,
        }
    }

    fn false_id() -> usize {
        CONCEPTS_LEN
    }

    fn reduction_id() -> usize {
        CONCEPTS_LEN
    }

    fn assoc_id() -> usize {
        CONCEPTS_LEN
    }

    fn exists_such_that_id() -> usize {
        0
    }

    fn true_id() -> usize {
        1
    }

    fn precedence_id() -> usize {
        CONCEPTS_LEN
    }
}

#[test]
fn basic_existence() {
    let snapshot = BasicExistenceSnapShot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicExistenceSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
    let exists_such_that_syntax = EXISTS_SUCH_THAT_SYNTAX.clone();
    let variable_syntax = || VARIABLE_SYNTAX.clone();
    let variable_exists_such_that_variable_is_true_syntax =
        SyntaxTree::new_pair(
            variable_syntax(),
            SyntaxTree::new_pair(exists_such_that_syntax, variable_syntax()),
        );

    assert_eq!(
        context_search
            .reduce(&variable_exists_such_that_variable_is_true_syntax.into()),
        Some(TRUTH_SYNTAX.clone().into())
    );
}
