use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    snap_shot::Reader as SnapShotReader,
};
use lazy_static::lazy_static;

#[derive(Default)]
struct BasicPrecedenceSnapShot;

lazy_static! {
    static ref CONCEPTS: [Concept; 11] = {
        let mut precedence_concept = (SpecificPart::Concrete, 0).into();
        let mut greater_than_concept = (SpecificPart::Concrete, 1).into();
        let mut default_concept = (SpecificPart::Concrete, 2).into();
        let mut true_concept = (SpecificPart::Concrete, 3).into();
        let mut abstract_concept = (SpecificPart::default(), 4).into();
        let mut precedence_of_abstract_concept: Concept =
            (SpecificPart::default(), 5).into();
        precedence_of_abstract_concept.make_composition_of(
            &mut precedence_concept,
            &mut abstract_concept,
        );
        let mut greater_than_precedence_of_abstract_concept: Concept =
            (SpecificPart::default(), 6).into();
        greater_than_precedence_of_abstract_concept.make_composition_of(
            &mut greater_than_concept,
            &mut precedence_of_abstract_concept,
        );
        let mut precedence_of_abstract_concept_is_below_default: Concept =
            (SpecificPart::default(), 7).into();
        precedence_of_abstract_concept_is_below_default.make_composition_of(
            &mut default_concept,
            &mut greater_than_precedence_of_abstract_concept,
        );
        precedence_of_abstract_concept_is_below_default
            .make_reduce_to(&mut true_concept);
        let assoc_concept: Concept = (SpecificPart::Concrete, 8).into();
        let left_concept: Concept = (SpecificPart::Concrete, 9).into();
        let right_concept: Concept = (SpecificPart::Concrete, 10).into();
        [
            precedence_concept,
            greater_than_concept,
            default_concept,
            true_concept,
            abstract_concept,
            precedence_of_abstract_concept,
            greater_than_precedence_of_abstract_concept,
            precedence_of_abstract_concept_is_below_default,
            assoc_concept,
            left_concept,
            right_concept,
        ]
    };
    static ref ABSTRACT_SYNTAX: SyntaxTree =
        SyntaxTree::from("a").bind_concept(4);
}

impl SnapShotReader for BasicPrecedenceSnapShot {
    fn implication_id() -> usize {
        13
    }

    fn false_id() -> usize {
        14
    }

    fn reduction_id() -> usize {
        11
    }

    fn assoc_id() -> usize {
        8
    }

    fn right_id() -> usize {
        10
    }

    fn left_id() -> usize {
        9
    }

    fn exists_such_that_id() -> usize {
        12
    }

    fn precedence_id() -> usize {
        0
    }

    fn greater_than_id() -> usize {
        1
    }

    fn default_id() -> usize {
        2
    }

    fn true_id() -> usize {
        3
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "a" => Some(4),
            _ => None,
        }
    }

    fn read_concept(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Concept {
        CONCEPTS[concept_id].clone()
    }

    fn has_variable(&self, _delta: &ContextDelta, _variable_id: usize) -> bool {
        false
    }

    fn concept_len(&self, _delta: &ContextDelta) -> usize {
        9
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            4 => Some("a".into()),
            _ => None,
        }
    }
}

#[test]
fn basic_precedence() {
    let snapshot = BasicPrecedenceSnapShot::default();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicPrecedenceSnapShot>::from((
        &snapshot, &delta, &cache,
    ));

    assert_eq!(
        context_search.ast_from_expression("c b a"),
        Ok(SyntaxTree::from("(c b) a")
            .bind_pair(
                SyntaxTree::new_pair(
                    SyntaxTree::from("c"),
                    SyntaxTree::from("b")
                ),
                ABSTRACT_SYNTAX.clone()
            )
            .into())
    );
}
