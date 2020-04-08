use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    snap_shot::Reader as SnapShotReader,
};
use lazy_static::lazy_static;
use std::sync::Arc;

#[derive(Default)]
struct BasicInferenceSnapShot;

lazy_static! {
    static ref CONCEPTS: [Concept; 6] = {
        let mut implication_concept = (SpecificPart::Concrete, 0).into();
        let mut true_concept = (SpecificPart::Concrete, 1).into();
        let mut condition_concept: Concept =
            (SpecificPart::default(), 2).into();
        let mut result_concept = (SpecificPart::default(), 3).into();
        condition_concept.make_reduce_to(&mut true_concept);
        let mut implies_result_concept: Concept =
            (SpecificPart::default(), 4).into();
        implies_result_concept
            .make_composition_of(&mut implication_concept, &mut result_concept);
        let mut condition_implies_result_concept: Concept =
            (SpecificPart::default(), 5).into();
        condition_implies_result_concept.make_composition_of(
            &mut condition_concept,
            &mut implies_result_concept,
        );
        condition_implies_result_concept.make_reduce_to(&mut true_concept);
        [
            implication_concept,
            true_concept,
            condition_concept,
            result_concept,
            implies_result_concept,
            condition_implies_result_concept,
        ]
    };
    static ref SYNTAX: [SyntaxTree; 6] = {
        let implication_syntax = SyntaxTree::from("=>").bind_concept(0);
        let true_syntax = SyntaxTree::from("true").bind_concept(1);
        let condition_syntax = SyntaxTree::from("a").bind_concept(2);
        let result_syntax = SyntaxTree::from("b").bind_concept(3);
        let implies_result_syntax = SyntaxTree::new_concept(4).bind_pair(
            &Arc::new(implication_syntax.clone()),
            &Arc::new(result_syntax.clone()),
        );
        let condition_implies_result_syntax = SyntaxTree::new_concept(5)
            .bind_pair(
                &Arc::new(condition_syntax.clone()),
                &Arc::new(implies_result_syntax.clone()),
            );
        [
            implication_syntax,
            true_syntax,
            condition_syntax,
            result_syntax,
            implies_result_syntax,
            condition_implies_result_syntax,
        ]
    };
}

impl SnapShotReader for BasicInferenceSnapShot {
    fn implication_id() -> usize {
        0
    }

    fn true_id() -> usize {
        1
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

    fn precedence_id() -> usize {
        unimplemented!()
    }

    fn greater_than_id() -> usize {
        unimplemented!()
    }

    fn default_id() -> usize {
        unimplemented!()
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        _label: &str,
    ) -> Option<usize> {
        unimplemented!()
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
        6
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            0 => Some("implication".into()),
            1 => Some("true".into()),
            2 => Some("a".into()),
            3 => Some("b".into()),
            _ => None,
        }
    }

    fn ast_from_symbol(
        &self,
        _delta: &ContextDelta,
        symbol: &str,
    ) -> SyntaxTree {
        let [implication_syntax, true_syntax, condition_syntax, result_syntax, ..] =
            SYNTAX.clone();
        match symbol {
            "implication" => implication_syntax,
            "true" => true_syntax,
            "a" => condition_syntax,
            "b" => result_syntax,
            _ => symbol.into(),
        }
    }
}

#[test]
fn basic_inference() {
    let snapshot = BasicInferenceSnapShot::default();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicInferenceSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
    let [_, true_syntax, _, result_syntax, ..] = SYNTAX.clone();
    let true_syntax = Arc::new(true_syntax);
    let result_syntax = Arc::new(result_syntax);

    assert_eq!(
        context_search.reduce(&result_syntax),
        Some(true_syntax.clone())
    );

    assert_eq!(context_search.recursively_reduce(&result_syntax), true_syntax);
}
