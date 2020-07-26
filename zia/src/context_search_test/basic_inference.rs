use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{ContextSearch, ReductionReason},
    context_search_test::check_order,
    snap_shot::Reader as SnapShotReader,
};

struct BasicInferenceSnapShot {
    concepts: Vec<Concept>,
}

impl Default for BasicInferenceSnapShot {
    fn default() -> Self {
        let mut implication_concept = (SpecificPart::Concrete, 0).into();
        let mut true_concept = (SpecificPart::Concrete, 1).into();
        let mut condition_concept: Concept =
            (SpecificPart::default(), 2).into();
        let mut result_concept = (SpecificPart::default(), 3).into();
        condition_concept.make_reduce_to(&mut true_concept);
        let mut implies_result_concept = Concept::composition_of(
            4,
            &mut implication_concept,
            &mut result_concept,
        );
        let mut condition_implies_result_concept = Concept::composition_of(
            5,
            &mut condition_concept,
            &mut implies_result_concept,
        );
        condition_implies_result_concept.make_reduce_to(&mut true_concept);
        Self {
            concepts: check_order(&[
                implication_concept,
                true_concept,
                condition_concept,
                result_concept,
                implies_result_concept,
                condition_implies_result_concept,
                (SpecificPart::Concrete, 6).into(),
                (SpecificPart::Concrete, 7).into(),
                (SpecificPart::Concrete, 8).into(),
                (SpecificPart::Concrete, 9).into(),
            ]),
        }
    }
}

impl SnapShotReader for BasicInferenceSnapShot {
    fn implication_id() -> usize {
        0
    }

    fn true_id() -> usize {
        1
    }

    fn assoc_id() -> usize {
        6
    }

    fn left_id() -> usize {
        7
    }

    fn precedence_id() -> usize {
        8
    }

    fn right_id() -> usize {
        9
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "implication" => Some(0),
            "true" => Some(1),
            "a" => Some(2),
            "b" => Some(3),
            _ => None,
        }
    }

    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        self.concepts.get(concept_id)
    }

    fn lowest_unoccupied_concept_id(&self, _delta: &ContextDelta) -> usize {
        self.concepts.len()
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
}

#[test]
fn basic_inference() {
    let snapshot = BasicInferenceSnapShot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicInferenceSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
    let true_syntax = || SyntaxTree::from("true").bind_concept(1);
    let result_syntax = || SyntaxTree::from("b").bind_concept(3);
    let reduction_reason = ReductionReason::Inference {
        implication: context_search.to_ast(5),
        reason: ReductionReason::Explicit.into(),
    };
    assert_eq!(
        context_search.reduce(&result_syntax().into()),
        Some((true_syntax().into(), reduction_reason.clone()))
    );

    assert_eq!(
        context_search.recursively_reduce(&result_syntax().into()),
        (true_syntax().into(), Some(reduction_reason))
    );
}
