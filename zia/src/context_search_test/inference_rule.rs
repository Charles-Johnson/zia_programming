use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    context_search_test::check_order,
    snap_shot::Reader,
};

struct InferenceRuleSnapShot {
    concepts: Vec<Concept>,
}

impl Default for InferenceRuleSnapShot {
    fn default() -> Self {
        let mut implication_concept = (SpecificPart::Concrete, 0).into();
        let mut true_concept = (SpecificPart::Concrete, 1).into();
        let mut concept_a = (SpecificPart::default(), 3).into();
        let mut example_concept = (SpecificPart::default(), 7).into();
        let mut variable_concept = (SpecificPart::variable(), 9).into();
        let mut concept_b = (SpecificPart::default(), 10).into();
        let mut cause_concept =
            Concept::composition_of(4, &mut concept_a, &mut variable_concept);
        let mut result_concept =
            Concept::composition_of(2, &mut variable_concept, &mut concept_b);
        let mut implies_result_concept = Concept::composition_of(
            5,
            &mut implication_concept,
            &mut result_concept,
        );
        let mut cause_implies_result_concept = Concept::composition_of(
            6,
            &mut cause_concept,
            &mut implies_result_concept,
        );
        cause_implies_result_concept.make_reduce_to(&mut true_concept);
        let mut example_composition =
            Concept::composition_of(8, &mut concept_a, &mut example_concept);
        example_composition.make_reduce_to(&mut true_concept);
        Self {
            concepts: check_order(&[
                implication_concept,
                true_concept,
                result_concept,
                concept_a,
                cause_concept,
                implies_result_concept,
                cause_implies_result_concept,
                example_concept,
                example_composition,
                variable_concept,
                concept_b,
            ]),
        }
    }
}

impl Reader for InferenceRuleSnapShot {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        self.concepts.get(concept_id)
    }

    fn lowest_unoccupied_concept_id(&self, _delta: &ContextDelta) -> usize {
        self.concepts.len()
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        _concept_id: usize,
    ) -> Option<String> {
        None
    }

    fn concept_from_label(
        &self,
        _delta: &ContextDelta,
        _s: &str,
    ) -> Option<usize> {
        None
    }

    fn implication_id() -> usize {
        0
    }

    fn true_id() -> usize {
        1
    }

    fn precedence_id() -> usize {
        11
    }

    fn assoc_id() -> usize {
        11
    }
}

#[test]
fn inference_rule() {
    let context_cache = ContextCache::default();
    let context_delta = ContextDelta::default();
    let context_snap_shot = InferenceRuleSnapShot::new_test_case();
    let context_search = ContextSearch::from((
        &context_snap_shot,
        &context_delta,
        &context_cache,
    ));
    let example_syntax = SyntaxTree::new_pair(
        SyntaxTree::new_concept(7),
        SyntaxTree::new_concept(10),
    );
    let true_syntax = SyntaxTree::new_concept(1);
    assert_eq!(
        context_search.reduce(&example_syntax.into()),
        Some(true_syntax.into())
    );
}
