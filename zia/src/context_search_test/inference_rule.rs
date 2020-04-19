use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    snap_shot::Reader
};

struct InferenceRuleSnapShot {
    concepts: Vec<Concept>
}

impl Default for InferenceRuleSnapShot {
    fn default() -> Self {
        let mut implication_concept = (SpecificPart::Concrete, 0).into();
        let mut true_concept = (SpecificPart::Concrete, 1).into();
        let mut result_concept = (SpecificPart::default(), 2).into();
        let mut concept_a = (SpecificPart::default(), 3).into();
        let mut cause_concept: Concept = (SpecificPart::default(), 4).into();
        let mut implies_result_concept: Concept = (SpecificPart::default(), 5).into();
        let mut cause_implies_result_concept: Concept = (SpecificPart::default(), 6).into();
        let mut example_concept = (SpecificPart::default(), 7).into();
        let mut example_composition: Concept = (SpecificPart::default(), 8).into();
        let mut variable_concept = (SpecificPart::default(), 9).into();
        let mut concept_b = (SpecificPart::default(), 10).into();
        cause_concept.make_composition_of(&mut concept_a, &mut variable_concept);
        implies_result_concept.make_composition_of(&mut implication_concept, &mut result_concept);
        cause_implies_result_concept.make_composition_of(&mut cause_concept, &mut implies_result_concept);
        cause_implies_result_concept.make_reduce_to(&mut true_concept);
        example_composition.make_composition_of(&mut concept_a, &mut example_concept);
        example_composition.make_reduce_to(&mut true_concept);
        result_concept.make_composition_of(&mut variable_concept, &mut concept_b);
        Self {
            concepts: vec![
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
                concept_b
            ]
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
    fn has_variable(&self, _delta: &ContextDelta, variable_id: usize) -> bool {
        if let 2|4|5|6|9 = variable_id {
            true
        } else {
            false
        }
    }
    fn get_label(&self, _delta: &ContextDelta, _concept_id: usize) -> Option<String> {
        None
    }
    fn concept_from_label(&self, _delta: &ContextDelta, _s: &str) -> Option<usize> {
        None
    }
    fn implication_id() -> usize {
        0
    }
    fn true_id() -> usize {
        1
    }
    fn precedence_id() -> usize {
        10
    }
    fn assoc_id() -> usize {
        10
    }
}

#[test]
fn inference_rule() {
    simple_logger::init().unwrap();
    let context_cache = ContextCache::default();
    let context_delta = ContextDelta::default();
    let context_snap_shot = InferenceRuleSnapShot::new_test_case();
    let context_search = ContextSearch::from((&context_snap_shot, &context_delta, &context_cache));
    let example_syntax = SyntaxTree::new_pair(SyntaxTree::new_concept(7), SyntaxTree::new_concept(10));
    let true_syntax = SyntaxTree::new_concept(1);
    assert_eq!(context_search.reduce(&example_syntax.into()), Some(true_syntax.into()));
}