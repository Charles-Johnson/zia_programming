use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch, ReductionReason},
    context_search_test::check_order,
    snap_shot::Reader,
};
use maplit::hashmap;

const CONCEPT_LEN: usize = 17;

struct ExistenceInferenceRuleSnapShot {
    concepts: Vec<Concept>,
}

impl Default for ExistenceInferenceRuleSnapShot {
    fn default() -> Self {
        let mut implication_concept = (SpecificPart::Concrete, 0).into();
        let mut true_concept = (SpecificPart::Concrete, 1).into();
        let mut concept_a = (SpecificPart::default(), 3).into();
        let mut example_concept = (SpecificPart::default(), 7).into();
        let mut free_variable_concept = (SpecificPart::variable(), 9).into();
        let mut concept_b = (SpecificPart::default(), 10).into();
        let mut bound_variable = (SpecificPart::variable(), 11).into();
        let mut exists_such_that_concept = (SpecificPart::Concrete, 14).into();
        let mut bound_variable_composed_with_free_variable =
            Concept::composition_of(
                13,
                &mut bound_variable,
                &mut free_variable_concept,
            );
        let mut exists_such_that_bound_variable_composed_with_free_variable =
            Concept::composition_of(
                12,
                &mut exists_such_that_concept,
                &mut bound_variable_composed_with_free_variable,
            );
        let mut cause_concept = Concept::composition_of(
            4,
            &mut bound_variable,
            &mut exists_such_that_bound_variable_composed_with_free_variable,
        );
        let mut result_concept = Concept::composition_of(
            2,
            &mut free_variable_concept,
            &mut concept_b,
        );
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
        let assoc_concept = (SpecificPart::Concrete, 15).into();
        let right_concept = (SpecificPart::Concrete, 16).into();
        let concepts: [_; CONCEPT_LEN] = [
            implication_concept,
            true_concept,
            result_concept,
            concept_a,
            cause_concept,
            implies_result_concept,
            cause_implies_result_concept,
            example_concept,
            example_composition,
            free_variable_concept,
            concept_b,
            bound_variable,
            exists_such_that_bound_variable_composed_with_free_variable,
            bound_variable_composed_with_free_variable,
            exists_such_that_concept,
            assoc_concept,
            right_concept,
        ];
        Self {
            concepts: check_order(&concepts),
        }
    }
}

impl Reader for ExistenceInferenceRuleSnapShot {
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
            0 => Some("=>".into()),
            1 => Some("true".into()),
            3 => Some("a".into()),
            7 => Some("example".into()),
            9 => Some("_y_".into()),
            10 => Some("b".into()),
            11 => Some("_x_".into()),
            14 => Some("exists_such_that".into()),
            _ => None,
        }
    }

    fn concept_from_label(
        &self,
        _delta: &ContextDelta,
        s: &str,
    ) -> Option<usize> {
        match s {
            "=>" => Some(0),
            "true" => Some(1),
            "a" => Some(3),
            "example" => Some(7),
            "_y_" => Some(9),
            "b" => Some(10),
            "_x_" => Some(11),
            "exists_such_that" => Some(14),
            _ => None,
        }
    }

    fn implication_id() -> usize {
        0
    }

    fn true_id() -> usize {
        1
    }

    fn exists_such_that_id() -> usize {
        14
    }

    fn false_id() -> usize {
        CONCEPT_LEN + 3
    }

    fn reduction_id() -> usize {
        CONCEPT_LEN + 2
    }

    fn precedence_id() -> usize {
        CONCEPT_LEN + 1
    }

    fn left_id() -> usize {
        CONCEPT_LEN
    }

    fn right_id() -> usize {
        16
    }

    fn assoc_id() -> usize {
        15
    }
}

#[test]
fn existence_inference_rule() {
    let context_cache = ContextCache::default();
    let context_delta = ContextDelta::default();
    let context_snap_shot = ExistenceInferenceRuleSnapShot::new_test_case();
    let context_search = ContextSearch::from((
        &context_snap_shot,
        &context_delta,
        &context_cache,
    ));
    let example_syntax = SyntaxTree::new_pair(
        SyntaxTree::from("example").bind_concept(7),
        SyntaxTree::from("b").bind_concept(10),
    );
    let true_syntax = SyntaxTree::from("true").bind_concept(1);
    let variable_mask = hashmap! {9 => context_search.to_ast(7)};
    assert_eq!(
        context_search.reduce(&example_syntax.into()),
        Some((
            true_syntax.into(),
            ReductionReason::Rule {
                generalisation: context_search.to_ast(2),
                variable_mask: variable_mask.clone(),
                reason: ReductionReason::Inference {
                    implication: context_search
                        .substitute(&context_search.to_ast(6), &variable_mask),
                    reason: ReductionReason::Existence {
                        example: context_search.to_ast(3),
                        reason: ReductionReason::Explicit.into()
                    }
                    .into()
                }
                .into()
            }
        ))
    );
}
