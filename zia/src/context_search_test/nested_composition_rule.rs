use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart, ConcreteConceptType},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{ContextSearch, ReductionReason},
    context_search_test::check_order,
    snap_shot::Reader as SnapShotReader,
};
use maplit::hashmap;

struct NestedCompositionRuleSnapshot {
    concepts: Vec<Concept>,
}

const CONCEPT_LENGTH: usize = 8;

impl Default for NestedCompositionRuleSnapshot {
    fn default() -> Self {
        let mut concrete_concept = (ConcreteConceptType::True, 0).into();
        let mut left_concept = (SpecificPart::default(), 2).into();
        let mut right_left_concept = (SpecificPart::default(), 3).into();
        let mut right_right_concept_variable = (SpecificPart::variable(), 4).into();
        let mut right_composite_concept = Concept::composition_of(5, &mut right_left_concept, &mut right_right_concept_variable);
        let mut composite_concept = Concept::composition_of(
            1,
            &mut left_concept,
            &mut right_composite_concept,
        );
        let assoc_concept = (ConcreteConceptType::Associativity, 6).into();
        let right_id_concept = (ConcreteConceptType::Right, 7).into();
        composite_concept.make_reduce_to(&mut concrete_concept);
        let concepts: [Concept; CONCEPT_LENGTH] = [
            concrete_concept,
            composite_concept,
            left_concept,
            right_left_concept,
            right_right_concept_variable,
            right_composite_concept,
            assoc_concept,
            right_id_concept
        ];
        Self {
            concepts: check_order(&concepts),
        }
    }
}

impl SnapShotReader for NestedCompositionRuleSnapshot {
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
            0 => Some("concrete".into()),
            2 => Some("left".into()),
            3 => Some("right_left".into()),
            4 => Some("_y_".into()),
            _ => None,
        }
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "concrete" => Some(0),
            "left" => Some(2),
            "right_left" => Some(3),
            "_y_" => Some(4),
            _ => None,
        }
    }

    fn assoc_id() -> usize {
        6
    }

    fn left_id() -> usize {
        CONCEPT_LENGTH + 1
    }

    fn right_id() -> usize {
        7
    }

    fn precedence_id() -> usize {
        CONCEPT_LENGTH
    }

    fn reduction_id() -> usize {
        CONCEPT_LENGTH + 2
    }

    fn exists_such_that_id() -> usize {
        CONCEPT_LENGTH + 3
    }

    fn implication_id() -> usize {
        CONCEPT_LENGTH + 4
    }
}

#[test]
fn basic_rule() {
    let snapshot = NestedCompositionRuleSnapshot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<NestedCompositionRuleSnapshot>::from((&snapshot, &delta, &cache));
    let concrete_syntax = || SyntaxTree::from("concrete").bind_concept(0);
    let left_syntax = || SyntaxTree::from("left").bind_concept(2);
    let right_left_syntax = SyntaxTree::from("right_left").bind_concept(3);
    let left_and_right_left_and_random_syntax =
        SyntaxTree::new_pair(left_syntax(), SyntaxTree::new_pair(right_left_syntax, SyntaxTree::from("random"))).into();

    assert_eq!(context_search.to_ast(0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(2), left_syntax().into());

    let reduction_reason = ReductionReason::Rule {
        generalisation: context_search.to_ast(1),
        variable_mask: hashmap! {4 => SyntaxTree::from("random").into()},
        reason: ReductionReason::Explicit.into(),
    };

    assert_eq!(
        context_search.reduce(&left_and_right_left_and_random_syntax),
        Some((concrete_syntax().into(), reduction_reason.clone()))
    );

    assert_eq!(
        context_search.recursively_reduce(&left_and_right_left_and_random_syntax),
        (concrete_syntax().into(), Some(reduction_reason))
    );
}
