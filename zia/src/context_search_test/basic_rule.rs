use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    snap_shot::Reader as SnapShotReader,
};
use lazy_static::lazy_static;

#[derive(Default)]
struct BasicRuleSnapShot;

lazy_static! {
    static ref CONCEPTS: [Concept; 4] = {
        let mut concrete_concept = (SpecificPart::Concrete, 0).into();
        let mut composite_concept: Concept =
            (SpecificPart::default(), 1).into();
        composite_concept.make_reduce_to(&mut concrete_concept);
        let mut left_concept = (SpecificPart::default(), 2).into();
        let mut right_concept_variable = (SpecificPart::default(), 3).into();
        composite_concept.make_composition_of(
            &mut left_concept,
            &mut right_concept_variable,
        );
        [
            concrete_concept,
            composite_concept,
            left_concept,
            right_concept_variable,
        ]
    };
    static ref CONCRETE_SYNTAX: SyntaxTree =
        SyntaxTree::from("concrete").bind_concept(0);
    static ref LEFT_SYNTAX: SyntaxTree =
        SyntaxTree::from("left").bind_concept(2);
}

impl SnapShotReader for BasicRuleSnapShot {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        CONCEPTS.get(concept_id)
    }

    fn has_variable(&self, _delta: &ContextDelta, variable_id: usize) -> bool {
        variable_id == 3 || variable_id == 1
    }

    fn lowest_unoccupied_concept_id(&self, _delta: &ContextDelta) -> usize {
        4
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            0 => Some("concrete".into()),
            1 => None,
            2 => Some("left".into()),
            3 => None,
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
            _ => None,
        }
    }

    fn assoc_id() -> usize {
        5
    }

    fn precedence_id() -> usize {
        4
    }
}

#[test]
fn basic_rule() {
    let snapshot = BasicRuleSnapShot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<BasicRuleSnapShot>::from((&snapshot, &delta, &cache));
    let concrete_syntax = || CONCRETE_SYNTAX.clone();
    let left_syntax = || LEFT_SYNTAX.clone();
    let left_and_random_syntax =
        SyntaxTree::new_pair(left_syntax(), SyntaxTree::from("random")).into();

    assert_eq!(context_search.to_ast(0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(2), left_syntax().into());

    assert_eq!(
        context_search.reduce(&left_and_random_syntax),
        Some(concrete_syntax().into())
    );

    assert_eq!(
        context_search.recursively_reduce(&left_and_random_syntax),
        concrete_syntax().into()
    );
}
