use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch, ReductionReason},
    context_search_test::check_order,
    snap_shot::Reader as SnapShotReader,
};

struct BasicRuleSnapShot {
    concepts: Vec<Concept>,
}

impl Default for BasicRuleSnapShot {
    fn default() -> Self {
        let mut concrete_concept = (SpecificPart::Concrete, 0).into();
        let mut left_concept = (SpecificPart::default(), 2).into();
        let mut right_concept_variable = (SpecificPart::variable(), 3).into();
        let mut composite_concept = Concept::composition_of(
            1,
            &mut left_concept,
            &mut right_concept_variable,
        );
        composite_concept.make_reduce_to(&mut concrete_concept);
        Self {
            concepts: check_order(&[
                concrete_concept,
                composite_concept,
                left_concept,
                right_concept_variable,
            ]),
        }
    }
}

impl SnapShotReader for BasicRuleSnapShot {
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
    let concrete_syntax = || SyntaxTree::from("concrete").bind_concept(0);
    let left_syntax = || SyntaxTree::from("left").bind_concept(2);
    let left_and_random_syntax =
        SyntaxTree::new_pair(left_syntax(), SyntaxTree::from("random")).into();

    assert_eq!(context_search.to_ast(0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(2), left_syntax().into());

    assert_eq!(
        context_search.reduce(&left_and_random_syntax),
        Some((concrete_syntax().into(), ReductionReason::Rule{pattern: 1, reason: ReductionReason::Explicit.into()}))
    );

    assert_eq!(
        context_search.recursively_reduce(&left_and_random_syntax),
        (concrete_syntax().into(), Some(ReductionReason::Rule{pattern: 1, reason: ReductionReason::Explicit.into()}))
    );
}
