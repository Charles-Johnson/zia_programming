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
struct BasicRuleSnapShot;

lazy_static! {
    static ref CONCEPTS: (Concept, Concept, Concept, Concept) = {
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
        (
            concrete_concept,
            composite_concept,
            left_concept,
            right_concept_variable,
        )
    };
    static ref CONCRETE_SYNTAX: SyntaxTree =
        SyntaxTree::from("concrete").bind_concept(0);
    static ref LEFT_SYNTAX: SyntaxTree =
        SyntaxTree::from("left").bind_concept(2);
}

impl SnapShotReader for BasicRuleSnapShot {
    fn read_concept(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Concept {
        let (
            concrete_concept,
            composite_concept,
            left_concept,
            right_concept_variable,
        ) = CONCEPTS.clone();
        match concept_id {
            0 => concrete_concept,
            1 => composite_concept,
            2 => left_concept,
            3 => right_concept_variable,
            _ => panic!("No concepts with id: {}", concept_id),
        }
    }

    fn has_variable(&self, _delta: &ContextDelta, variable_id: usize) -> bool {
        variable_id == 3 || variable_id == 1
    }

    fn concept_len(&self, _delta: &ContextDelta) -> usize {
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

    fn ast_from_symbol(
        &self,
        _delta: &ContextDelta,
        symbol: &str,
    ) -> SyntaxTree {
        match symbol {
            "concrete" => CONCRETE_SYNTAX.clone(),
            "left" => LEFT_SYNTAX.clone(),
            _ => symbol.into(),
        }
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        _label: &str,
    ) -> Option<usize> {
        unimplemented!()
    }

    fn false_id() -> usize {
        unimplemented!()
    }

    fn reduction_id() -> usize {
        unimplemented!()
    }

    fn assoc_id() -> usize {
        5
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

    fn true_id() -> usize {
        unimplemented!()
    }

    fn implication_id() -> usize {
        unimplemented!()
    }

    fn precedence_id() -> usize {
        4
    }

    fn greater_than_id() -> usize {
        unimplemented!()
    }

    fn default_id() -> usize {
        unimplemented!()
    }
}

#[test]
fn basic_rule() {
    let snapshot = BasicRuleSnapShot::default();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search =
        ContextSearch::<BasicRuleSnapShot>::from((&snapshot, &delta, &cache));
    let concrete_syntax = Arc::new(CONCRETE_SYNTAX.clone());
    let left_syntax = Arc::new(LEFT_SYNTAX.clone());
    let random_syntax = Arc::new(SyntaxTree::from("random"));
    let left_and_random_syntax = Arc::new(
        SyntaxTree::from("left random").bind_pair(&left_syntax, &random_syntax),
    );

    assert_eq!(
        context_search.ast_from_expression("left"),
        Ok(left_syntax.clone())
    );
    assert_eq!(
        context_search.ast_from_expression("concrete"),
        Ok(concrete_syntax.clone())
    );

    assert_eq!(context_search.to_ast(0), concrete_syntax);
    assert_eq!(context_search.to_ast(2), left_syntax);

    assert_eq!(
        context_search.reduce(&left_and_random_syntax),
        Some(concrete_syntax.clone())
    );

    assert_eq!(
        context_search.recursively_reduce(&left_and_random_syntax),
        concrete_syntax
    );
}
