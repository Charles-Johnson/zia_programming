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
struct BasicCompositionSnapShot;

lazy_static! {
    static ref CONCEPTS: (Concept, Concept, Concept) = {
        let mut composite_concept: Concept =
            (SpecificPart::default(), 0).into();
        let mut left_concept: Concept = (SpecificPart::default(), 1).into();
        let mut right_concept: Concept = (SpecificPart::default(), 2).into();
        composite_concept
            .make_composition_of(&mut left_concept, &mut right_concept);
        (composite_concept, left_concept, right_concept)
    };
    static ref SYNTAX: (SyntaxTree, SyntaxTree, SyntaxTree) = {
        let left_syntax = SyntaxTree::from("b").bind_concept(1);
        let right_syntax = SyntaxTree::from("c").bind_concept(2);
        let composite_syntax = SyntaxTree::from("a").bind_concept(0).bind_pair(
            &Arc::new(left_syntax.clone()),
            &Arc::new(right_syntax.clone()),
        );
        (composite_syntax, left_syntax, right_syntax)
    };
}

impl SnapShotReader for BasicCompositionSnapShot {
    fn read_concept(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Concept {
        let (composite_concept, left_concept, right_concept) = CONCEPTS.clone();
        match concept_id {
            0 => composite_concept,
            1 => left_concept,
            2 => right_concept,
            _ => panic!("No concepts with id: {}", concept_id),
        }
    }

    fn true_id() -> usize {
        unimplemented!()
    }

    fn implication_id() -> usize {
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

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        _label: &str,
    ) -> Option<usize> {
        unimplemented!()
    }

    fn has_variable(&self, _delta: &ContextDelta, _variable_id: usize) -> bool {
        false
    }

    fn concept_len(&self, _delta: &ContextDelta) -> usize {
        3
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            0 => Some("a".into()),
            1 => Some("b".into()),
            2 => Some("c".into()),
            _ => None,
        }
    }

    fn ast_from_symbol(
        &self,
        _delta: &ContextDelta,
        symbol: &str,
    ) -> SyntaxTree {
        let (composite_syntax, left_syntax, right_syntax) = SYNTAX.clone();
        match symbol {
            "a" => composite_syntax.clone(),
            "b" => left_syntax.clone(),
            "c" => right_syntax.clone(),
            _ => symbol.into(),
        }
    }
}

#[test]
fn basic_composition() {
    let snapshot = BasicCompositionSnapShot::default();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicCompositionSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
    let (composite_syntax, left_syntax, right_syntax) = SYNTAX.clone();
    let composite_syntax = Arc::new(composite_syntax);
    let left_syntax = Arc::new(left_syntax);
    let right_syntax = Arc::new(right_syntax);

    assert_eq!(
        context_search.ast_from_expression("b c"),
        Ok(Arc::new(
            SyntaxTree::new_concept(0).bind_pair(&left_syntax, &right_syntax)
        ))
    );
    assert_eq!(
        context_search.ast_from_expression("a"),
        Ok(composite_syntax.clone())
    );
    assert_eq!(
        context_search.ast_from_expression("b"),
        Ok(left_syntax.clone())
    );
    assert_eq!(
        context_search.ast_from_expression("c"),
        Ok(right_syntax.clone())
    );

    assert_eq!(
        context_search.contract_pair(&left_syntax, &right_syntax),
        composite_syntax
    );

    assert_eq!(context_search.expand(&Arc::new("a".into())), composite_syntax);

    assert_eq!(context_search.to_ast(0), composite_syntax);
    assert_eq!(context_search.to_ast(1), left_syntax);
    assert_eq!(context_search.to_ast(2), right_syntax);
}
