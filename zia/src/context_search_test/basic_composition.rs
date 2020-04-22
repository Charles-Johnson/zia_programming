use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextCache, ContextSearch},
    context_search_test::check_order,
    snap_shot::Reader as SnapShotReader,
};

struct BasicCompositionSnapShot {
    concepts: Vec<Concept>,
}

impl Default for BasicCompositionSnapShot {
    fn default() -> Self {
        let mut left_concept = (SpecificPart::default(), 1).into();
        let mut right_concept = (SpecificPart::default(), 2).into();
        let composite_concept =
            Concept::composition_of(0, &mut left_concept, &mut right_concept);
        Self {
            concepts: check_order(&[composite_concept, left_concept, right_concept]),
        }
    }
}

impl SnapShotReader for BasicCompositionSnapShot {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        self.concepts.get(concept_id)
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "a" => Some(0),
            "b" => Some(1),
            "c" => Some(2),
            _ => None,
        }
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
            0 => Some("a".into()),
            1 => Some("b".into()),
            2 => Some("c".into()),
            _ => None,
        }
    }
}

#[test]
fn basic_composition() {
    let snapshot = BasicCompositionSnapShot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicCompositionSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
    let left_syntax = SyntaxTree::from("b").bind_concept(1);
    let right_syntax = SyntaxTree::from("c").bind_concept(2);
    let composite_syntax = SyntaxTree::from("a")
        .bind_concept(0)
        .bind_pair(left_syntax.clone(), right_syntax.clone());
    let composite_syntax = || composite_syntax.clone();
    let left_syntax = || left_syntax.clone();
    let right_syntax = || right_syntax.clone();

    assert_eq!(
        context_search
            .contract_pair(&left_syntax().into(), &right_syntax().into()),
        composite_syntax().into()
    );

    assert_eq!(
        context_search.expand(&SyntaxTree::from("a").into()),
        composite_syntax().into()
    );

    assert_eq!(context_search.to_ast(0), composite_syntax().into());
    assert_eq!(context_search.to_ast(1), left_syntax().into());
    assert_eq!(context_search.to_ast(2), right_syntax().into());
}
