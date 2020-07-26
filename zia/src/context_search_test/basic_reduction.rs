use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{ContextSearch, ReductionReason},
    context_search_test::check_order,
    snap_shot::Reader as SnapShotReader,
};

struct BasicReductionSnapShot {
    concepts: Vec<Concept>,
}

impl Default for BasicReductionSnapShot {
    fn default() -> Self {
        let mut concrete_concept = (SpecificPart::Concrete, 0).into();
        let mut abstract_concept: Concept = (SpecificPart::default(), 1).into();
        abstract_concept.make_reduce_to(&mut concrete_concept);
        Self {
            concepts: check_order(&[concrete_concept, abstract_concept]),
        }
    }
}

impl SnapShotReader for BasicReductionSnapShot {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        self.concepts.get(concept_id)
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "concrete" => Some(0),
            "abstract" => Some(1),
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
            0 => Some("concrete".into()),
            1 => Some("abstract".into()),
            _ => None,
        }
    }
}

#[test]
fn basic_reduction() {
    let snapshot = BasicReductionSnapShot::new_test_case();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicReductionSnapShot>::from((
        &snapshot, &delta, &cache,
    ));
    let abstract_syntax = || SyntaxTree::from("abstract").bind_concept(1);
    let concrete_syntax = || SyntaxTree::from("concrete").bind_concept(0);

    assert_eq!(
        context_search.recursively_reduce(&abstract_syntax().into()),
        (concrete_syntax().into(), Some(ReductionReason::Explicit))
    );
    assert_eq!(
        context_search.recursively_reduce(&concrete_syntax().into()),
        (concrete_syntax().into(), None)
    );

    assert_eq!(
        context_search.reduce(&abstract_syntax().into()),
        Some((concrete_syntax().into(), ReductionReason::Explicit))
    );
    assert_eq!(context_search.reduce(&concrete_syntax().into()), None);

    assert_eq!(
        context_search.expand(&abstract_syntax().into()),
        abstract_syntax().into()
    );
    assert_eq!(
        context_search.expand(&concrete_syntax().into()),
        concrete_syntax().into()
    );

    assert_eq!(context_search.to_ast(0), concrete_syntax().into());
    assert_eq!(context_search.to_ast(1), abstract_syntax().into());
}
