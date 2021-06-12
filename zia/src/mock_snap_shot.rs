use crate::{
    concepts::{Concept, ConcreteConceptType},
    context_delta::ContextDelta,
    context_search_test::check_order,
    delta::Apply,
    snap_shot::Reader,
};
use bimap::BiMap;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct MockSnapShot {
    concrete_concepts: BiMap<usize, ConcreteConceptType>,
    concept_labels: BiMap<usize, &'static str>,
    concepts: Vec<Concept<usize>>,
}

impl MockSnapShot {
    pub fn new_test_case(
        concepts: &[Concept<usize>],
        concept_labels: &HashMap<usize, &'static str>,
    ) -> Self {
        Self {
            concepts: check_order(concepts),
            concept_labels: concept_labels
                .iter()
                .map(|(l, r)| (*l, *r))
                .collect(),
            concrete_concepts: concepts
                .iter()
                .filter_map(|c| {
                    c.get_concrete_concept_type().map(|cc| (c.id(), cc))
                })
                .collect(),
        }
    }
}

impl Apply for MockSnapShot {
    type Delta = ContextDelta<usize>;

    fn apply(&mut self, _: Self::Delta) {}
}
impl Reader for MockSnapShot {
    type ConceptId = usize;

    fn get_concept(
        &self,
        concept_id: usize,
    ) -> Option<&Concept<Self::ConceptId>> {
        self.concepts.get(concept_id)
    }

    fn lowest_unoccupied_concept_id(
        &self,
        _: &ContextDelta<Self::ConceptId>,
    ) -> usize {
        self.concepts.len()
    }

    fn get_label(
        &self,
        _: &ContextDelta<Self::ConceptId>,
        concept_id: Self::ConceptId,
    ) -> Option<String> {
        self.concept_labels.get_by_left(&concept_id).map(|s| s.to_string())
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta<Self::ConceptId>,
        s: &str,
    ) -> Option<usize> {
        self.concept_labels.get_by_right(&s).cloned()
    }

    fn concrete_concept_id(
        &self,
        _: &ContextDelta<Self::ConceptId>,
        cc: ConcreteConceptType,
    ) -> Option<usize> {
        self.concrete_concepts.get_by_right(&cc).cloned()
    }

    fn concrete_concept_type(
        &self,
        _: &ContextDelta<Self::ConceptId>,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.concrete_concepts.get_by_left(&concept_id).cloned()
    }
}
