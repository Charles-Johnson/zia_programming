use crate::{
    concepts::{Concept, ConceptTrait, ConcreteConceptType},
    context_delta::{ContextDelta, DirectConceptDelta},
    context_search_test::check_order,
    delta::Apply,
    snap_shot::Reader,
};
use bimap::BiMap;
use std::{collections::HashMap, sync::Arc};

pub type ConceptId = usize;

#[derive(Debug, Default)]
pub struct MockSnapShot {
    concrete_concepts: BiMap<ConceptId, ConcreteConceptType>,
    concept_labels: BiMap<ConceptId, &'static str>,
    concepts: Vec<Concept<ConceptId>>,
}

impl MockSnapShot {
    pub fn new_test_case(
        concepts: &[Concept<ConceptId>],
        concept_labels: &HashMap<ConceptId, &'static str>,
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

impl Apply<Arc<DirectConceptDelta<ConceptId>>> for MockSnapShot {
    fn apply(
        &mut self,
        _: ContextDelta<ConceptId, Arc<DirectConceptDelta<ConceptId>>>,
    ) {
    }
}
impl Reader<Arc<DirectConceptDelta<ConceptId>>> for MockSnapShot {
    type CommittedConceptId = ConceptId;
    type ConceptId = ConceptId;
    type MixedConcept<'a> = Concept<ConceptId>;

    fn get_concept<'a>(
        &'a self,
        concept_id: Self::ConceptId,
    ) -> Option<Self::MixedConcept<'a>> {
        self.concepts.get(concept_id).cloned()
    }

    fn get_label(
        &self,
        _: &ContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>>,
        concept_id: Self::ConceptId,
    ) -> Option<String> {
        self.concept_labels.get_by_left(&concept_id).map(|s| s.to_string())
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>>,
        s: &str,
    ) -> Option<Self::ConceptId> {
        self.concept_labels.get_by_right(&s).cloned()
    }

    fn concrete_concept_id(
        &self,
        _: &ContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>>,
        cc: ConcreteConceptType,
    ) -> Option<Self::ConceptId> {
        self.concrete_concepts.get_by_right(&cc).cloned()
    }

    fn concrete_concept_type(
        &self,
        _: &ContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>>,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.concrete_concepts.get_by_left(&concept_id).cloned()
    }
}
