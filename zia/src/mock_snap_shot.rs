use crate::{
    concepts::{Concept, ConceptTrait, ConcreteConceptType},
    context_delta::{DirectConceptDelta, NestedContextDelta},
    context_search_test::check_order,
    delta::Apply,
    snap_shot::Reader,
};
use bimap::BiMap;
use std::{collections::HashMap, sync::Arc, fmt::Debug};

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
    fn apply<D: AsRef<NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>> + Debug>(
        &mut self,
        _: NestedContextDelta<ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>,
    ) {
    }
}
impl Reader<Arc<DirectConceptDelta<ConceptId>>> for MockSnapShot {
    type CommittedConceptId = ConceptId;
    type ConceptId = ConceptId;
    type MixedConcept<'a> = Concept<ConceptId>;

    fn get_concept(
        &self,
        concept_id: Self::ConceptId,
    ) -> Option<Self::MixedConcept<'_>> {
        self.concepts.get(concept_id).cloned()
    }

    fn get_label<D: AsRef<NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>> + Debug>(
        &self,
        _: &NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>,
        concept_id: Self::ConceptId,
    ) -> Option<String> {
        self.concept_labels.get_by_left(&concept_id).map(|s| (*s).to_string())
    }

    fn concept_from_label<D: AsRef<NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>> + Debug>(
        &self,
        _: &NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>,
        s: &str,
    ) -> Option<Self::ConceptId> {
        self.concept_labels.get_by_right(&s).copied()
    }

    fn concrete_concept_id<D: AsRef<NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>> + Debug>(
        &self,
        _: &NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>,
        cc: ConcreteConceptType,
    ) -> Option<Self::ConceptId> {
        self.concrete_concepts.get_by_right(&cc).copied()
    }

    fn concrete_concept_type<D: AsRef<NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>> + Debug>(
        &self,
        _: &NestedContextDelta<Self::ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.concrete_concepts.get_by_left(&concept_id).copied()
    }
}
