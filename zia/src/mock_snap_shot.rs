use crate::{
    concepts::{Concept, ConceptTrait, ConcreteConceptType},
    context_delta::{
        DirectConceptDelta, NestedContextDelta, SharedDelta, ValueChange,
    },
    context_search_test::check_order,
    delta::Apply,
    mixed_concept::MixedConcept,
    snap_shot::Reader,
};
use bimap::BiMap;
use std::{
    collections::HashMap,
    convert::TryFrom,
    fmt::{Debug, Display},
    sync::Arc,
};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ConceptId {
    Committed(usize),
    Uncommitted(usize),
}

impl MixedConcept for ConceptId {
    fn uncommitted(id: usize) -> Self {
        Self::Uncommitted(id)
    }
}

impl From<usize> for ConceptId {
    fn from(value: usize) -> Self {
        Self::Committed(value)
    }
}

impl TryFrom<ConceptId> for usize {
    type Error = ();

    fn try_from(value: ConceptId) -> Result<Self, Self::Error> {
        match value {
            ConceptId::Committed(x) => Ok(x),
            ConceptId::Uncommitted(_) => Err(()),
        }
    }
}

impl Display for ConceptId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Committed(cci) => {
                Display::fmt("CommittedConceptId: ", f)?;
                Display::fmt(cci, f)
            },
            Self::Uncommitted(uci) => {
                Display::fmt("UncommittedConceptId: ", f)?;
                Display::fmt(uci, f)
            },
        }
    }
}

#[derive(Debug, Default)]
pub struct MockSnapShot {
    concrete_concepts: BiMap<ConceptId, ConcreteConceptType>,
    concept_labels: BiMap<ConceptId, &'static str>,
    concepts: Vec<Concept<ConceptId>>,
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
                .map(|(l, r)| ((*l).into(), *r))
                .collect(),
            concrete_concepts: concepts
                .iter()
                .filter_map(|c| {
                    c.get_concrete_concept_type().map(|cc| (c.id().into(), cc))
                })
                .collect(),
        }
    }
}

impl Apply<Arc<DirectConceptDelta<ConceptId>>> for MockSnapShot {
    fn apply<
        D: SharedDelta<
            NestedDelta = NestedContextDelta<
                Self::ConceptId,
                Arc<DirectConceptDelta<ConceptId>>,
                D,
            >,
        >,
    >(
        &mut self,
        _: NestedContextDelta<ConceptId, Arc<DirectConceptDelta<ConceptId>>, D>,
    ) {
    }
}
impl Reader<Arc<DirectConceptDelta<ConceptId>>> for MockSnapShot {
    type CommittedConceptId = usize;
    type ConceptId = ConceptId;
    type MixedConcept<'a> = Concept<ConceptId>;

    fn get_concept(
        &self,
        concept_id: Self::ConceptId,
    ) -> Option<Self::MixedConcept<'_>> {
        match concept_id {
            ConceptId::Committed(concept_id) => {
                self.concepts.get(concept_id).cloned()
            },
            ConceptId::Uncommitted(_) => None,
        }
    }

    fn get_label<
        D: SharedDelta<
            NestedDelta = NestedContextDelta<
                Self::ConceptId,
                Arc<DirectConceptDelta<ConceptId>>,
                D,
            >,
        >,
    >(
        &self,
        delta: &NestedContextDelta<
            Self::ConceptId,
            Arc<DirectConceptDelta<ConceptId>>,
            D,
        >,
        concept_id: Self::ConceptId,
    ) -> Option<String> {
        let mut iter = delta.iter_string();
        if let Some((string, value_change)) = iter.next() {
            if ValueChange::Create(concept_id) == value_change {
                return Some(string.into());
            }
        }
        self.concept_labels.get_by_left(&concept_id).map(|s| (*s).to_string())
    }

    fn concept_from_label<
        D: SharedDelta<
            NestedDelta = NestedContextDelta<
                Self::ConceptId,
                Arc<DirectConceptDelta<ConceptId>>,
                D,
            >,
        >,
    >(
        &self,
        delta: &NestedContextDelta<
            Self::ConceptId,
            Arc<DirectConceptDelta<ConceptId>>,
            D,
        >,
        s: &str,
    ) -> Option<Self::ConceptId> {
        if let Some(ValueChange::Create(concept_id)) = delta.get_string(s) {
            return Some(concept_id);
        }
        self.concept_labels.get_by_right(&s).copied()
    }

    fn concrete_concept_id<
        D: SharedDelta<
            NestedDelta = NestedContextDelta<
                Self::ConceptId,
                Arc<DirectConceptDelta<ConceptId>>,
                D,
            >,
        >,
    >(
        &self,
        _: &NestedContextDelta<
            Self::ConceptId,
            Arc<DirectConceptDelta<ConceptId>>,
            D,
        >,
        cc: ConcreteConceptType,
    ) -> Option<Self::ConceptId> {
        self.concrete_concepts.get_by_right(&cc).copied()
    }

    fn concrete_concept_type<
        D: SharedDelta<
            NestedDelta = NestedContextDelta<
                Self::ConceptId,
                Arc<DirectConceptDelta<ConceptId>>,
                D,
            >,
        >,
    >(
        &self,
        _: &NestedContextDelta<
            Self::ConceptId,
            Arc<DirectConceptDelta<ConceptId>>,
            D,
        >,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.concrete_concepts.get_by_left(&concept_id).copied()
    }
}
