use crate::{
    ast::GenericSyntaxTree,
    context::Context as GenericContext,
    context_cache::GenericCache,
    context_delta::{DirectConceptDelta, NestedDelta, SharedDelta},
    context_search::ContextSearch,
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    mixed_concept::ConceptId,
    nester::SharedReference,
    reduction_reason::GenericReductionReason,
    variable_mask_list::GenericVariableMaskList,
};
use lazy_static::lazy_static;
use std::{fmt::Debug, sync::Arc};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ArcFamily;

impl SharedReference for ArcFamily {
    type Share<T> = Arc<T>;

    fn share<T>(owned: T) -> Self::Share<T> {
        Arc::new(owned)
    }

    fn make_mut<T: Clone>(refcounter: &mut Self::Share<T>) -> &mut T {
        Arc::make_mut(refcounter)
    }
}

pub type MultiThreadedSyntaxTree<CI> = GenericSyntaxTree<CI, ArcFamily>;
pub type MultiThreadedContextCache<CI> = GenericCache<CI, ArcFamily>;
pub type MultiThreadedVariableMaskList<CI> =
    GenericVariableMaskList<CI, ArcFamily>;
pub type MultiThreadedReductionReason<CI> =
    GenericReductionReason<CI, ArcFamily>;

pub type Context = GenericContext<
    ContextSnapShot<ArcFamily>,
    SharedDirectConceptDelta<ContextConceptId>,
    MultiThreadedVariableMaskList<ContextConceptId>,
    SharedContextDelta<ContextConceptId>,
    ContextConceptId,
    ArcFamily,
>;

pub type MTContextSearch<'s, 'v, S, CCI> = ContextSearch<
    's,
    'v,
    S,
    MultiThreadedVariableMaskList<CCI>,
    SharedDirectConceptDelta<CCI>,
    SharedContextDelta<CCI>,
    CCI,
    ArcFamily,
>;

// Saves having to construct a new `Context` each time.
lazy_static! {
    pub static ref NEW_CONTEXT: Context = Context::new().unwrap();
}

type MultiThreadedContextDelta<CCI> = NestedDelta<
    CCI,
    SharedDirectConceptDelta<CCI>,
    SharedContextDelta<CCI>,
    ArcFamily,
>;

#[derive(Debug)]
pub struct SharedContextDelta<CCI: ConceptId>(
    pub Arc<MultiThreadedContextDelta<CCI>>,
);

impl<CCI: ConceptId> Clone for SharedContextDelta<CCI> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<CCI: ConceptId> Default for SharedContextDelta<CCI> {
    fn default() -> Self {
        Self(MultiThreadedContextDelta::default().into())
    }
}

impl<CCI: ConceptId> SharedDelta for SharedContextDelta<CCI> {
    type NestedDelta = MultiThreadedContextDelta<CCI>;

    fn get_mut(&mut self) -> Option<&mut Self::NestedDelta> {
        Arc::get_mut(&mut self.0)
    }

    fn from_nested(nested: Self::NestedDelta) -> Self {
        Self(nested.into())
    }

    fn into_nested(self) -> crate::errors::ZiaResult<Self::NestedDelta> {
        Arc::try_unwrap(self.0)
            .map_err(|_| crate::ZiaError::MultiplePointersToDelta)
    }

    fn strong_count(&self) -> usize {
        Arc::strong_count(&self.0)
    }
}

impl<CCI: ConceptId> AsRef<MultiThreadedContextDelta<CCI>>
    for SharedContextDelta<CCI>
{
    fn as_ref(&self) -> &MultiThreadedContextDelta<CCI> {
        self.0.as_ref()
    }
}

pub type SharedDirectConceptDelta<CCI> = Arc<DirectConceptDelta<CCI>>;
