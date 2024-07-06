use crate::{
    ast::impl_syntax_tree,
    context::Context as GenericContext,
    context_cache::impl_cache,
    context_delta::{DirectConceptDelta, NestedDelta, SharedDelta},
    context_search::ContextSearch,
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    mixed_concept::MixedConcept,
    variable_mask_list::impl_variable_mask_list,
};
use lazy_static::lazy_static;
use std::{fmt::Debug, sync::Arc};

impl_syntax_tree!(Arc, MultiThreadedSyntaxTree);
impl_cache!(Arc, MultiThreadedContextCache);
impl_variable_mask_list!(Arc, MultiThreadedVariableMaskList);
impl_reduction_reason!(Arc, MultiThreadedReductionReason);

pub type Context = GenericContext<
    ContextSnapShot,
    MultiThreadedContextCache<
        MultiThreadedReductionReason<MultiThreadedSyntaxTree<ContextConceptId>>,
    >,
    SharedDirectConceptDelta<ContextConceptId>,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<ContextConceptId>>,
    SharedContextDelta<ContextConceptId>,
    ContextConceptId,
>;

pub type MTContextSearch<'s, 'v, S, CCI> = ContextSearch<
    's,
    'v,
    S,
    MultiThreadedContextCache<
        MultiThreadedReductionReason<MultiThreadedSyntaxTree<CCI>>,
    >,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<CCI>>,
    SharedDirectConceptDelta<CCI>,
    SharedContextDelta<CCI>,
    CCI,
>;

// Saves having to construct a new `Context` each time.
lazy_static! {
    pub static ref NEW_CONTEXT: Context = Context::new().unwrap();
}

type MultiThreadedContextDelta<CCI> =
    NestedDelta<CCI, SharedDirectConceptDelta<CCI>, SharedContextDelta<CCI>>;

#[derive(Debug)]
pub struct SharedContextDelta<CCI: MixedConcept>(
    pub Arc<MultiThreadedContextDelta<CCI>>,
);

impl<CCI: MixedConcept> Clone for SharedContextDelta<CCI> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<CCI: MixedConcept> Default for SharedContextDelta<CCI> {
    fn default() -> Self {
        Self(MultiThreadedContextDelta::default().into())
    }
}

impl<CCI: MixedConcept> SharedDelta for SharedContextDelta<CCI> {
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

impl<CCI: MixedConcept> AsRef<MultiThreadedContextDelta<CCI>>
    for SharedContextDelta<CCI>
{
    fn as_ref(&self) -> &MultiThreadedContextDelta<CCI> {
        self.0.as_ref()
    }
}

pub type SharedDirectConceptDelta<CCI> = Arc<DirectConceptDelta<CCI>>;
