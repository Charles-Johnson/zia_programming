use crate::{
    ast::impl_syntax_tree,
    context::Context as GenericContext,
    context_cache::impl_cache,
    context_delta::{DirectConceptDelta, NestedDelta, SharedDelta},
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    errors::ZiaResult,
    variable_mask_list::impl_variable_mask_list,
};
use std::{fmt::Debug, rc::Rc};

impl_syntax_tree!(Rc, SingleThreadedSyntaxTree);
impl_cache!(Rc, SingleThreadedContextCache);
impl_variable_mask_list!(Rc, SingleThreadedVariableMaskList);
impl_reduction_reason!(Rc, SingleThreadedReductionReason);

pub type Context = GenericContext<
    ContextSnapShot,
    SingleThreadedContextCache<
        SingleThreadedReductionReason<
            SingleThreadedSyntaxTree<ContextConceptId>,
        >,
    >,
    SharedDirectConceptDelta,
    SingleThreadedVariableMaskList<SingleThreadedSyntaxTree<ContextConceptId>>,
    SharedContextDelta,
    ContextConceptId,
>;

type SingleThreadedContextDelta =
    NestedDelta<ContextConceptId, SharedDirectConceptDelta, SharedContextDelta>;

#[derive(Clone, Default, Debug)]
pub struct SharedContextDelta(Rc<SingleThreadedContextDelta>);

impl SharedDelta for SharedContextDelta {
    type NestedDelta = SingleThreadedContextDelta;

    fn get_mut(&mut self) -> Option<&mut Self::NestedDelta> {
        Rc::get_mut(&mut self.0)
    }

    fn from_nested(nested: Self::NestedDelta) -> Self {
        Self(nested.into())
    }

    fn into_nested(self) -> ZiaResult<Self::NestedDelta> {
        Rc::try_unwrap(self.0)
            .map_err(|_| crate::ZiaError::MultiplePointersToDelta)
    }

    fn strong_count(&self) -> usize {
        Rc::strong_count(&self.0)
    }
}

impl AsRef<SingleThreadedContextDelta> for SharedContextDelta {
    fn as_ref(&self) -> &SingleThreadedContextDelta {
        self.0.as_ref()
    }
}

type SharedDirectConceptDelta = Rc<DirectConceptDelta<ContextConceptId>>;
