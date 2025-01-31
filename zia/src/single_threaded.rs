use crate::{
    context::Context as GenericContext,
    context_delta::{DirectConceptDelta, NestedDelta, SharedDelta},
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    errors::ZiaResult,
    nester::SharedReference,
    variable_mask_list::GenericVariableMaskList,
};
use std::{fmt::Debug, rc::Rc};
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct RcFamily;

impl SharedReference for RcFamily {
    type Share<T> = Rc<T>;

    fn share<T>(owned: T) -> Self::Share<T> {
        Rc::new(owned)
    }

    fn make_mut<T: Clone>(refcounter: &mut Self::Share<T>) -> &mut T {
        Rc::make_mut(refcounter)
    }
}

type SingleThreadedVariableMaskList<CI> = GenericVariableMaskList<CI, RcFamily>;

pub type Context = GenericContext<
    ContextSnapShot<RcFamily>,
    SharedDirectConceptDelta,
    SingleThreadedVariableMaskList<ContextConceptId>,
    SharedContextDelta,
    ContextConceptId,
    RcFamily,
>;

type SingleThreadedContextDelta = NestedDelta<
    ContextConceptId,
    SharedDirectConceptDelta,
    SharedContextDelta,
    RcFamily,
>;

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
