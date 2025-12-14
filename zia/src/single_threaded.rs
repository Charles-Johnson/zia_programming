use crate::{
    context::Context as GenericContext,
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    nester::SharedReference,
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

pub type Context =
    GenericContext<ContextSnapShot<RcFamily>, ContextConceptId, RcFamily>;
