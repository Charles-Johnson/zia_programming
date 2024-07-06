use crate::{
    ast::impl_syntax_tree,
    context::Context as GenericContext,
    context_cache::impl_cache,
    context_delta::{DirectConceptDelta, NestedDelta, SharedDelta},
    context_search::{ContextSearch, Generalisation},
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    errors::ZiaResult,
    iteration::Iteration as ContextSearchIteration,
    snap_shot::Reader as SnapShotReader,
    variable_mask_list::impl_variable_mask_list,
};
use std::{collections::HashSet, fmt::Debug, rc::Rc};

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

impl<'s, 'v, S> ContextSearchIteration
    for ContextSearch<
        's,
        'v,
        S,
        SingleThreadedContextCache<
            SingleThreadedReductionReason<
                SingleThreadedSyntaxTree<S::ConceptId>,
            >,
        >,
        SingleThreadedVariableMaskList<SingleThreadedSyntaxTree<S::ConceptId>>,
        SharedDirectConceptDelta,
        SharedContextDelta,
        ContextConceptId,
    >
where
    S: SnapShotReader<SharedDirectConceptDelta, ConceptId = ContextConceptId>
        + Sync
        + Debug,
{
    type ConceptId = S::ConceptId;

    fn filter_generalisations_from_candidates(
        &'a self,
        example: <SingleThreadedSyntaxTree<S::ConceptId> as SyntaxTree>::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> impl Iterator<
        Item = Generalisation<SingleThreadedSyntaxTree<S::ConceptId>>,
    > + 'a {
        candidates.into_iter().filter_map(move |gc| {
            self.check_generalisation(&example, &gc).and_then(|vm| {
                if vm.is_empty() {
                    None
                } else {
                    Some((gc, vm))
                }
            })
        })
    }
}
