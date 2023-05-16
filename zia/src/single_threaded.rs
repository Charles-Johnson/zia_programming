use crate::{
    ast::impl_syntax_tree,
    context::Context as GenericContext,
    context_cache::impl_cache,
    context_delta::{ContextDelta, DirectConceptDelta},
    context_search::{ContextSearch, Generalisations},
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
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
    ContextDelta<ContextConceptId, SharedDirectConceptDelta>;

#[derive(Clone, Default)]
pub struct SharedContextDelta(Rc<SingleThreadedContextDelta>);

impl<'a> From<&'a mut SharedContextDelta>
    for Option<&'a mut SingleThreadedContextDelta>
{
    fn from(scd: &'a mut SharedContextDelta) -> Self {
        Rc::get_mut(&mut scd.0)
    }
}

impl From<SingleThreadedContextDelta> for SharedContextDelta {
    fn from(mtcd: SingleThreadedContextDelta) -> Self {
        Self(mtcd.into())
    }
}

impl From<SharedContextDelta> for SingleThreadedContextDelta {
    fn from(scd: SharedContextDelta) -> Self {
        Rc::try_unwrap(scd.0).unwrap()
    }
}

impl From<SharedContextDelta> for Option<SingleThreadedContextDelta> {
    fn from(scd: SharedContextDelta) -> Self {
        Rc::try_unwrap(scd.0).ok()
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
    type Syntax = SingleThreadedSyntaxTree<S::ConceptId>;

    fn filter_generalisations_from_candidates(
        &self,
        example: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> Generalisations<Self::Syntax> {
        candidates
            .iter()
            .filter_map(|gc| {
                self.check_generalisation(example, *gc).and_then(|vm| {
                    if vm.is_empty() {
                        None
                    } else {
                        Some((*gc, vm))
                    }
                })
            })
            .collect()
    }
}
