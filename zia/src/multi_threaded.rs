use crate::{
    ast::impl_syntax_tree,
    context::Context as GenericContext,
    context_cache::impl_cache,
    context_delta::{DirectConceptDelta, ContextDelta},
    context_search::{
        ContextSearch, Generalisations,
    },
    iteration::Iteration as ContextSearchIteration,
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    snap_shot::Reader as SnapShotReader,
    variable_mask_list::impl_variable_mask_list,
};
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{collections::HashSet, fmt::Debug, sync::Arc};

impl_syntax_tree!(Arc, MultiThreadedSyntaxTree);
impl_cache!(Arc, MultiThreadedContextCache);
impl_variable_mask_list!(Arc, MultiThreadedVariableMaskList);
impl_reduction_reason!(Arc, MultiThreadedReductionReason);

pub type Context = GenericContext<
    ContextSnapShot,
    MultiThreadedContextCache<
        MultiThreadedReductionReason<MultiThreadedSyntaxTree<ContextConceptId>>,
    >,
    SharedDirectConceptDelta,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<ContextConceptId>>,
    SharedContextDelta
>;

// Saves having to construct a new `Context` each time.
lazy_static! {
    pub static ref NEW_CONTEXT: Context = Context::new().unwrap();
}

type MultiThreadedContextDelta = ContextDelta<ContextConceptId, SharedDirectConceptDelta>;

#[derive(Clone, Default)]
pub struct SharedContextDelta(Arc<MultiThreadedContextDelta>);

impl<'a> From<&'a mut SharedContextDelta> for Option<&'a mut MultiThreadedContextDelta> {
    fn from(scd: &'a mut SharedContextDelta) -> Self {
        Arc::get_mut(&mut scd.0)
    }
}

impl From<MultiThreadedContextDelta> for SharedContextDelta {
    fn from(mtcd: MultiThreadedContextDelta) -> Self {
        Self(mtcd.into())
    }
}

impl From<SharedContextDelta> for MultiThreadedContextDelta {
    fn from(scd: SharedContextDelta) -> Self {
        Arc::try_unwrap(scd.0).unwrap()
    }
}

impl From<SharedContextDelta> for Option<MultiThreadedContextDelta> {
    fn from(scd: SharedContextDelta) -> Self {
        Arc::try_unwrap(scd.0).ok()
    }
}

impl AsRef<MultiThreadedContextDelta> for SharedContextDelta {
    fn as_ref(&self) -> &MultiThreadedContextDelta {
        self.0.as_ref()
    }
}


pub type SharedDirectConceptDelta =
    Arc<DirectConceptDelta<ContextConceptId>>;


impl<'s, 'v, S> ContextSearchIteration
    for ContextSearch<
        's,
        'v,
        S,
        MultiThreadedContextCache<
            MultiThreadedReductionReason<MultiThreadedSyntaxTree<S::ConceptId>>,
        >,
        MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<S::ConceptId>>,
        SharedDirectConceptDelta,
        SharedContextDelta
    >
where
    S: SnapShotReader<SharedDirectConceptDelta, ConceptId = ContextConceptId> + Sync + Debug,
{
    type ConceptId = S::ConceptId;
    type Syntax = MultiThreadedSyntaxTree<S::ConceptId>;

    fn filter_generalisations_from_candidates(
        &self,
        example: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> Generalisations<Self::Syntax> {
        candidates
            .par_iter()
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
