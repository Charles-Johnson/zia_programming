use crate::{
    ast::impl_syntax_tree,
    context::Context as GenericContext,
    context_cache::impl_cache,
    context_delta::DirectConceptDelta,
    context_search::{
        ContextSearch, Generalisations, Iteration as ContextSearchIteration,
    },
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
    SharedDirectConceptDelta<ContextConceptId>,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<ContextConceptId>>,
>;

// Saves having to construct a new `Context` each time.
lazy_static! {
    pub static ref NEW_CONTEXT: Context = Context::new();
}

pub type SharedDirectConceptDelta<ConceptId> =
    Arc<DirectConceptDelta<ConceptId>>;

impl<'a, S, SDCD> ContextSearchIteration
    for ContextSearch<
        'a,
        S,
        MultiThreadedContextCache<
            MultiThreadedReductionReason<MultiThreadedSyntaxTree<S::ConceptId>>,
        >,
        SDCD,
        MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<S::ConceptId>>,
    >
where
    S: SnapShotReader<SDCD> + Sync + Debug,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>
        + Sync,
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
