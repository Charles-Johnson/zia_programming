use crate::{
    ast::impl_syntax_tree,
    context::Context as GenericContext,
    context_cache::impl_cache,
    context_delta::DirectConceptDelta,
    context_search::{
        ContextSearch, Generalisations, Iteration as ContextSearchIteration,
    },
    context_snap_shot::ContextSnapShot,
    snap_shot::Reader as SnapShotReader,
};
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{collections::HashSet, fmt::Debug, sync::Arc};

impl_syntax_tree!(Arc, MultiThreadedSyntaxTree, usize);
impl_cache!(Arc, MultiThreadedContextCache, MultiThreadedSyntaxTree);
impl_variable_mask_list!(Arc, MultiThreadedVariableMaskList);

pub type Context = GenericContext<
    ContextSnapShot,
    MultiThreadedContextCache,
    SharedDirectConceptDelta,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree>,
>;

// Saves having to construct a new `Context` each time.
#[macro_export]
lazy_static! {
    pub static ref NEW_CONTEXT: Context = Context::new();
}

pub type SharedDirectConceptDelta = Arc<DirectConceptDelta<usize>>;

impl<'a, S, SDCD> ContextSearchIteration
    for ContextSearch<
        'a,
        S,
        MultiThreadedContextCache,
        SDCD,
        MultiThreadedVariableMaskList<MultiThreadedSyntaxTree>,
    >
where
    S: SnapShotReader<SDCD, ConceptId = usize> + Sync + Debug,
    SDCD: Clone
        + AsRef<DirectConceptDelta<usize>>
        + From<DirectConceptDelta<usize>>
        + Sync,
{
    type ConceptId = S::ConceptId;
    type Syntax = MultiThreadedSyntaxTree;

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
