use crate::{
    ast::impl_syntax_tree,
    context_search::{
        ContextSearch, Generalisations, Iteration as ContextSearchIteration,
    },
    snap_shot::Reader as SnapShotReader,
};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{collections::HashSet, fmt::Debug, sync::Arc};

impl_syntax_tree!(Arc, MultiThreadedSyntaxTree, usize);
impl_cache!(Arc, MultiThreadedContextCache, MultiThreadedSyntaxTree);

impl<'a, S> ContextSearchIteration
    for ContextSearch<'a, S, MultiThreadedContextCache>
where
    S: SnapShotReader<ConceptId = usize> + Sync + Debug,
{
    type ConceptId = S::ConceptId;
    type Syntax = MultiThreadedSyntaxTree;

    fn filter_generalisations_from_candidates(
        &self,
        example: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> Generalisations<
        Self::ConceptId,
        <Self::Syntax as SyntaxTree>::SharedSyntax,
    > {
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
