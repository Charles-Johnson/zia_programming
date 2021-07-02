use crate::{
    ast::impl_syntax_tree,
    context_cache::impl_cache,
    context_search::{
        ContextSearch, Generalisations, Iteration as ContextSearchIteration,
    },
    snap_shot::Reader as SnapShotReader,
};
use std::{collections::HashSet, fmt::Debug, rc::Rc};

impl_syntax_tree!(Rc, SingleThreadedSyntaxTree, usize);
impl_cache!(Rc, SingleThreadedContextCache, SingleThreadedSyntaxTree);

impl<'a, S> ContextSearchIteration
    for ContextSearch<'a, S, SingleThreadedContextCache>
where
    S: SnapShotReader<ConceptId = usize> + Sync + Debug,
{
    type ConceptId = S::ConceptId;
    type Syntax = SingleThreadedSyntaxTree;

    fn filter_generalisations_from_candidates(
        &self,
        example: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> Generalisations<
        Self::ConceptId,
        <Self::Syntax as SyntaxTree>::SharedSyntax,
    > {
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
