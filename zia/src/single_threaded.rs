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
>;

type SharedDirectConceptDelta = Rc<DirectConceptDelta<usize>>;

impl<'a, S, SDCD> ContextSearchIteration
    for ContextSearch<
        'a,
        S,
        SingleThreadedContextCache<
            SingleThreadedReductionReason<
                SingleThreadedSyntaxTree<S::ConceptId>,
            >,
        >,
        SDCD,
        SingleThreadedVariableMaskList<SingleThreadedSyntaxTree<S::ConceptId>>,
    >
where
    S: SnapShotReader<SDCD, ConceptId = usize> + Sync + Debug,
    SDCD: Clone
        + AsRef<DirectConceptDelta<usize>>
        + From<DirectConceptDelta<usize>>,
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
