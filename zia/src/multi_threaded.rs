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
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    collections::HashSet,
    fmt::{Debug, Display},
    sync::Arc,
};

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
    SharedContextDelta<ContextConceptId>,
    ContextConceptId,
>;

// Saves having to construct a new `Context` each time.
lazy_static! {
    pub static ref NEW_CONTEXT: Context = Context::new().unwrap();
}

type MultiThreadedContextDelta<CCI> =
    ContextDelta<CCI, SharedDirectConceptDelta<CCI>>;

#[derive(Clone)]
pub struct SharedContextDelta<CCI: Clone + Display>(
    pub Arc<MultiThreadedContextDelta<CCI>>,
);

impl<CCI: Clone + Display> Default for SharedContextDelta<CCI> {
    fn default() -> Self {
        Self(MultiThreadedContextDelta::default().into())
    }
}

impl<'a, CCI: Clone + Display> From<&'a mut SharedContextDelta<CCI>>
    for Option<&'a mut MultiThreadedContextDelta<CCI>>
{
    fn from(scd: &'a mut SharedContextDelta<CCI>) -> Self {
        Arc::get_mut(&mut scd.0)
    }
}

impl<CCI: Clone + Display> From<MultiThreadedContextDelta<CCI>>
    for SharedContextDelta<CCI>
{
    fn from(mtcd: MultiThreadedContextDelta<CCI>) -> Self {
        Self(mtcd.into())
    }
}

impl<CCI: Clone + Debug + Display + Eq + Hash> From<SharedContextDelta<CCI>>
    for MultiThreadedContextDelta<CCI>
{
    fn from(scd: SharedContextDelta<CCI>) -> Self {
        Arc::try_unwrap(scd.0).unwrap()
    }
}

impl<CCI: Clone + Display> From<SharedContextDelta<CCI>>
    for Option<MultiThreadedContextDelta<CCI>>
{
    fn from(scd: SharedContextDelta<CCI>) -> Self {
        Arc::try_unwrap(scd.0).ok()
    }
}

impl<CCI: Clone + Display> AsRef<MultiThreadedContextDelta<CCI>>
    for SharedContextDelta<CCI>
{
    fn as_ref(&self) -> &MultiThreadedContextDelta<CCI> {
        self.0.as_ref()
    }
}

pub type SharedDirectConceptDelta<CCI> = Arc<DirectConceptDelta<CCI>>;

impl<
        's,
        'v,
        S,
        CCI: Clone + Display + Copy + Debug + Eq + Hash + Send + Sync + From<usize>,
    > ContextSearchIteration
    for ContextSearch<
        's,
        'v,
        S,
        MultiThreadedContextCache<
            MultiThreadedReductionReason<MultiThreadedSyntaxTree<S::ConceptId>>,
        >,
        MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<S::ConceptId>>,
        SharedDirectConceptDelta<S::ConceptId>,
        SharedContextDelta<S::ConceptId>,
        CCI,
    >
where
    S: SnapShotReader<SharedDirectConceptDelta<CCI>, ConceptId = CCI>
        + Sync
        + Debug,
    for<'a> &'a std::collections::HashSet<CCI>:
        rayon::iter::IntoParallelIterator<Item = &'a CCI>,
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
