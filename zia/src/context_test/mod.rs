mod basic_precedence;
mod infered_precedence;

use crate::{
    context::Context as GenericContext,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        MultiThreadedContextCache, MultiThreadedReductionReason,
        MultiThreadedSyntaxTree, MultiThreadedVariableMaskList,
        SharedContextDelta, SharedDirectConceptDelta,
    },
};

pub type Context = GenericContext<
    MockSnapShot,
    MultiThreadedContextCache<
        MultiThreadedReductionReason<MultiThreadedSyntaxTree<ConceptId>>,
    >,
    SharedDirectConceptDelta<ConceptId>,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<ConceptId>>,
    SharedContextDelta<ConceptId>,
    ConceptId,
>;

type Syntax = MultiThreadedSyntaxTree<ConceptId>;
