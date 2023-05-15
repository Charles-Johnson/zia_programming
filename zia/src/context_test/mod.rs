mod basic_precedence;
mod infered_precedence;

use crate::{
    context::Context as GenericContext,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        MultiThreadedContextCache, MultiThreadedReductionReason,
        MultiThreadedSyntaxTree, MultiThreadedVariableMaskList,
        SharedDirectConceptDelta, SharedContextDelta
    },
};

pub type Context = GenericContext<
    MockSnapShot,
    MultiThreadedContextCache<
        MultiThreadedReductionReason<MultiThreadedSyntaxTree<ConceptId>>,
    >,
    SharedDirectConceptDelta,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree<ConceptId>>,
    SharedContextDelta
>;

type Syntax = MultiThreadedSyntaxTree<ConceptId>;
