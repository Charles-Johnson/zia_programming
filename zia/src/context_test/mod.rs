mod basic_precedence;
mod infered_precedence;

use crate::{
    context::Context as GenericContext,
    mock_snap_shot::MockSnapShot,
    multi_threaded::{
        MultiThreadedContextCache, MultiThreadedReductionReason,
        MultiThreadedSyntaxTree, MultiThreadedVariableMaskList,
        SharedDirectConceptDelta,
    },
};

pub type Context = GenericContext<
    MockSnapShot,
    MultiThreadedContextCache<
        MultiThreadedReductionReason<MultiThreadedSyntaxTree>,
    >,
    SharedDirectConceptDelta,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree>,
>;

type Syntax = MultiThreadedSyntaxTree;
