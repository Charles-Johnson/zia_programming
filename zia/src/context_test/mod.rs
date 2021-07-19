mod basic_precedence;
mod infered_precedence;

use crate::{
    context::Context as GenericContext,
    mock_snap_shot::MockSnapShot,
    multi_threaded::{
        MultiThreadedContextCache, MultiThreadedSyntaxTree,
        MultiThreadedVariableMaskList, SharedDirectConceptDelta,
    },
};

pub type Context = GenericContext<
    MockSnapShot,
    MultiThreadedContextCache,
    SharedDirectConceptDelta,
    MultiThreadedVariableMaskList<MultiThreadedSyntaxTree>,
>;
