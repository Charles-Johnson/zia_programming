mod basic_precedence;
mod infered_precedence;

use crate::{
    context::Context as GenericContext,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{
        ArcFamily, MTSyntaxTree, SharedContextDelta, SharedDirectConceptDelta,
    },
};

pub type Context = GenericContext<
    MockSnapShot,
    SharedDirectConceptDelta<ConceptId>,
    SharedContextDelta<ConceptId>,
    ConceptId,
    ArcFamily,
>;

type Syntax = MTSyntaxTree<ConceptId>;
