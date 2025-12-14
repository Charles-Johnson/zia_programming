mod basic_precedence;
mod infered_precedence;

use crate::{
    context::Context as GenericContext,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::{ArcFamily, MTSyntaxTree},
};

pub type Context = GenericContext<MockSnapShot, ConceptId, ArcFamily>;

type Syntax = MTSyntaxTree<ConceptId>;
