#![allow(clippy::redundant_pub_crate)]

mod r#macro;
pub(crate) mod r#trait;

use crate::{
    ast::{GenericSyntaxTree, SyntaxKey},
    nester::SharedReference,
    reduction_reason::ReductionResult,
};
use dashmap::DashMap;
pub(crate) use r#macro::GenericCache;
pub use r#trait::ContextCache;

pub type ReductionCache<CI, SR> =
    DashMap<SyntaxKey<CI>, ReductionResult<CI, SR>>;

pub type InferenceCache<CI, SR> = DashMap<CI, ReductionResult<CI, SR>>;

pub type SharedSyntax<CI, SR> =
    <SR as SharedReference>::Share<GenericSyntaxTree<CI, SR>>;
