#![allow(clippy::redundant_pub_crate)]

mod r#macro;
mod r#trait;

use crate::{
    ast::SyntaxTree,
    context_search::{ReductionReason, ReductionResult},
};
use dashmap::DashMap;
pub(crate) use r#macro::impl_cache;
pub use r#trait::ContextCache;

pub type ReductionCache<RR> = DashMap<
    <<RR as ReductionReason>::Syntax as SyntaxTree>::SharedSyntax,
    ReductionResult<RR>,
>;

pub type ConceptId<RR> =
    <<RR as ReductionReason>::Syntax as SyntaxTree>::ConceptId;
pub type SharedSyntax<RR> =
    <<RR as ReductionReason>::Syntax as SyntaxTree>::SharedSyntax;
