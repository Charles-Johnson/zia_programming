#![allow(clippy::redundant_pub_crate)]

mod r#macro;
mod r#trait;

use crate::{ast::SyntaxTree, context_search::ReductionResult};
use dashmap::DashMap;
pub(crate) use r#macro::impl_cache;
pub use r#trait::ContextCache;

pub type ReductionCache<Syntax> =
    DashMap<<Syntax as SyntaxTree>::SharedSyntax, ReductionResult<Syntax>>;
