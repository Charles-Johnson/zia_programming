use crate::{
    ast::SyntaxTree,
    concepts::ConceptTrait,
    reduction_reason::{ReductionReason, ReductionResult},
};
use std::fmt::Debug;

use super::{ConceptId, SharedSyntax};

pub trait ContextCache
where
    Self: Clone + Debug + Default,
{
    type SharedReductionCache: Default;
    type RR: ReductionReason;
    fn invalidate(&mut self);

    fn spawn(&self, cache: &Self::SharedReductionCache) -> Self;

    fn remember_if_contains_bound_variable_syntax_or_else(
        &self,
        syntax: &SharedSyntax<Self::RR>,
        f: impl Fn() -> bool,
    ) -> bool;

    fn get_syntax_tree_or_else(
        &self,
        concept_id: ConceptId<Self::RR>,
        build_syntax: impl Fn() -> SharedSyntax<Self::RR> + Copy,
    ) -> <<Self::RR as ReductionReason>::Syntax as SyntaxTree>::SharedSyntax;

    fn insert_syntax_tree(
        &self,
        concept: &impl ConceptTrait<Id = ConceptId<Self::RR>>,
        syntax_tree: &SharedSyntax<Self::RR>,
    );

    fn get_reduction_or_else(
        &self,
        ast: &SharedSyntax<Self::RR>,
        reduce: impl Fn() -> ReductionResult<Self::RR> + Copy,
    ) -> ReductionResult<Self::RR>;

    fn insert_reduction(
        &self,
        ast: &SharedSyntax<Self::RR>,
        reduction_result: &ReductionResult<Self::RR>,
    );

    fn get_inference_or_else(
        &self,
        concept: ConceptId<Self::RR>,
        infer: impl Fn() -> ReductionResult<Self::RR> + Copy,
    ) -> ReductionResult<Self::RR>;

    fn insert_inference(
        &self,
        concept: ConceptId<Self::RR>,
        rr: &ReductionResult<Self::RR>,
    );
}
