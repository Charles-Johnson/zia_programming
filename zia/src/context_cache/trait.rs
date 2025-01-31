use crate::{
    ast::GenericSyntaxTree, concepts::ConceptTrait, mixed_concept::ConceptId,
    nester::SharedReference, reduction_reason::ReductionResult,
};
use std::fmt::Debug;

use super::SharedSyntax;

pub trait ContextCache<CI: ConceptId, SR: SharedReference>
where
    Self: Clone + Debug + Default,
{
    type SharedReductionCache: Default;
    fn invalidate(&mut self);

    fn spawn(&self, cache: &Self::SharedReductionCache) -> Self;

    fn remember_if_contains_bound_variable_syntax_or_else(
        &self,
        syntax: &SharedSyntax<CI, SR>,
        f: impl Fn() -> bool,
    ) -> bool;

    fn get_syntax_tree_or_else(
        &self,
        concept_id: CI,
        build_syntax: impl Fn() -> SharedSyntax<CI, SR> + Copy,
    ) -> SR::Share<GenericSyntaxTree<CI, SR>>;

    fn insert_syntax_tree(
        &self,
        concept: &impl ConceptTrait<Id = CI>,
        syntax_tree: &SharedSyntax<CI, SR>,
    );

    fn get_reduction_or_else(
        &self,
        ast: &SharedSyntax<CI, SR>,
        reduce: impl Fn() -> ReductionResult<CI, SR> + Copy,
    ) -> ReductionResult<CI, SR>;

    fn insert_reduction(
        &self,
        ast: &SharedSyntax<CI, SR>,
        reduction_result: &ReductionResult<CI, SR>,
    );

    fn get_inference_or_else(
        &self,
        concept: CI,
        infer: impl Fn() -> ReductionResult<CI, SR> + Copy,
    ) -> ReductionResult<CI, SR>;

    fn insert_inference(&self, concept: CI, rr: &ReductionResult<CI, SR>);
}
