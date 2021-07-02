use crate::{
    ast::SyntaxTree, concepts::Concept, context_search::ReductionResult,
};
use std::fmt::Debug;

pub trait ContextCache
where
    Self: Clone + Debug + Default,
{
    type SharedReductionCache: Default;
    type Syntax: SyntaxTree;
    fn invalidate(&mut self);

    fn spawn(&self, cache: &Self::SharedReductionCache) -> Self;

    fn remember_if_contains_bound_variable_syntax_or_else(
        &self,
        syntax: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        f: impl Fn() -> bool,
    ) -> bool;

    fn get_syntax_tree_or_else(
        &self,
        concept_id: <Self::Syntax as SyntaxTree>::ConceptId,
        build_syntax: impl Fn() -> <Self::Syntax as SyntaxTree>::SharedSyntax + Copy,
    ) -> <Self::Syntax as SyntaxTree>::SharedSyntax;

    fn insert_syntax_tree(
        &self,
        concept: &Concept<<Self::Syntax as SyntaxTree>::ConceptId>,
        syntax_tree: &<Self::Syntax as SyntaxTree>::SharedSyntax,
    );

    fn get_reduction_or_else(
        &self,
        ast: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        reduce: impl Fn() -> ReductionResult<Self::Syntax> + Copy,
    ) -> ReductionResult<Self::Syntax>;

    fn insert_reduction(
        &self,
        ast: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        reduction_result: &ReductionResult<Self::Syntax>,
    );
}
