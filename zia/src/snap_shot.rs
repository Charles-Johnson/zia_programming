use crate::{ast::SyntaxTree, concepts::Concept, context_delta::ContextDelta};

pub trait Reader {
    fn read_concept(&self, delta: &ContextDelta, concept_id: usize) -> Concept;
    fn has_variable(&self, delta: &ContextDelta, variable_id: usize) -> bool;
    fn concept_len(&self, delta: &ContextDelta) -> usize;
    fn get_label(
        &self,
        delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String>;
    fn ast_from_symbol(&self, delta: &ContextDelta, symbol: &str)
        -> SyntaxTree;
    fn true_id() -> usize;
    fn implication_id() -> usize;
    fn precedence_id() -> usize;
    fn greater_than_id() -> usize;
    fn default_id() -> usize;
    fn reduction_id() -> usize;
    fn false_id() -> usize;
    fn assoc_id() -> usize;
    fn right_id() -> usize;
    fn left_id() -> usize;
    fn exists_such_that_id() -> usize;
    fn concept_from_label(
        &self,
        delta: &ContextDelta,
        s: &str,
    ) -> Option<usize>;
}
