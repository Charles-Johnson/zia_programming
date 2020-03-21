use crate::{ast::SyntaxTree, concepts::Concept, context_delta::ContextDelta};

pub trait Reader {
    fn read_concept(&self, delta: &ContextDelta, concept_id: usize) -> Concept;
    fn find_definition(
        &self,
        delta: &ContextDelta,
        left_id: usize,
        right_id: usize,
    ) -> Option<usize>;
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
}
