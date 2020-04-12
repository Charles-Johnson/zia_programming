use crate::{ast::SyntaxTree, concepts::Concept, context_delta::ContextDelta};

pub trait Reader: Default {
    fn read_concept(&self, delta: &ContextDelta, concept_id: usize) -> Concept;
    fn has_variable(&self, delta: &ContextDelta, variable_id: usize) -> bool;
    fn concept_len(&self, delta: &ContextDelta) -> usize;
    fn get_label(
        &self,
        delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String>;
    fn ast_from_symbol(&self, delta: &ContextDelta, s: &str) -> SyntaxTree {
        self.concept_from_label(delta, s).map_or_else(
            || s.into(),
            |concept| SyntaxTree::from(s).bind_concept(concept),
        )
    }
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
    #[cfg(test)]
    fn new_test_case() -> Self {
        let test_case = Self::default();
        let delta = ContextDelta::default();
        for id in 0..test_case.concept_len(&delta) {
            test_case.get_label(&delta, id).map(|s| {
                assert_eq!(test_case.concept_from_label(&delta, &s), Some(id))
            });
        }
        test_case
    }
}
