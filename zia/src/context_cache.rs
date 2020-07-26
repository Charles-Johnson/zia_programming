use crate::{concepts::Concept, ast::SyntaxTree, context_search::ReductionResult};
use dashmap::DashMap;
use std::{mem::swap, sync::Arc};
use log::debug;

#[derive(Debug, Default, Clone)]
pub struct ContextCache {
    reductions_disabled: bool,
    reductions: DashMap<Arc<SyntaxTree>, ReductionResult>,
    syntax_trees_disabled: bool,
    syntax_trees: DashMap<usize, Arc<SyntaxTree>>,
}

impl ContextCache {
    pub fn get_syntax_tree_or_else(&self, concept_id: usize, build_syntax: impl Fn() -> Arc<SyntaxTree>) -> Arc<SyntaxTree> {
        self.syntax_trees.get(&concept_id).map_or_else(build_syntax, |r| r.value().clone())
    }
    pub fn insert_syntax_tree(&self, concept: &Concept, syntax_tree: &Arc<SyntaxTree>) {
        if !self.syntax_trees_disabled && !concept.variable()  {
            self.syntax_trees.insert(concept.id(), syntax_tree.clone());
        }
    }
    pub fn get_reduction_or_else(&self, ast: &Arc<SyntaxTree>, reduce: impl Fn() -> ReductionResult) -> ReductionResult {
        self.reductions.get(ast).map_or_else(reduce, |r| r.as_ref().cloned())
    }
    pub fn insert_reduction(&self, ast: &Arc<SyntaxTree>, reduction_result: &ReductionResult) {
        if !self.reductions_disabled && !ast.is_variable()
            && (&reduction_result).as_ref().map_or(true, |(r, _)| r != ast)
        {
            self.reductions.insert(ast.clone(), reduction_result.clone());
        }
    }
    pub fn invalidate(&mut self) {
        let mut empty_cache = Self::default();
        swap(self, &mut empty_cache);
        debug!("Cache invalidated");
    }
    pub fn disable_reduction_cache(&mut self) -> &mut Self {
        self.reductions_disabled = true;
        self
    }
    pub fn disable_syntax_tree_cache(&mut self) -> &mut Self {
        self.syntax_trees_disabled = true;
        self
    }
}