use crate::{
    ast::SyntaxTree, concepts::Concept, context_search::ReductionResult,
};
use dashmap::DashMap;
use log::debug;
use std::{mem::swap, sync::Arc};

#[derive(Debug)]
pub struct ContextCacheList<'a> {
    head: &'a ContextCache,
    tail: Option<Arc<ContextCacheList<'a>>>,
}

impl<'a> From<&'a ContextCache> for ContextCacheList<'a> {
    fn from(head: &'a ContextCache) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl<'a> ContextCacheList<'a> {
    pub fn spawn(list: &Arc<Self>, cache: &'a ContextCache) -> Self {
        Self {
            head: cache,
            tail: Some(list.clone()),
        }
    }

    pub fn get_syntax_tree_or_else(
        &self,
        concept_id: usize,
        build_syntax: impl Fn() -> Arc<SyntaxTree> + Copy,
    ) -> Arc<SyntaxTree> {
        self.head.syntax_trees.get(&concept_id).map_or_else(
            || {
                self.tail.as_ref().map_or_else(build_syntax, |ccl| {
                    ccl.get_syntax_tree_or_else(concept_id, build_syntax)
                })
            },
            |r| r.value().clone(),
        )
    }

    pub fn insert_syntax_tree(
        &self,
        concept: &Concept,
        syntax_tree: &Arc<SyntaxTree>,
    ) {
        if !concept.variable() {
            self.head.syntax_trees.insert(concept.id(), syntax_tree.clone());
        }
    }

    pub fn get_reduction_or_else(
        &self,
        ast: &Arc<SyntaxTree>,
        reduce: impl Fn() -> ReductionResult + Copy,
    ) -> ReductionResult {
        self.head.reductions.get(ast).map_or_else(
            || {
                self.tail.as_ref().map_or_else(reduce, |ccl| {
                    ccl.get_reduction_or_else(ast, reduce)
                })
            },
            |r| r.as_ref().cloned(),
        )
    }

    pub fn insert_reduction(
        &self,
        ast: &Arc<SyntaxTree>,
        reduction_result: &ReductionResult,
    ) {
        if !ast.is_variable()
            && reduction_result.as_ref().map_or(true, |(r, _)| r != ast)
        {
            self.head.reductions.insert(ast.clone(), reduction_result.clone());
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ContextCache {
    reductions: DashMap<Arc<SyntaxTree>, ReductionResult>,
    syntax_trees: DashMap<usize, Arc<SyntaxTree>>,
}

impl ContextCache {
    pub fn invalidate(&mut self) {
        let mut empty_cache = Self::default();
        swap(self, &mut empty_cache);
        debug!("Cache invalidated");
    }
}
