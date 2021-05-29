use crate::{
    ast::SyntaxTree, concepts::Concept, context_search::ReductionResult,
};
use dashmap::DashMap;
use log::debug;
use std::sync::Arc;

#[derive(Debug, Default, Clone)]
pub struct ReductionCacheList {
    head: Arc<ReductionCache>,
    tail: Option<Arc<ReductionCacheList>>,
}

impl<'a> From<Arc<ReductionCache>> for ReductionCacheList {
    fn from(head: Arc<ReductionCache>) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl ReductionCacheList {
    pub fn spawn(self: &Arc<Self>, cache: Arc<ReductionCache>) -> Arc<Self> {
        Arc::new(Self {
            head: cache,
            tail: Some(self.clone()),
        })
    }

    pub fn get_reduction_or_else(
        &self,
        ast: &Arc<SyntaxTree>,
        reduce: impl Fn() -> ReductionResult + Copy,
    ) -> ReductionResult {
        self.head.get(ast).map_or_else(
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
            self.head.insert(ast.clone(), reduction_result.clone());
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ContextCache {
    pub reductions: Arc<ReductionCacheList>,
    syntax_trees: Arc<DashMap<usize, Arc<SyntaxTree>>>,
    contains_bound_variable_syntax: Arc<DashMap<Arc<SyntaxTree>, bool>>,
}

pub type ReductionCache = DashMap<Arc<SyntaxTree>, ReductionResult>;

impl ContextCache {
    pub fn invalidate(&mut self) {
        std::mem::take(self);
        debug!("Cache invalidated");
    }

    pub fn spawn(&self, cache: &Arc<ReductionCache>) -> Self {
        Self {
            reductions: self.reductions.spawn(cache.clone()),
            ..Self::default()
        }
    }

    pub fn remember_if_contains_bound_variable_syntax_or_else(
        &self,
        syntax: &Arc<SyntaxTree>,
        f: impl Fn() -> bool,
    ) -> bool {
        self.contains_bound_variable_syntax.get(syntax).map_or_else(f, |v| *v)
    }

    pub fn get_syntax_tree_or_else(
        &self,
        concept_id: usize,
        build_syntax: impl Fn() -> Arc<SyntaxTree> + Copy,
    ) -> Arc<SyntaxTree> {
        self.syntax_trees
            .get(&concept_id)
            .map_or_else(build_syntax, |v| v.clone())
    }

    pub fn insert_syntax_tree(
        &self,
        concept: &Concept,
        syntax_tree: &Arc<SyntaxTree>,
    ) {
        if !concept.anonymous_variable() {
            self.syntax_trees.insert(concept.id(), syntax_tree.clone());
        }
    }
}
