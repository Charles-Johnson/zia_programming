use crate::{
    ast::SyntaxTree, concepts::Concept, context_search::ReductionResult,
};
use dashmap::DashMap;
use log::debug;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct ReductionCacheList<ConceptId: Eq + Hash> {
    head: Arc<ReductionCache<ConceptId>>,
    tail: Option<Arc<ReductionCacheList<ConceptId>>>,
}

impl<ConceptId: Eq + Hash> Default for ReductionCacheList<ConceptId> {
    fn default() -> Self {
        Self {
            head: Arc::new(ReductionCache::default()),
            tail: None,
        }
    }
}

impl<'a, ConceptId: Clone + Eq + Hash> From<Arc<ReductionCache<ConceptId>>>
    for ReductionCacheList<ConceptId>
{
    fn from(head: Arc<ReductionCache<ConceptId>>) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl<ConceptId: Copy + Debug + Eq + Hash> ReductionCacheList<ConceptId> {
    pub fn spawn(
        self: &Arc<Self>,
        cache: Arc<ReductionCache<ConceptId>>,
    ) -> Arc<Self> {
        Arc::new(Self {
            head: cache,
            tail: Some(self.clone()),
        })
    }

    pub fn get_reduction_or_else(
        &self,
        ast: &Arc<SyntaxTree<ConceptId>>,
        reduce: impl Fn() -> ReductionResult<ConceptId> + Copy,
    ) -> ReductionResult<ConceptId> {
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
        ast: &Arc<SyntaxTree<ConceptId>>,
        reduction_result: &ReductionResult<ConceptId>,
    ) {
        if !ast.is_variable()
            && reduction_result.as_ref().map_or(true, |(r, _)| r != ast)
        {
            self.head.insert(ast.clone(), reduction_result.clone());
        }
    }
}

#[derive(Debug, Clone)]
pub struct ContextCache<ConceptId: Eq + Hash> {
    pub reductions: Arc<ReductionCacheList<ConceptId>>,
    syntax_trees: Arc<DashMap<ConceptId, Arc<SyntaxTree<ConceptId>>>>,
    contains_bound_variable_syntax:
        Arc<DashMap<Arc<SyntaxTree<ConceptId>>, bool>>,
}

impl<ConceptId: Eq + Hash> Default for ContextCache<ConceptId> {
    fn default() -> Self {
        Self {
            reductions: Arc::new(ReductionCacheList::default()),
            syntax_trees: Arc::new(DashMap::default()),
            contains_bound_variable_syntax: Arc::new(DashMap::default()),
        }
    }
}

pub type ReductionCache<ConceptId> =
    DashMap<Arc<SyntaxTree<ConceptId>>, ReductionResult<ConceptId>>;

impl<ConceptId: Copy + Debug + Display + Eq + Hash> ContextCache<ConceptId> {
    pub fn invalidate(&mut self) {
        std::mem::take(self);
        debug!("Cache invalidated");
    }

    pub fn spawn(&self, cache: &Arc<ReductionCache<ConceptId>>) -> Self {
        Self {
            reductions: self.reductions.spawn(cache.clone()),
            ..Self::default()
        }
    }

    pub fn remember_if_contains_bound_variable_syntax_or_else(
        &self,
        syntax: &Arc<SyntaxTree<ConceptId>>,
        f: impl Fn() -> bool,
    ) -> bool {
        self.contains_bound_variable_syntax.get(syntax).map_or_else(f, |v| *v)
    }

    pub fn get_syntax_tree_or_else(
        &self,
        concept_id: ConceptId,
        build_syntax: impl Fn() -> Arc<SyntaxTree<ConceptId>> + Copy,
    ) -> Arc<SyntaxTree<ConceptId>> {
        self.syntax_trees
            .get(&concept_id)
            .map_or_else(build_syntax, |v| v.clone())
    }

    pub fn insert_syntax_tree(
        &self,
        concept: &Concept<ConceptId>,
        syntax_tree: &Arc<SyntaxTree<ConceptId>>,
    ) where
        ConceptId: Eq + Hash,
    {
        if !concept.anonymous_variable() {
            self.syntax_trees.insert(concept.id(), syntax_tree.clone());
        }
    }
}
