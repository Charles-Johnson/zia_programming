use crate::{
    ast::SyntaxTree, concepts::Concept, context_search::ReductionResult,
};
use dashmap::DashMap;
use log::debug;
use std::{fmt::Debug, sync::Arc};

#[derive(Debug, Clone)]
pub struct ReductionCacheList<Syntax: SyntaxTree> {
    head: Arc<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>,
    tail: Option<Arc<ReductionCacheList<Syntax>>>,
}

impl<Syntax> Default for ReductionCacheList<Syntax>
where
    Syntax: SyntaxTree,
{
    fn default() -> Self {
        Self {
            head: Arc::new(ReductionCache::default()),
            tail: None,
        }
    }
}

impl<'a, Syntax>
    From<Arc<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>>
    for ReductionCacheList<Syntax>
where
    Syntax: SyntaxTree,
{
    fn from(
        head: Arc<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>,
    ) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl<Syntax: SyntaxTree> ReductionCacheList<Syntax> {
    pub fn spawn(
        self: &Arc<Self>,
        cache: Arc<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>,
    ) -> Arc<Self> {
        Arc::new(Self {
            head: cache,
            tail: Some(self.clone()),
        })
    }

    pub fn get_reduction_or_else(
        &self,
        ast: &Syntax::SharedSyntax,
        reduce: impl Fn() -> ReductionResult<Syntax::ConceptId, Syntax::SharedSyntax>
            + Copy,
    ) -> ReductionResult<Syntax::ConceptId, Syntax::SharedSyntax> {
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
        ast: &Syntax::SharedSyntax,
        reduction_result: &ReductionResult<
            Syntax::ConceptId,
            Syntax::SharedSyntax,
        >,
    ) {
        if !ast.is_variable()
            && reduction_result.as_ref().map_or(true, |(r, _)| r != ast)
        {
            self.head.insert(ast.clone(), reduction_result.clone());
        }
    }
}

#[derive(Debug, Clone)]
pub struct ContextCache<Syntax: SyntaxTree> {
    pub reductions: Arc<ReductionCacheList<Syntax>>,
    syntax_trees: Arc<DashMap<Syntax::ConceptId, Syntax::SharedSyntax>>,
    contains_bound_variable_syntax: Arc<DashMap<Syntax::SharedSyntax, bool>>,
}

impl<Syntax: SyntaxTree> Default for ContextCache<Syntax> {
    fn default() -> Self {
        Self {
            reductions: Arc::new(ReductionCacheList::default()),
            syntax_trees: Arc::new(DashMap::default()),
            contains_bound_variable_syntax: Arc::new(DashMap::default()),
        }
    }
}

pub type ReductionCache<ConceptId, SharedSyntax> =
    DashMap<SharedSyntax, ReductionResult<ConceptId, SharedSyntax>>;

impl<Syntax> ContextCache<Syntax>
where
    Syntax: SyntaxTree,
{
    pub fn invalidate(&mut self) {
        std::mem::take(self);
        debug!("Cache invalidated");
    }

    pub fn spawn(
        &self,
        cache: &Arc<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>,
    ) -> Self {
        Self {
            reductions: self.reductions.spawn(cache.clone()),
            ..Self::default()
        }
    }

    pub fn remember_if_contains_bound_variable_syntax_or_else(
        &self,
        syntax: &Syntax::SharedSyntax,
        f: impl Fn() -> bool,
    ) -> bool {
        self.contains_bound_variable_syntax.get(syntax).map_or_else(f, |v| *v)
    }

    pub fn get_syntax_tree_or_else(
        &self,
        concept_id: Syntax::ConceptId,
        build_syntax: impl Fn() -> Syntax::SharedSyntax + Copy,
    ) -> Syntax::SharedSyntax {
        self.syntax_trees
            .get(&concept_id)
            .map_or_else(build_syntax, |v| v.clone())
    }

    pub fn insert_syntax_tree(
        &self,
        concept: &Concept<Syntax::ConceptId>,
        syntax_tree: &Syntax::SharedSyntax,
    ) {
        if !concept.anonymous_variable() {
            self.syntax_trees.insert(concept.id(), syntax_tree.clone());
        }
    }
}
