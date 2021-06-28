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
pub struct ReductionCacheList<
    ConceptId: Eq + Hash,
    Syntax: SyntaxTree<ConceptId>,
> {
    head: Arc<ReductionCache<ConceptId, Syntax::SharedSyntax>>,
    tail: Option<Arc<ReductionCacheList<ConceptId, Syntax>>>,
}

impl<ConceptId: Eq + Hash, Syntax> Default
    for ReductionCacheList<ConceptId, Syntax>
where
    Syntax: SyntaxTree<ConceptId>,
{
    fn default() -> Self {
        Self {
            head: Arc::new(ReductionCache::default()),
            tail: None,
        }
    }
}

impl<'a, ConceptId: Clone + Eq + Hash, Syntax>
    From<Arc<ReductionCache<ConceptId, Syntax::SharedSyntax>>>
    for ReductionCacheList<ConceptId, Syntax>
where
    Syntax: SyntaxTree<ConceptId>,
{
    fn from(
        head: Arc<ReductionCache<ConceptId, Syntax::SharedSyntax>>,
    ) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl<ConceptId: Copy + Debug + Eq + Hash, Syntax: SyntaxTree<ConceptId>>
    ReductionCacheList<ConceptId, Syntax>
{
    pub fn spawn(
        self: &Arc<Self>,
        cache: Arc<ReductionCache<ConceptId, Syntax::SharedSyntax>>,
    ) -> Arc<Self> {
        Arc::new(Self {
            head: cache,
            tail: Some(self.clone()),
        })
    }

    pub fn get_reduction_or_else(
        &self,
        ast: &Syntax::SharedSyntax,
        reduce: impl Fn() -> ReductionResult<ConceptId, Syntax::SharedSyntax> + Copy,
    ) -> ReductionResult<ConceptId, Syntax::SharedSyntax> {
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
        reduction_result: &ReductionResult<ConceptId, Syntax::SharedSyntax>,
    ) {
        if !ast.is_variable()
            && reduction_result.as_ref().map_or(true, |(r, _)| r != ast)
        {
            self.head.insert(ast.clone(), reduction_result.clone());
        }
    }
}

#[derive(Debug, Clone)]
pub struct ContextCache<ConceptId: Eq + Hash, Syntax: SyntaxTree<ConceptId>> {
    pub reductions: Arc<ReductionCacheList<ConceptId, Syntax>>,
    syntax_trees: Arc<DashMap<ConceptId, Syntax::SharedSyntax>>,
    contains_bound_variable_syntax: Arc<DashMap<Syntax::SharedSyntax, bool>>,
}

impl<ConceptId: Eq + Hash, Syntax: SyntaxTree<ConceptId>> Default
    for ContextCache<ConceptId, Syntax>
{
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

impl<ConceptId: Copy + Debug + Display + Eq + Hash, Syntax>
    ContextCache<ConceptId, Syntax>
where
    Syntax: SyntaxTree<ConceptId>,
{
    pub fn invalidate(&mut self) {
        std::mem::take(self);
        debug!("Cache invalidated");
    }

    pub fn spawn(
        &self,
        cache: &Arc<ReductionCache<ConceptId, Syntax::SharedSyntax>>,
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
        concept_id: ConceptId,
        build_syntax: impl Fn() -> Syntax::SharedSyntax + Copy,
    ) -> Syntax::SharedSyntax {
        self.syntax_trees
            .get(&concept_id)
            .map_or_else(build_syntax, |v| v.clone())
    }

    pub fn insert_syntax_tree(
        &self,
        concept: &Concept<ConceptId>,
        syntax_tree: &Syntax::SharedSyntax,
    ) where
        ConceptId: Eq + Hash,
    {
        if !concept.anonymous_variable() {
            self.syntax_trees.insert(concept.id(), syntax_tree.clone());
        }
    }
}
