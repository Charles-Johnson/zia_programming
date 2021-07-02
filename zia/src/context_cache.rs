use crate::{
    ast::SyntaxTree, concepts::Concept, context_search::ReductionResult,
};
use dashmap::DashMap;
use std::fmt::Debug;

macro_rules! impl_cache {
    ($refcounter:tt, $cache:tt, $syntax:ty) => {
        use crate::{
            ast::SyntaxTree,
            concepts::Concept,
            context_cache::{ContextCache, ReductionCache},
            context_search::ReductionResult,
        };
        use dashmap::DashMap;
        use log::debug;
        #[derive(Debug, Clone)]
        pub struct $cache {
            pub reductions: $refcounter<ReductionCacheList<$syntax>>,
            syntax_trees: $refcounter<DashMap<<$syntax as SyntaxTree>::ConceptId, <$syntax as SyntaxTree>::SharedSyntax>>,
            contains_bound_variable_syntax: $refcounter<DashMap<<$syntax as SyntaxTree>::SharedSyntax, bool>>,
        }

        impl Default for $cache {
            fn default() -> Self {
                Self {
                    reductions: $refcounter::new(ReductionCacheList::default()),
                    syntax_trees: $refcounter::new(DashMap::default()),
                    contains_bound_variable_syntax: $refcounter::new(DashMap::default()),
                }
            }
        }

        impl ContextCache for $cache
        {
            type Syntax = $syntax;
            type SharedReductionCache = $refcounter<ReductionCache<<Self::Syntax as SyntaxTree>::ConceptId, <Self::Syntax as SyntaxTree>::SharedSyntax>>;
            fn invalidate(&mut self) {
                std::mem::take(self);
                debug!("Cache invalidated");
            }

            fn spawn(
                &self,
                cache: &Self::SharedReductionCache,
            ) -> Self {
                Self {
                    reductions: self.reductions.spawn(cache.clone()),
                    ..Self::default()
                }
            }

            fn remember_if_contains_bound_variable_syntax_or_else(
                &self,
                syntax: &<Self::Syntax as SyntaxTree>::SharedSyntax,
                f: impl Fn() -> bool,
            ) -> bool {
                self.contains_bound_variable_syntax.get(syntax).map_or_else(f, |v| *v)
            }

            fn get_syntax_tree_or_else(
                &self,
                concept_id: <Self::Syntax as SyntaxTree>::ConceptId,
                build_syntax: impl Fn() -> <Self::Syntax as SyntaxTree>::SharedSyntax + Copy,
            ) -> <Self::Syntax as SyntaxTree>::SharedSyntax {
                self.syntax_trees
                    .get(&concept_id)
                    .map_or_else(build_syntax, |v| v.clone())
            }

            fn insert_syntax_tree(
                &self,
                concept: &Concept<<Self::Syntax as SyntaxTree>::ConceptId>,
                syntax_tree: &<Self::Syntax as SyntaxTree>::SharedSyntax,
            ) {
                if !concept.anonymous_variable() {
                    self.syntax_trees.insert(concept.id(), syntax_tree.clone());
                }
            }

            fn get_reduction_or_else(
                &self,
                ast: &<Self::Syntax as SyntaxTree>::SharedSyntax,
                reduce: impl Fn() -> ReductionResult<<Self::Syntax as SyntaxTree>::ConceptId, <Self::Syntax as SyntaxTree>::SharedSyntax>
                    + Copy,
            ) -> ReductionResult<<Self::Syntax as SyntaxTree>::ConceptId, <Self::Syntax as SyntaxTree>::SharedSyntax> {
                self.reductions.get_reduction_or_else(ast, reduce)
            }

            fn insert_reduction(&self,
                ast: &<Self::Syntax as SyntaxTree>::SharedSyntax,
                reduction_result: &ReductionResult<
                    <Self::Syntax as SyntaxTree>::ConceptId,
                    <Self::Syntax as SyntaxTree>::SharedSyntax,
                >,) {
                self.reductions.insert_reduction(ast, reduction_result);
            }
        }

        #[derive(Debug, Clone)]
        pub struct ReductionCacheList<Syntax: SyntaxTree> {
            head: $refcounter<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>,
            tail: Option<$refcounter<Self>>,
        }

        impl<Syntax> Default for ReductionCacheList<Syntax>
        where
            Syntax: SyntaxTree,
        {
            fn default() -> Self {
                Self {
                    head: $refcounter::new(ReductionCache::default()),
                    tail: None,
                }
            }
        }

        impl<'a, Syntax>
            From<$refcounter<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>>
            for ReductionCacheList<Syntax>
        where
            Syntax: SyntaxTree,
        {
            fn from(
                head: $refcounter<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>,
            ) -> Self {
                Self {
                    head,
                    tail: None,
                }
            }
        }

        impl<Syntax: SyntaxTree> ReductionCacheList<Syntax> {
            pub fn spawn(
                self: &$refcounter<Self>,
                cache: $refcounter<ReductionCache<Syntax::ConceptId, Syntax::SharedSyntax>>,
            ) -> $refcounter<Self> {
                $refcounter::new(Self {
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
    }
}

pub type ReductionCache<ConceptId, SharedSyntax> =
    DashMap<SharedSyntax, ReductionResult<ConceptId, SharedSyntax>>;

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
        reduce: impl Fn() -> ReductionResult<
                <Self::Syntax as SyntaxTree>::ConceptId,
                <Self::Syntax as SyntaxTree>::SharedSyntax,
            > + Copy,
    ) -> ReductionResult<
        <Self::Syntax as SyntaxTree>::ConceptId,
        <Self::Syntax as SyntaxTree>::SharedSyntax,
    >;

    fn insert_reduction(
        &self,
        ast: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        reduction_result: &ReductionResult<
            <Self::Syntax as SyntaxTree>::ConceptId,
            <Self::Syntax as SyntaxTree>::SharedSyntax,
        >,
    );
}
