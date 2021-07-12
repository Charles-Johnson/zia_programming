#![allow(clippy::single_component_path_imports)]

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
            type SharedReductionCache = $refcounter<ReductionCache<Self::Syntax>>;
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
                reduce: impl Fn() -> ReductionResult<Self::Syntax>
                    + Copy,
            ) -> ReductionResult<Self::Syntax> {
                self.reductions.get_reduction_or_else(ast, reduce)
            }

            fn insert_reduction(&self,
                ast: &<Self::Syntax as SyntaxTree>::SharedSyntax,
                reduction_result: &ReductionResult<Self::Syntax>,) {
                self.reductions.insert_reduction(ast, reduction_result);
            }
        }

        #[derive(Debug, Clone)]
        pub struct ReductionCacheList<Syntax: SyntaxTree> {
            head: $refcounter<ReductionCache<Syntax>>,
            tail: Option<$refcounter<Self>>,
        }

        impl<Syntax> Default for ReductionCacheList<Syntax>
        where
            Syntax: SyntaxTree,
        {
            fn default() -> Self {
                Self {
                    head: $refcounter::new(ReductionCache::<Syntax>::default()),
                    tail: None,
                }
            }
        }

        impl<'a, Syntax>
            From<$refcounter<ReductionCache<Syntax>>>
            for ReductionCacheList<Syntax>
        where
            Syntax: SyntaxTree,
        {
            fn from(
                head: $refcounter<ReductionCache<Syntax>>,
            ) -> Self {
                Self {
                    head,
                    tail: None,
                }
            }
        }

        impl<Syntax: SyntaxTree> ReductionCacheList<Syntax> {
            #[must_use]
            pub fn spawn(
                self: &$refcounter<Self>,
                cache: $refcounter<ReductionCache<Syntax>>,
            ) -> $refcounter<Self> {
                $refcounter::new(Self {
                    head: cache,
                    tail: Some(self.clone()),
                })
            }

            pub fn get_reduction_or_else(
                &self,
                ast: &Syntax::SharedSyntax,
                reduce: impl Fn() -> ReductionResult<Syntax>
                    + Copy,
            ) -> ReductionResult<Syntax> {
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
                reduction_result: &ReductionResult<Syntax>,
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

pub(crate) use impl_cache;
