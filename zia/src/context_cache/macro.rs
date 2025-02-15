#![allow(clippy::single_component_path_imports)]

macro_rules! impl_cache {
    ($refcounter:tt, $cache:tt) => {
        use crate::{
            ast::SyntaxTree,
            concepts::ConceptTrait,
            context_cache::{ContextCache, ReductionCache, InferenceCache, ConceptId, SharedSyntax},
            reduction_reason::{ReductionReason, ReductionResult}
        };
        use dashmap::DashMap;
        use log::debug;
        #[derive(Debug, Clone)]
        pub struct $cache<RR: ReductionReason> {
            pub reductions: $refcounter<ReductionCacheList<RR>>,
            syntax_trees: $refcounter<DashMap<ConceptId<RR>, SharedSyntax<RR>>>,
            contains_bound_variable_syntax: $refcounter<DashMap<SharedSyntax<RR>, bool>>,
            inferences: $refcounter<InferenceCacheList<RR>>
        }

        impl<RR: ReductionReason> Default for $cache<RR> {
            fn default() -> Self {
                Self {
                    reductions: $refcounter::new(ReductionCacheList::default()),
                    syntax_trees: $refcounter::new(DashMap::default()),
                    contains_bound_variable_syntax: $refcounter::new(DashMap::default()),
                    inferences: $refcounter::new(InferenceCacheList::default())
                }
            }
        }

        impl<RR: ReductionReason> ContextCache for $cache<RR>
        {
            type RR = RR;
            type SharedReductionCache = $refcounter<ReductionCache<RR>>;
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
                    inferences: self.inferences.clone(),
                    ..Self::default()
                }
            }

            fn remember_if_contains_bound_variable_syntax_or_else(
                &self,
                syntax: &SharedSyntax<Self::RR>,
                f: impl Fn() -> bool,
            ) -> bool {
                self.contains_bound_variable_syntax.get(syntax).map_or_else(f, |v| *v)
            }

            fn get_syntax_tree_or_else(
                &self,
                concept_id: ConceptId<Self::RR>,
                build_syntax: impl Fn() -> SharedSyntax<Self::RR> + Copy,
            ) -> <<Self::RR as ReductionReason>::Syntax as SyntaxTree>::SharedSyntax {
                self.syntax_trees
                    .get(&concept_id)
                    .map_or_else(build_syntax, |v| v.clone())
            }

            fn insert_syntax_tree(
                &self,
                concept: &impl ConceptTrait<Id=ConceptId<Self::RR>>,
                syntax_tree: &SharedSyntax<Self::RR>,
            )
            {
                if !concept.anonymous_variable() {
                    self.syntax_trees.insert(concept.id(), syntax_tree.clone());
                }
            }

            fn get_reduction_or_else(
                &self,
                ast: &SharedSyntax<Self::RR>,
                reduce: impl Fn() -> ReductionResult<RR>
                    + Copy,
            ) -> ReductionResult<RR> {
                self.reductions.get_reduction_or_else(ast, reduce)
            }

            fn insert_reduction(&self,
                ast: &SharedSyntax<Self::RR>,
                reduction_result: &ReductionResult<RR>,) {
                self.reductions.insert_reduction(ast, reduction_result);
            }

            fn get_inference_or_else(&self, concept: ConceptId<RR>, infer: impl Fn() -> ReductionResult<RR> + Copy) -> ReductionResult<RR> {
                self.inferences.get_inference_or_else(concept, infer)
            }

            fn insert_inference(&self, concept: ConceptId<RR>, rr: &ReductionResult<RR>) {
                self.inferences.insert_inference(concept, rr)
            }
        }

        #[derive(Debug, Clone)]
        pub struct InferenceCacheList<RR: ReductionReason> {
            head: $refcounter<InferenceCache<RR>>,
            tail: Option<$refcounter<Self>>,
        }

        impl<RR: ReductionReason> Default for InferenceCacheList<RR>
        {
            fn default() -> Self {
                Self {
                    head: $refcounter::new(InferenceCache::<RR>::default()),
                    tail: None,
                }
            }
        }

        impl<'a, RR: ReductionReason>
            From<$refcounter<InferenceCache<RR>>>
            for InferenceCacheList<RR>
        {
            fn from(
                head: $refcounter<InferenceCache<RR>>,
            ) -> Self {
                Self {
                    head,
                    tail: None,
                }
            }
        }

        impl<RR: ReductionReason> InferenceCacheList<RR> {
            pub fn get_inference_or_else(
                &self,
                concept: ConceptId<RR>,
                reduce: impl Fn() -> ReductionResult<RR>
                    + Copy,
            ) -> ReductionResult<RR> {
                self.head.get(&concept).map_or_else(
                    || {
                        self.tail.as_ref().map_or_else(reduce, |ccl| {
                            ccl.get_inference_or_else(concept, reduce)
                        })
                    },
                    |r| r.as_ref().cloned(),
                )
            }

            pub fn insert_inference(
                &self,
                concept: ConceptId<RR>,
                reduction_result: &ReductionResult<RR>,
            ) {
                self.head.insert(concept, reduction_result.clone());
            }
        }

        #[derive(Debug, Clone)]
        pub struct ReductionCacheList<RR: ReductionReason> {
            head: $refcounter<ReductionCache<RR>>,
            tail: Option<$refcounter<Self>>,
        }

        impl<RR: ReductionReason> Default for ReductionCacheList<RR>
        {
            fn default() -> Self {
                Self {
                    head: $refcounter::new(ReductionCache::<RR>::default()),
                    tail: None,
                }
            }
        }

        impl<'a, RR: ReductionReason>
            From<$refcounter<ReductionCache<RR>>>
            for ReductionCacheList<RR>
        {
            fn from(
                head: $refcounter<ReductionCache<RR>>,
            ) -> Self {
                Self {
                    head,
                    tail: None,
                }
            }
        }

        impl<RR: ReductionReason> ReductionCacheList<RR> {
            #[must_use]
            pub fn spawn(
                self: &$refcounter<Self>,
                cache: $refcounter<ReductionCache<RR>>,
            ) -> $refcounter<Self> {
                $refcounter::new(Self {
                    head: cache,
                    tail: Some(self.clone()),
                })
            }

            pub fn get_reduction_or_else(
                &self,
                ast: &SharedSyntax<RR>,
                reduce: impl Fn() -> ReductionResult<RR>
                    + Copy,
            ) -> ReductionResult<RR> {
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
                ast: &SharedSyntax<RR>,
                reduction_result: &ReductionResult<RR>,
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
