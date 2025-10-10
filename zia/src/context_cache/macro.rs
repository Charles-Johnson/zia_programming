#![allow(clippy::single_component_path_imports)]

use crate::{
    ast::SyntaxKey,
    concepts::ConceptTrait,
    context_cache::{InferenceCache, ReductionCache},
    mixed_concept::ConceptId,
    nester::SharedReference,
    reduction_reason::{ReductionResult, SharedSyntax},
};
use dashmap::DashMap;
use log::debug;
#[derive(Debug, Clone)]
pub struct GenericCache<CI: ConceptId, SR: SharedReference> {
    pub reductions: SR::Share<ReductionCacheList<CI, SR>>,
    syntax_trees: SR::Share<DashMap<CI, SharedSyntax<CI, SR>>>,
    contains_bound_variable_syntax: SR::Share<DashMap<SyntaxKey<CI>, bool>>,
    inferences: SR::Share<InferenceCacheList<CI, SR>>,
}

impl<CI: ConceptId, SR: SharedReference> Default for GenericCache<CI, SR> {
    fn default() -> Self {
        Self {
            reductions: SR::share(ReductionCacheList::default()),
            syntax_trees: SR::share(DashMap::default()),
            contains_bound_variable_syntax: SR::share(DashMap::default()),
            inferences: SR::share(InferenceCacheList::default()),
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> GenericCache<CI, SR> {
    pub fn invalidate(&mut self) {
        std::mem::take(self);
        debug!("Cache invalidated");
    }

    pub fn spawn(&self, cache: &SR::Share<ReductionCache<CI, SR>>) -> Self {
        Self {
            reductions: ReductionCacheList::<CI, SR>::spawn(
                &self.reductions,
                cache.clone(),
            ),
            inferences: self.inferences.clone(),
            ..Self::default()
        }
    }

    pub fn remember_if_contains_bound_variable_syntax_or_else(
        &self,
        syntax: &SharedSyntax<CI, SR>,
        f: impl Fn() -> bool,
    ) -> bool {
        self.contains_bound_variable_syntax
            .get(&syntax.key())
            .map_or_else(f, |v| *v)
    }

    pub fn get_syntax_tree_or_else(
        &self,
        concept_id: CI,
        build_syntax: impl Fn() -> SharedSyntax<CI, SR> + Copy,
    ) -> SharedSyntax<CI, SR> {
        self.syntax_trees
            .get(&concept_id)
            .map_or_else(build_syntax, |v| v.clone())
    }

    pub fn insert_syntax_tree(
        &self,
        concept: &impl ConceptTrait<Id = CI>,
        syntax_tree: &SharedSyntax<CI, SR>,
    ) {
        if !concept.anonymous_variable() {
            self.syntax_trees.insert(concept.id(), syntax_tree.clone());
        }
    }

    pub fn get_reduction_or_else(
        &self,
        ast: &SharedSyntax<CI, SR>,
        reduce: impl Fn() -> ReductionResult<CI, SR> + Copy,
    ) -> ReductionResult<CI, SR> {
        self.reductions.get_reduction_or_else(ast, reduce)
    }

    pub fn insert_reduction(
        &self,
        ast: &SharedSyntax<CI, SR>,
        reduction_result: &ReductionResult<CI, SR>,
    ) {
        self.reductions.insert_reduction(ast, reduction_result);
    }

    pub fn get_inference_or_else(
        &self,
        concept: CI,
        infer: impl Fn() -> ReductionResult<CI, SR> + Copy,
    ) -> ReductionResult<CI, SR> {
        self.inferences.get_inference_or_else(concept, infer)
    }

    pub fn insert_inference(&self, concept: CI, rr: &ReductionResult<CI, SR>) {
        self.inferences.insert_inference(concept, rr);
    }
}

#[derive(Debug, Clone)]
pub struct InferenceCacheList<CI: ConceptId, SR: SharedReference> {
    head: SR::Share<InferenceCache<CI, SR>>,
    tail: Option<SR::Share<Self>>,
}

impl<CI: ConceptId, SR: SharedReference> Default
    for InferenceCacheList<CI, SR>
{
    fn default() -> Self {
        Self {
            head: SR::share(InferenceCache::<CI, SR>::default()),
            tail: None,
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> InferenceCacheList<CI, SR> {
    pub fn get_inference_or_else(
        &self,
        concept: CI,
        reduce: impl Fn() -> ReductionResult<CI, SR> + Copy,
    ) -> ReductionResult<CI, SR> {
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
        concept: CI,
        reduction_result: &ReductionResult<CI, SR>,
    ) {
        self.head.insert(concept, reduction_result.clone());
    }
}

#[derive(Debug, Clone)]
pub struct ReductionCacheList<CI: ConceptId, SR: SharedReference> {
    head: SR::Share<ReductionCache<CI, SR>>,
    tail: Option<SR::Share<Self>>,
}

impl<CI: ConceptId, SR: SharedReference> Default
    for ReductionCacheList<CI, SR>
{
    fn default() -> Self {
        Self {
            head: SR::share(ReductionCache::<CI, SR>::default()),
            tail: None,
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> ReductionCacheList<CI, SR> {
    #[must_use]
    pub fn spawn(
        shared_ref: &SR::Share<Self>,
        cache: SR::Share<ReductionCache<CI, SR>>,
    ) -> SR::Share<Self> {
        SR::share(Self {
            head: cache,
            tail: Some(shared_ref.clone()),
        })
    }

    pub const fn start_list(head: SR::Share<ReductionCache<CI, SR>>) -> Self {
        Self {
            head,
            tail: None,
        }
    }

    pub fn get_reduction_or_else(
        &self,
        ast: &SharedSyntax<CI, SR>,
        reduce: impl Fn() -> ReductionResult<CI, SR> + Copy,
    ) -> ReductionResult<CI, SR> {
        self.head.get(&ast.key()).map_or_else(
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
        ast: &SharedSyntax<CI, SR>,
        reduction_result: &ReductionResult<CI, SR>,
    ) {
        if !ast.is_variable()
            && reduction_result
                .as_ref()
                .map_or(true, |(r, _)| r.key() != ast.key())
        {
            self.head.insert(ast.key(), reduction_result.clone());
        }
    }
}
