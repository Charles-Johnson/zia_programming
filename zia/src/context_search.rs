//  Library for the Zia programming language.
// Copyright (C) 2019 Charles Johnson
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.

use crate::{
    and_also::AndAlso,
    associativity::Associativity,
    ast::SyntaxTree,
    concepts::{format_string, ConceptTrait, ConcreteConceptType, Hand},
    context_cache::ContextCache,
    context_delta::{DirectConceptDelta, NewConceptDelta, NestedContextDelta},
    iteration::Iteration,
    reduction_reason::{
        RRSharedSyntax, ReductionReason, ReductionResult, SharedSyntax, Syntax,
    },
    snap_shot::Reader as SnapShotReader,
    substitute::{substitute, Substitutions},
    variable_mask_list::{VariableMask, VariableMaskList},
};
use log::debug;
use maplit::{hashmap, hashset};
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
};

#[derive(Debug)]
pub struct ContextSearch<
    's,
    'v,
    S,
    C,
    VML,
    SDCD,
    D,
    CCI: Clone + Display + Copy + Debug + Eq + Hash,
> where
    S: SnapShotReader<SDCD, ConceptId = CCI> + Sync + std::fmt::Debug,
    Self: Iteration<ConceptId = S::ConceptId, Syntax = Syntax<C>>,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>> + Debug,
    VML: VariableMaskList<Syntax = Syntax<C>>,
    D: Clone + AsRef<NestedContextDelta<S::ConceptId, SDCD, D>> + Debug + From<NestedContextDelta<S::ConceptId, SDCD, D>>
{
    snap_shot: &'s S,
    variable_mask: VML::Shared,
    delta: D,
    caches: C,
    syntax_evaluating: HashSet<SharedSyntax<C>>,
    bound_variable_syntax: &'v HashSet<SharedSyntax<C>>,
    phantom: PhantomData<SDCD>,
    phantom2: PhantomData<CCI>,
}

impl<
        's,
        'v,
        S,
        C,
        SDCD,
        VML,
        D,
        CCI: Clone + Display + Copy + Eq + Hash + From<usize> + Debug,
    > ContextSearch<'s, 'v, S, C, VML, SDCD, D, CCI>
where
    S: SnapShotReader<SDCD, ConceptId = CCI> + Sync + std::fmt::Debug,
    Self: Iteration<ConceptId = S::ConceptId, Syntax = Syntax<C>>,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>> + Debug,
    VML: VariableMaskList<Syntax = Syntax<C>>,
    D: Clone + AsRef<NestedContextDelta<S::ConceptId, SDCD, D>> + Debug + From<NestedContextDelta<S::ConceptId, SDCD, D>>
{
    fn infer_reduction(
        &self,
        concept: &S::MixedConcept<'_>,
    ) -> ReductionResult<C::RR> {
        debug!("infer_reduction({:#?})", concept);
        let implication_id =
            self.concrete_concept_id(ConcreteConceptType::Implication)?;
        let composition_id = concept
            .find_as_hand_in_composition_with(implication_id, Hand::Right)?;
        let composition_concept =
            self.snap_shot.read_concept(self.delta.as_ref(), composition_id);
        // TODO: check if implication is actually true
        // TODO: check for any concepts that are the reduction of `concept`
        let result = composition_concept.iter_hand_of(Hand::Right).find_map(
            |(condition_id, implication_rule_id)| {
                let condition = self.to_ast(&condition_id);
                let (reduced_condition, reason) = self.reduce(&condition)?;
                let x = reduced_condition.get_concept()?;
                self.is_concrete_type(ConcreteConceptType::True, &x).map(|x| {
                    (
                        // TODO: this should be the "true" concept only if the implication result is equivalent to the `concept`
                        self.to_ast(&x),
                        C::RR::inference(
                            self.to_ast(&implication_rule_id),
                            reason,
                        ),
                    )
                })
            },
        );
        result.or_else(|| {
            let reduction_operator = self.concrete_ast(ConcreteConceptType::Reduction)?;
            let mut spawned_delta = NestedContextDelta::spawn(self.delta.clone());
            let variable_reduction_id = spawned_delta.insert_delta_for_new_concept(NewConceptDelta::FreeVariable);
            let variable_reduction_syntax = Syntax::<C>::new_leaf_variable(variable_reduction_id).share();
            let variable_condition_id = spawned_delta.insert_delta_for_new_concept(NewConceptDelta::FreeVariable);
            let variable_condition_syntax = Syntax::<C>::new_leaf_variable(variable_condition_id).share();
            let cache = <C as ContextCache>::SharedReductionCache::default();
            let spawned_context_search = self.spawn(&cache, spawned_delta.into());
            let implication_rule_fn = |condition: &<<<C as ContextCache>::RR as ReductionReason>::Syntax as SyntaxTree>::SharedSyntax, reduction: &<<<C as ContextCache>::RR as ReductionReason>::Syntax as SyntaxTree>::SharedSyntax| {
                Syntax::<C>::new_pair(
                    condition.clone(),
                    Syntax::<C>::new_pair(
                        spawned_context_search.to_ast(&implication_id),
                        Syntax::<C>::new_pair(
                            spawned_context_search.to_ast(&concept.id()),
                            Syntax::<C>::new_pair(
                                reduction_operator.clone(),
                                reduction.clone(),
                            ).share()
                        ).share(),
                    ).share()
                )
            };
            let implication_rule_pattern = implication_rule_fn(&variable_condition_syntax, &variable_reduction_syntax);
            let true_id = spawned_context_search
            .concrete_concept_id(ConcreteConceptType::True)
            .expect("true concept must exist");
            let true_concept =
                spawned_context_search.snap_shot.read_concept(spawned_context_search.delta.as_ref(), true_id);
            let truths = true_concept.find_what_reduces_to_it();

            let irp = implication_rule_pattern.share();
            let t = truths.collect();
            let x = spawned_context_search.find_examples(&irp, &t).into_iter().find_map(|substitutions| {
                substitutions.get(&variable_condition_syntax).and_then(|condition_syntax| {
                    let (condition_normal_form, reason) = spawned_context_search.recursively_reduce(condition_syntax);
                    spawned_context_search.is_concrete_type(ConcreteConceptType::True, &condition_normal_form.get_concept()?)?;
                    substitutions.get(&variable_reduction_syntax).map(|result| (result.clone(), C::RR::inference(
                        implication_rule_fn(condition_syntax, result).share(),
                        reason.unwrap_or_else(ReductionReason::explicit),
                    )))
                })
            });
            drop(spawned_context_search);
            x
        })
    }

    fn is_concrete_type(
        &self,
        cct: ConcreteConceptType,
        concept_id: &S::ConceptId,
    ) -> Option<S::ConceptId> {
        if Some(cct) == self.concrete_type(concept_id) {
            Some(*concept_id)
        } else {
            None
        }
    }

    fn concrete_type(
        &self,
        concept_id: &S::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.snap_shot.concrete_concept_type(self.delta.as_ref(), *concept_id)
    }

    fn concrete_type_of_ast(
        &self,
        ast: &SharedSyntax<C>,
    ) -> Option<ConcreteConceptType> {
        ast.get_concept().and_then(|c| self.concrete_type(&c))
    }

    fn concrete_concept_id(
        &self,
        cct: ConcreteConceptType,
    ) -> Option<S::ConceptId> {
        self.snap_shot.concrete_concept_id(self.delta.as_ref(), cct)
    }

    pub fn concrete_ast(
        &self,
        cct: ConcreteConceptType,
    ) -> Option<SharedSyntax<C>> {
        self.concrete_concept_id(cct).map(|id| self.to_ast(&id))
    }

    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, id: &S::ConceptId) -> ReductionResult<C::RR> {
        debug!("reduce_concept({})", id);
        let concept = self.snap_shot.read_concept(self.delta.as_ref(), *id);
        self.infer_reduction(&concept).or_else(|| {
            let n = concept.get_reduction()?;
            if self.is_leaf_variable(&n) {
                self.variable_mask.get(n).cloned()
            } else {
                Some(self.to_ast(&n))
            }
            .map(|r| (r, C::RR::explicit()))
        })
    }

    // If (operator right)  cannot by trying be reduced by other means, then it should reduce to default_concept
    fn reduce_otherwise_default<'b>(
        &'b self,
        ast: &SharedSyntax<C>,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
        operator_id: &S::ConceptId,
        default_concept_id: &S::ConceptId,
    ) -> ReductionResult<C::RR> {
        let mut reduced_pair: ReductionResult<C::RR> = None;
        let mut operator_composition_check = || {
            let cache = <C as ContextCache>::SharedReductionCache::default();
            let mut context_search = self.spawn(&cache, self.delta.clone());
            context_search.syntax_evaluating.insert(ast.clone());
            reduced_pair = context_search.reduce_pair(left, right);
            let operator_concept =
                self.snap_shot.read_concept(self.delta.as_ref(), *operator_id);
            let find = |c| {
                operator_concept.find_as_hand_in_composition_with(c, Hand::Left)
            };
            reduced_pair.is_none()
                && right.get_concept().and_then(find).is_none()
        };
        if self.syntax_evaluating.contains(ast) || operator_composition_check()
        {
            Some((
                self.to_ast(default_concept_id),
                C::RR::default(*operator_id),
            ))
        } else {
            reduced_pair
        }
    }

    /// Reduces the syntax by using the reduction rules of associated concepts.
    pub fn reduce(&self, ast: &SharedSyntax<C>) -> ReductionResult<C::RR> {
        debug!("reduce({})", ast.to_string());
        self.caches.get_reduction_or_else(ast, || {
            let reduction_result = ast
                .get_concept()
                .and_then(|c| self.reduce_concept(&c))
                .or_else(|| {
                    let (ref left, ref right) = ast.get_expansion()?;
                    left.get_concept()
                        .and_then(|lc| match self.concrete_type(&lc) {
                            Some(ConcreteConceptType::Precedence) => {
                                let default_concept_id = self
                                    .concrete_concept_id(
                                        ConcreteConceptType::Default,
                                    )?;
                                self.reduce_otherwise_default(
                                    ast,
                                    left,
                                    right,
                                    &lc,
                                    &default_concept_id,
                                )
                            },
                            Some(ConcreteConceptType::Associativity) => {
                                let default_concept_id = self
                                    .concrete_concept_id(
                                        ConcreteConceptType::Right,
                                    )?;
                                self.reduce_otherwise_default(
                                    ast,
                                    left,
                                    right,
                                    &lc,
                                    &default_concept_id,
                                )
                            },
                            _ => None,
                        })
                        .or_else(|| self.reduce_pair(left, right))
                });
            self.caches.insert_reduction(ast, &reduction_result);
            reduction_result
        })
    }

    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair<'b>(
        &'b self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ReductionResult<C::RR> {
        debug!("reduce_pair({}, {})", left.to_string(), right.to_string());
        right
            .get_expansion()
            .and_then(|(ref rightleft, ref rightright)| {
                self.reduce_by_expanded_right_branch(
                    left, rightleft, rightright,
                )
            })
            .or_else(|| {
                let (leftleft, leftright) = left.get_expansion()?;
                self.reduce_by_expanded_left_branch(
                    &leftleft, &leftright, right,
                )
            })
            .or_else(|| self.recursively_reduce_pair(left, right))
    }

    fn recursively_reduce_pair<'b>(
        &'b self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ReductionResult<C::RR> {
        debug!("recursively_reduce_pair({}, {})", left, right);
        let left_result = self.reduce(left);
        let right_result = self.reduce(right);
        let maybe_subbed_r =
            right.get_concept().and_then(|r| self.variable_mask.get(r));
        let maybe_subbed_l =
            left.get_concept().and_then(|l| self.variable_mask.get(l));
        let cache = <C as ContextCache>::SharedReductionCache::default();
        match (left_result, right_result) {
            (None, None) => self
                .filter_generalisations_for_pair(left, right)
                .iter()
                .find_map(|(generalisation, variable_mask)| {
                    let mut context_search =
                        self.spawn(&cache, self.delta.clone());
                    // Stack overflow occurs if you remove this
                    context_search
                        .insert_variable_mask(variable_mask.clone())
                        .ok()?;
                    context_search.reduce_concept(generalisation).map(
                        |(ast, reason)| {
                            (
                                context_search.substitute(&ast, variable_mask),
                                C::RR::rule(
                                    self.to_ast(generalisation),
                                    variable_mask.clone(),
                                    reason,
                                ),
                            )
                        },
                    )
                }),
            (Some((left_ast, left_reason)), None) => Some((
                self.contract_pair(&left_ast, maybe_subbed_r.unwrap_or(right))
                    .share(),
                C::RR::partial(
                    hashmap! {left.clone() => (left_ast, left_reason)},
                ),
            )),
            (None, Some((right_ast, right_reason))) => Some((
                self.contract_pair(maybe_subbed_l.unwrap_or(left), &right_ast)
                    .share(),
                C::RR::partial(
                    hashmap! {right.clone() => (right_ast, right_reason)},
                ),
            )),
            (
                Some((left_ast, left_reason)),
                Some((right_ast, right_reason)),
            ) => Some((
                self.contract_pair(&left_ast, &right_ast).share(),
                C::RR::partial(hashmap! {
                    left.clone() => (left_ast, left_reason),
                    right.clone() => (right_ast, right_reason)
                }),
            )),
        }
    }

    pub fn substitute<'b>(
        &'b self,
        ast: &SharedSyntax<C>,
        variable_mask: &VariableMask<Syntax<C>>,
    ) -> SharedSyntax<C> {
        ast.get_concept()
            .and_then(|c| variable_mask.get(&c).cloned())
            .unwrap_or_else(|| {
                ast.get_expansion().map_or_else(
                    || ast.clone(),
                    |(l, r)| {
                        self.contract_pair(
                            &self.substitute(&l, variable_mask),
                            &self.substitute(&r, variable_mask),
                        )
                        .share()
                    },
                )
            })
    }

    fn filter_generalisations_for_pair(
        &self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> Generalisations<Syntax<C>> {
        let ast = self.contract_pair(left, right);
        let generalisation_candidates = self.find_generalisations(&ast);
        debug!("filter_generalisations_for_pair({}, {}): generalisation_candidates = {:#?}", left.to_string(), right.to_string(), generalisation_candidates);
        let ast = ast.share();
        let result = self.filter_generalisations_from_candidates(
            &ast,
            generalisation_candidates,
        );
        debug!(
            "filter_generalisations_for_pair({}, {}) -> {:#?}",
            left.to_string(),
            right.to_string(),
            result
        );
        result
    }

    /// Returns the abstract syntax from two syntax parts, using the label and concept of the composition of associated concepts if it exists.
    pub fn contract_pair(
        &self,
        lefthand: &SharedSyntax<C>,
        righthand: &SharedSyntax<C>,
    ) -> Syntax<C> {
        lefthand
            .get_concept()
            .and_also(&righthand.get_concept())
            .and_then(|(lc, rc)| {
                let left_concept =
                    self.snap_shot.read_concept(self.delta.as_ref(), *lc);
                left_concept
                    .find_as_hand_in_composition_with(*rc, Hand::Left)
                    .map(|def| {
                        let syntax = Syntax::<C>::from(
                            self.snap_shot
                                .get_label(self.delta.as_ref(), def)
                                .unwrap_or_else(|| {
                                    self.display_joint(lefthand, righthand)
                                }),
                        );
                        self.snap_shot.bind_concept_to_syntax(
                            self.delta.as_ref(),
                            syntax,
                            def,
                        )
                    })
            })
            .unwrap_or_else(|| self.display_joint(lefthand, righthand).into())
            .bind_pair(lefthand.clone(), righthand.clone())
    }

    /// Checks if the concepts in the composition of `generalisation` matches the corresponding nodes in `ast` or is a free variable
    /// returns `None` if does not match otherwise returns a `HashMap` mapping free variable concept IDs to the matching nodes in `ast`
    pub fn check_generalisation(
        &self,
        ast: &SharedSyntax<C>,
        generalisation: &S::ConceptId,
    ) -> Option<VariableMask<Syntax<C>>> {
        (self.is_free_variable(generalisation)
            && !self.bound_variable_syntax.contains(ast)
            && !ast.get_concept().map_or(false, |c| {
                self.snap_shot
                    .read_concept(self.delta.as_ref(), c)
                    .bounded_variable()
            }))
        .then(|| {
            if let Some((gl, gr)) = self
                .snap_shot
                .read_concept(self.delta.as_ref(), *generalisation)
                .get_composition()
            {
                let (l, r) = ast.get_expansion()?;
                match (self.is_free_variable(&gl), self.is_free_variable(&gr)) {
                    (true, true) => self
                        .check_generalisation(&l, &gl)
                        .and_also_move(self.check_generalisation(&r, &gr))
                        .and_then(|(lm, mut rm)| {
                            for (lmk, lmv) in lm {
                                if let Some(rmv) = rm.get(&lmk) {
                                    if rmv != &lmv {
                                        return None;
                                    }
                                } else {
                                    rm.insert(lmk, lmv);
                                }
                            }
                            Some(rm)
                        }),
                    (true, false) if r.get_concept() == Some(gr) => {
                        self.check_generalisation(&l, &gl)
                    },
                    (false, true) if l.get_concept() == Some(gl) => {
                        self.check_generalisation(&r, &gr)
                    },
                    (false, false)
                        if l.get_concept() == Some(gl)
                            && r.get_concept() == Some(gr) =>
                    {
                        Some(hashmap! {})
                    },
                    _ => None,
                }
            } else {
                Some(hashmap! {*generalisation => ast.clone()})
            }
        })
        .flatten()
    }

    fn find_generalisations(&self, ast: &Syntax<C>) -> HashSet<S::ConceptId> {
        let mut generalisations = HashSet::<S::ConceptId>::new();
        if let Some((l, r)) = ast.get_expansion() {
            if let Some(c) = l.get_concept() {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta.as_ref(), c)
                        .iter_composition_ids(Hand::Left),
                );
            }
            if let Some(c) = r.get_concept() {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta.as_ref(), c)
                        .iter_composition_ids(Hand::Right),
                );
            }
            self.find_generalisations(&l).iter().for_each(|g| {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta.as_ref(), *g)
                        .iter_composition_ids(Hand::Left),
                );
            });
            self.find_generalisations(&r).iter().for_each(|g| {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta.as_ref(), *g)
                        .iter_composition_ids(Hand::Right),
                );
            });
        }
        generalisations
    }

    fn is_leaf_variable(&self, lv: &S::ConceptId) -> bool {
        self.is_free_variable(lv) && self.is_leaf_concept(lv)
    }

    fn is_free_variable(&self, v: &S::ConceptId) -> bool {
        self.snap_shot.read_concept(self.delta.as_ref(), *v).free_variable()
            && self
                .variable_mask
                .tail()
                .map_or(true, |vml| vml.get(*v).is_none()) // A hack required to prevent stack overflow
    }

    fn is_leaf_concept(&self, l: &S::ConceptId) -> bool {
        self.snap_shot
            .read_concept(self.delta.as_ref(), *l)
            .get_composition()
            .is_none()
    }

    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(
        &self,
        ast: &SharedSyntax<C>,
    ) -> MaybeReducedSyntaxWithReason<C::RR> {
        debug!("recursively_reduce({})", ast);
        let mut maybe_reason: Option<C::RR> = None;
        let mut reduced_ast = ast.clone();
        while let Some((a, reason)) = self.reduce(&reduced_ast) {
            maybe_reason = Some(C::RR::recursive_reason(
                maybe_reason,
                reason,
                &reduced_ast,
            ));
            reduced_ast = a;
        }
        (reduced_ast, maybe_reason)
    }

    fn reduce_by_expanded_left_branch(
        &self,
        leftleft: &SharedSyntax<C>,
        leftright: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ReductionResult<C::RR> {
        let cct = self.concrete_type_of_ast(leftright)?;
        match cct {
            ConcreteConceptType::ExistsSuchThat
                if leftleft.is_leaf_variable() =>
            {
                let true_id = self
                    .concrete_concept_id(ConcreteConceptType::True)
                    .expect("true concept must exist");
                let true_concept =
                    self.snap_shot.read_concept(self.delta.as_ref(), true_id);
                let truths = true_concept.find_what_reduces_to_it();
                self.find_example(right, &truths.collect()).map(
                    |substitutions| {
                        let true_syntax = self.to_ast(&true_id);
                        (
                            true_syntax,
                            C::RR::existence(substitutions, right.clone()),
                        )
                    },
                )
            },
            _ => None,
        }
    }

    fn find_example(
        &self,
        generalisation: &SharedSyntax<C>,
        truths: &HashSet<S::ConceptId>,
    ) -> Option<Substitutions<SharedSyntax<C>>> {
        self.find_examples(generalisation, truths).pop()
    }

    // TODO Lazily compute the concepts that are equivalent to a given normal form
    // until a required number of examples are found
    /// `generalisation` needs to contain free variables
    fn find_examples(
        &self,
        generalisation: &SharedSyntax<C>,
        equivalence_set: &HashSet<S::ConceptId>, /* All concepts that are equal to generalisation */
    ) -> Vec<Substitutions<SharedSyntax<C>>> {
        if let Some((left, right)) = generalisation.get_expansion() {
            match (
                self.contains_bound_variable_syntax(&left),
                self.contains_bound_variable_syntax(&right),
            ) {
                (true, true) => {
                    equivalence_set.iter().filter_map(|equivalent_concept_id| {
                        let equivalent_concept = self
                            .snap_shot
                            .read_concept(self.delta.as_ref(), *equivalent_concept_id);
                        equivalent_concept.get_composition()
                    }).flat_map(|(equivalent_left_id, equivalent_right_id)| {
                        let equivalent_left = self
                            .snap_shot
                            .read_concept(self.delta.as_ref(), equivalent_left_id);
                        // TODO handle case when a concept implicitly reduces to `equivalent_left`
                        let mut equivalent_left_equivalence_set: HashSet<S::ConceptId> =
                            equivalent_left
                                .find_what_reduces_to_it()
                                .collect();
                        equivalent_left_equivalence_set.insert(equivalent_left_id);
                        let left_examples = self.find_examples(
                            &left,
                            &equivalent_left_equivalence_set,
                        );
                        let equivalent_right = self
                            .snap_shot
                            .read_concept(self.delta.as_ref(), equivalent_right_id);
                        // TODO handle case when a concept implicitly reduces to `equivalent_right`
                        let mut equivalent_right_equivalence_set: HashSet<
                            S::ConceptId,
                        > = equivalent_right
                            .find_what_reduces_to_it()
                            .collect();
                        equivalent_right_equivalence_set
                            .insert(equivalent_right_id);
                        let right_examples = self.find_examples(
                            &right,
                            &equivalent_right_equivalence_set,
                        );
                        left_examples.into_iter().flat_map(|left_example| {
                            let mut right_clone = right.clone();
                            let mutable_right = Syntax::<C>::make_mut(&mut right_clone);
                            substitute::<Syntax<C>>(mutable_right, &left_example);
                            if self.contains_bound_variable_syntax(&right_clone) {
                                self.find_examples(&right_clone, &equivalent_right_equivalence_set).into_iter().map(|mut right_example| {
                                    right_example.extend(left_example.iter().map(|(k, v)| (k.clone(), v.clone())));
                                    right_example
                                }).collect::<Vec<_>>()
                            } else if self.recursively_reduce(&right_clone).0.get_concept().map_or(false, |id| equivalent_right_equivalence_set.contains(&id)) {
                                vec![left_example]
                            } else {
                                vec![]
                            }
                        }).chain(right_examples.into_iter().flat_map(|right_example| {
                            let mut left_clone = left.clone();
                            let mutable_left = Syntax::<C>::make_mut(&mut left_clone);
                            substitute::<Syntax<C>>(mutable_left, &right_example);
                            if self.contains_bound_variable_syntax(&left_clone) {
                                self.find_examples(&left_clone, &equivalent_left_equivalence_set).into_iter().map(|mut left_example| {
                                    left_example.extend(right_example.iter().map(|(k, v)| (k.clone(), v.clone())));
                                    left_example
                                }).collect::<Vec<_>>()
                            } else if self.recursively_reduce(&left_clone).0.get_concept().map_or(false, |id| equivalent_left_equivalence_set.contains(&id)) {
                                vec![right_example]
                            } else {
                                vec![]
                            }
                        })).collect::<Vec<_>>()
                    }).collect()
                },
                (true, false) => self.find_examples_of_half_generalisation(
                    &left,
                    &right,
                    equivalence_set,
                    Hand::Right,
                ),
                (false, true) => self.find_examples_of_half_generalisation(
                    &right,
                    &left,
                    equivalence_set,
                    Hand::Left,
                ),
                (false, false) => vec![],
            }
        } else {
            debug_assert!(self.contains_bound_variable_syntax(generalisation));
            equivalence_set
                .iter()
                .map(|c| {
                    let example = self.to_ast(c);
                    hashmap! {generalisation.clone() => example}
                })
                .collect()
        }
    }

    fn find_examples_of_half_generalisation(
        &self,
        generalisated_part: &SharedSyntax<C>,
        non_generalised_part: &SharedSyntax<C>,
        equivalence_set_of_composition: &HashSet<S::ConceptId>,
        non_generalised_hand: Hand,
    ) -> Vec<Substitutions<SharedSyntax<C>>> {
        non_generalised_part.get_concept().map_or_else(
            Vec::new,
            |non_generalised_id| {
                let non_generalised_concept = self
                    .snap_shot
                    .read_concept(self.delta.as_ref(), non_generalised_id);
                let examples =
                    non_generalised_concept.iter_hand_of(non_generalised_hand);
                examples
                    .filter_map(|(generalised_hand, composition)| {
                        equivalence_set_of_composition
                            .contains(&composition)
                            .then_some(generalised_hand)
                    })
                    .filter_map(|generalised_hand| {
                        Syntax::<C>::check_example(
                            &self.to_ast(&generalised_hand),
                            generalisated_part,
                        )
                        .or_else(|| {
                            // TODO handle case when a concept implicitly reduces to `non_generalised_hand`
                            let mut equivalence_set =
                                hashset! {generalised_hand};
                            let non_generalised_hand_concept =
                                self.snap_shot.read_concept(
                                    self.delta.as_ref(),
                                    generalised_hand,
                                );
                            equivalence_set.extend(
                                non_generalised_hand_concept
                                    .find_what_reduces_to_it(),
                            );
                            self.find_example(
                                generalisated_part,
                                &equivalence_set,
                            )
                        })
                    })
                    .collect()
            },
        )
    }

    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        left: &SharedSyntax<C>,
        rightleft: &SharedSyntax<C>,
        rightright: &SharedSyntax<C>,
    ) -> ReductionResult<C::RR> {
        let cct = self.concrete_type_of_ast(rightleft)?;
        match cct {
            ConcreteConceptType::GreaterThan => {
                let (comparison, comparison_reason) =
                    self.compare(left, rightright);
                match comparison {
                    Comparison::GreaterThan => {
                        self.concrete_ast(ConcreteConceptType::True)
                    },
                    Comparison::EqualTo
                    | Comparison::LessThan
                    | Comparison::LessThanOrEqualTo => {
                        self.concrete_ast(ConcreteConceptType::False)
                    },
                    Comparison::GreaterThanOrEqualTo
                    | Comparison::Incomparable => None,
                }
                .map(|ast| (ast, comparison_reason.into()))
            },
            ConcreteConceptType::Reduction => {
                let (x, reason) = C::RR::determine_reduction_truth(
                    left,
                    rightright,
                    |syntax| self.reduce(syntax),
                )?;
                self.concrete_ast(if x {
                    ConcreteConceptType::True
                } else {
                    ConcreteConceptType::False
                })
                .map(|ast| (ast, reason))
            },
            _ => None,
        }
    }

    /// Returns the syntax for a concept. Panics if there is no concept with the given `concept_id`
    pub fn to_ast(&self, concept_id: &S::ConceptId) -> SharedSyntax<C> {
        self.variable_mask.get(*concept_id).cloned().unwrap_or_else(|| {
            self.caches.get_syntax_tree_or_else(*concept_id, || {
                let concept =
                    self.snap_shot.read_concept(self.delta.as_ref(), *concept_id);
                let syntax = concept.get_string().map_or_else(
                    || self.snap_shot.get_label(self.delta.as_ref(), *concept_id),
                    |s| Some(format_string(&s)),
                ).map_or_else(|| {
                    if let Some((left, right)) = concept.get_composition() {
                        self.combine(&self.to_ast(&left), &self.to_ast(&right))
                    } else {
                        self.snap_shot
                            .new_syntax_from_concept_that_has_no_label_or_composition(&concept)
                    }
                }, |s| {
                    self.snap_shot
                        .bind_concept_to_syntax(
                            self.delta.as_ref(),
                            Syntax::<C>::from(s),
                            *concept_id,
                        )
                }).share();
                self.caches.insert_syntax_tree(&concept, &syntax);
                syntax
            })
        })
    }

    pub fn combine(
        &self,
        ast: &SharedSyntax<C>,
        other: &SharedSyntax<C>,
    ) -> Syntax<C> {
        ast.get_concept()
            .and_also(&other.get_concept())
            .and_then(|(l, r)| {
                let left_concept =
                    self.snap_shot.read_concept(self.delta.as_ref(), *l);
                left_concept
                    .find_as_hand_in_composition_with(*r, Hand::Left)
                    .map(|concept| {
                        let syntax = self.join(ast, other);
                        self.snap_shot.bind_concept_to_syntax(
                            self.delta.as_ref(),
                            syntax,
                            concept,
                        )
                    })
            })
            .unwrap_or_else(|| self.join(ast, other))
    }

    fn join(
        &self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> Syntax<C> {
        Syntax::<C>::from(self.display_joint(left, right))
            .bind_pair(left.clone(), right.clone())
    }

    fn display_joint(
        &self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> String {
        // TODO: find a better way of checking that a syntax tree does not a have a labeled root concept
        let mut left_string = left.to_string();
        if left_string.chars().any(char::is_whitespace) {
            left_string = left.get_expansion().map_or_else(
                || left.to_string(),
                |(l, r)| self.get_associativity(&r).display_joint_left(l, r),
            );
        }
        let mut right_string = right.to_string();
        if right_string.chars().any(char::is_whitespace) {
            right_string =
                right.get_expansion().map_or(right_string, |(l, r)| {
                    self.get_associativity(&l).display_joint_right(l, r)
                });
        }
        left_string + " " + &right_string
    }

    pub fn get_associativity(&self, ast: &SharedSyntax<C>) -> Associativity {
        self.concrete_concept_id(ConcreteConceptType::Associativity).map_or(
            Associativity::Right,
            |associativity_concept_id| {
                let assoc_of_ast = self
                    .combine(&self.to_ast(&associativity_concept_id), ast)
                    .share();
                let maybe_concrete_concept_type = self
                    .reduce(&assoc_of_ast)
                    .and_then(|(ast, _)| self.concrete_type_of_ast(&ast));
                if Some(ConcreteConceptType::Left)
                    == maybe_concrete_concept_type
                {
                    Associativity::Left
                } else {
                    Associativity::Right
                }
            },
        )
    }

    /// Expands syntax by definition of its associated concept.
    pub fn expand(&self, ast: &SharedSyntax<C>) -> SharedSyntax<C> {
        ast.get_concept().map_or_else(
            || {
                if let Some((ref left, ref right)) = ast.get_expansion() {
                    self.combine(&self.expand(left), &self.expand(right))
                        .share()
                } else {
                    ast.clone()
                }
            },
            |con| {
                if let Some((left, right)) = self
                    .snap_shot
                    .read_concept(self.delta.as_ref(), con)
                    .get_composition()
                {
                    self.combine(
                        &self.expand(&self.to_ast(&left)),
                        &self.expand(&self.to_ast(&right)),
                    )
                    .share()
                } else {
                    self.to_ast(&con)
                }
            },
        )
    }

    #[allow(clippy::too_many_lines)]
    pub fn compare<'b>(
        &'b self,
        some_syntax: &SharedSyntax<C>,
        another_syntax: &SharedSyntax<C>,
    ) -> (Comparison, ComparisonReason<C::RR>) {
        if some_syntax == another_syntax {
            return (Comparison::EqualTo, ComparisonReason::SameSyntax);
        }

        self.concrete_concept_id(ConcreteConceptType::GreaterThan).map_or((Comparison::Incomparable, ComparisonReason::NoGreaterThanConcept), |greater_than_concept_id|
        {
            let greater_than_syntax = self.to_ast(&greater_than_concept_id);
            let comparing_syntax = self.combine(
                some_syntax,
                &self.combine(&greater_than_syntax, another_syntax).share(),
            );
            let comparing_reversed_syntax = self.combine(
                another_syntax,
                &self.combine(&greater_than_syntax, some_syntax).share(),
            );
            let mut reason = None;
            let mut reversed_reason = None;
            let cache = <C as ContextCache>::SharedReductionCache::default();
            (
                match (
                    if self.syntax_evaluating.contains(&comparing_syntax) {
                        self.caches
                            .get_reduction_or_else(
                                &comparing_syntax.share(),
                                || None,
                            )
                            .map(|(s, r)| (s, Some(r)))
                    } else {
                        let mut context_search = self.spawn(&cache, self.delta.clone());
                        let comparing_syntax = comparing_syntax.share();
                        context_search
                            .syntax_evaluating
                            .insert(comparing_syntax.clone());
                        Some(
                            context_search
                                .recursively_reduce(&comparing_syntax),
                        )
                    }
                    .and_then(
                        |(syntax_comparison, local_reason)| {
                            reason = local_reason;
                            self.concrete_type_of_ast(&syntax_comparison)
                        },
                    ),
                    if self
                        .syntax_evaluating
                        .contains(&comparing_reversed_syntax)
                    {
                        self.caches
                            .get_reduction_or_else(
                                &comparing_reversed_syntax.share(),
                                || None,
                            )
                            .map(|(s, r)| (s, Some(r)))
                    } else {
                        let mut context_search = self.spawn(&cache, self.delta.clone());
                        let comparing_reversed_syntax =
                            comparing_reversed_syntax.share();
                        context_search
                            .syntax_evaluating
                            .insert(comparing_reversed_syntax.clone());
                        Some(
                            context_search
                                .recursively_reduce(&comparing_reversed_syntax),
                        )
                    }
                    .and_then(
                        |(reversed_comparison, local_reversed_reason)| {
                            reversed_reason = local_reversed_reason;
                            self.concrete_type_of_ast(&reversed_comparison)
                        },
                    ),
                ) {
                    (
                        Some(ConcreteConceptType::False),
                        Some(ConcreteConceptType::False),
                    ) => Comparison::EqualTo,
                    (
                        Some(ConcreteConceptType::True),
                        Some(ConcreteConceptType::True),
                    ) => {
                        panic!("{:#?} is both greater than and less than {:#?}!\nReason: {:#?}\n Reversed reason: {:#?}", some_syntax, another_syntax, reason, reversed_reason);
                    },
                    (Some(ConcreteConceptType::True), _) => {
                        Comparison::GreaterThan
                    },
                    (_, Some(ConcreteConceptType::True)) => {
                        Comparison::LessThan
                    },
                    (Some(ConcreteConceptType::False), _) => {
                        Comparison::LessThanOrEqualTo
                    },
                    (_, Some(ConcreteConceptType::False)) => {
                        Comparison::GreaterThanOrEqualTo
                    },
                    _ => Comparison::Incomparable,
                },
                C::RR::simplify_reasoning(reason, reversed_reason),
            )
        })
    }

    pub fn spawn<'b, 'c>(
        &'b self,
        cache: &'c <C as ContextCache>::SharedReductionCache,
        delta: D,
    ) -> ContextSearch<'s, 'v, S, C, VML, SDCD, D, CCI>
    where
        VML: VariableMaskList,
    {
        ContextSearch::<'s, 'v> {
            bound_variable_syntax: self.bound_variable_syntax,
            caches: self.caches.spawn(cache),
            delta,
            snap_shot: self.snap_shot,
            syntax_evaluating: self.syntax_evaluating.clone(),
            variable_mask: self.variable_mask.clone(),
            phantom: self.phantom,
            phantom2: self.phantom2,
        }
    }

    /// Error if `variable_mask` is already included
    fn insert_variable_mask(
        &mut self,
        variable_mask: VariableMask<Syntax<C>>,
    ) -> Result<(), ()> {
        self.variable_mask =
            VML::push(&self.variable_mask, variable_mask).ok_or(())?.into();
        Ok(())
    }

    fn contains_bound_variable_syntax(&self, syntax: &SharedSyntax<C>) -> bool {
        self.caches.remember_if_contains_bound_variable_syntax_or_else(
            syntax,
            || {
                if let Some((left, right)) = syntax.get_expansion() {
                    if self.contains_bound_variable_syntax(&left)
                        || self.contains_bound_variable_syntax(&right)
                    {
                        return true;
                    }
                }
                self.bound_variable_syntax.contains(syntax)
                    || syntax.get_concept().map_or(false, |c| {
                        self.snap_shot
                            .read_concept(self.delta.as_ref(), c)
                            .bounded_variable()
                    })
            },
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Example<Syntax: SyntaxTree, RR: ReductionReason> {
    generalisation: Syntax::SharedSyntax,
    substitutions: HashMap<Syntax::SharedSyntax, Match<Syntax, RR>>,
}

type MaybeReducedSyntaxWithReason<RR> = (RRSharedSyntax<RR>, Option<RR>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match<Syntax: SyntaxTree, RR: ReductionReason> {
    value: Syntax::SharedSyntax,
    reduction: Syntax::SharedSyntax,
    reason: RR,
}

#[derive(PartialEq, Debug, Eq)]
pub enum Comparison {
    GreaterThan,
    LessThan,
    Incomparable,
    EqualTo,
    GreaterThanOrEqualTo,
    LessThanOrEqualTo,
}

#[derive(PartialEq, Eq, Debug)]
pub enum ComparisonReason<RR: ReductionReason> {
    SameSyntax,
    Reduction {
        reason: Option<RR>,
        reversed_reason: Option<RR>,
    },
    NoGreaterThanConcept,
}

impl<'c, 's, 'v, S, C, SDCD, VML, D, CCI: Clone + Display + Copy + Debug + Eq + Hash>
    From<ContextReferences<'c, 's, 'v, S, C, D>>
    for ContextSearch<'s, 'v, S, C, VML, SDCD, D, CCI>
where
    S: SnapShotReader<SDCD, ConceptId = CCI> + Sync + std::fmt::Debug,
    Self: Iteration<ConceptId = S::ConceptId, Syntax = Syntax<C>>,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>
        + Debug,
    VML: VariableMaskList<Syntax = Syntax<C>>,
    D: Clone + AsRef<NestedContextDelta<S::ConceptId, SDCD, D>> + Debug + From<NestedContextDelta<S::ConceptId, SDCD, D>>
{
    fn from(
        ContextReferences {
            snap_shot,
            delta,
            cache,
            bound_variable_syntax,
        }: ContextReferences<'c, 's, 'v, S, C, D>,
    ) -> Self {
        // simple_logger::init().unwrap_or(());
        Self {
            bound_variable_syntax,
            snap_shot,
            variable_mask: VML::from(hashmap! {}).into(),
            delta,
            caches: cache.clone(),
            syntax_evaluating: hashset! {},
            phantom: PhantomData::default(),
            phantom2: PhantomData::default(),
        }
    }
}

pub struct ContextReferences<'c, 's, 'v, S, C: ContextCache, D> {
    pub snap_shot: &'s S,
    pub delta: D,
    pub cache: &'c C,
    pub bound_variable_syntax: &'v HashSet<SharedSyntax<C>>,
}

pub type Generalisations<Syntax> =
    Vec<(<Syntax as SyntaxTree>::ConceptId, VariableMask<Syntax>)>;

pub type ReductionTruthResult<RR> = Option<(bool, RR)>;
