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
    ast::{ExampleSubstitutions, SyntaxTree},
    concepts::{format_string, ConceptTrait, ConcreteConceptType, Hand},
    consistent_merge::ConsistentMerge,
    context_cache::ContextCache,
    context_delta::{
        DirectConceptDelta, NestedDelta, NewConceptDelta, SharedDelta,
    },
    mixed_concept::MixedConcept,
    reduction_reason::{
        RRSharedSyntax, ReductionReason, ReductionResult, SharedSyntax, Syntax,
    },
    snap_shot::Reader as SnapShotReader,
    substitute::substitute,
    variable_mask_list::{VariableMask, VariableMaskList},
};
use log::debug;
use maplit::{hashmap, hashset};
use std::{collections::HashSet, fmt::Debug, iter, marker::PhantomData};

#[derive(Debug)]
pub struct ContextSearch<'s, 'v, S, C, VML, SDCD, D, CCI: MixedConcept>
where
    S: SnapShotReader<SDCD, ConceptId = CCI> + Sync + std::fmt::Debug,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>
        + Debug,
    VML: VariableMaskList<Syntax = Syntax<C>>,
    D: SharedDelta<NestedDelta = NestedDelta<S::ConceptId, SDCD, D>>,
{
    snap_shot: &'s S,
    variable_mask: VML::Shared,
    delta: D,
    caches: C,
    syntax_evaluating: HashSet<SharedSyntax<C>>,
    concept_inferring: HashSet<S::ConceptId>,
    bound_variable_syntax: &'v HashSet<SharedSyntax<C>>,
    phantom: PhantomData<SDCD>,
    phantom2: PhantomData<CCI>,
}

impl<'s, 'v, S, C, SDCD, VML, D, CCI: MixedConcept>
    ContextSearch<'s, 'v, S, C, VML, SDCD, D, CCI>
where
    S: SnapShotReader<SDCD, ConceptId = CCI> + Sync + std::fmt::Debug,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>
        + Debug,
    VML: VariableMaskList<Syntax = Syntax<C>>,
    D: AsRef<NestedDelta<S::ConceptId, SDCD, D>>
        + SharedDelta<NestedDelta = NestedDelta<S::ConceptId, SDCD, D>>,
{
    fn infer_reduction(
        &self,
        concept: &S::MixedConcept<'_>,
    ) -> ReductionResult<C::RR> {
        debug!("infer_reduction({:#?})", concept);
        let implication_id =
            self.concrete_concept_id(ConcreteConceptType::Implication)?;
        let result = concept
            .find_as_hand_in_composition_with(implication_id, Hand::Right)
            .and_then(|composition_id| {
                let composition_concept = self
                    .snap_shot
                    .read_concept(self.delta.as_ref(), composition_id);
                // TODO: check if implication is actually true
                // TODO: check for any concepts that are the reduction of `concept`
                let result = composition_concept
                    .iter_hand_of(Hand::Right)
                    .find_map(|(condition_id, implication_rule_id)| {
                        let condition = self.to_ast(&condition_id);
                        let (reduced_condition, reason) =
                            self.reduce(&condition)?;
                        let x = reduced_condition.get_concept()?;
                        self.is_concrete_type(ConcreteConceptType::True, &x)
                            .map(|x| {
                                (
                                    // TODO: this should be the "true" concept only if the implication result is equivalent to the `concept`
                                    self.to_ast(&x),
                                    C::RR::inference(
                                        self.to_ast(&implication_rule_id),
                                        reason,
                                    ),
                                )
                            })
                    });
                result
            });
        result.or_else(|| {
            self.caches.get_inference_or_else(concept.id(), || {
                let ast = &self.to_ast(&concept.id());
                let rr = self.find_examples_of_inferred_reduction(ast);
                self.caches.insert_inference(concept.id(), &rr);
                rr
            })
        })
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
        if self.concept_inferring.contains(id) {
            None
        } else {
            self.infer_reduction(&concept)
        }
        .or_else(|| {
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
    fn reduce_otherwise_default(
        &self,
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
            let maybe_concept: Option<CCI> = ast.get_concept();
            if let Some(id) = maybe_concept {
                if self.concrete_type(&id).is_some() {
                    return None;
                }
            }
            let reduction_result = maybe_concept
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
    fn reduce_pair(
        &self,
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

    fn recursively_reduce_pair(
        &self,
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
            (None, None) => {
                let ast = self.contract_pair(left, right);
                let generalisation_candidates = self.find_generalisations(&ast);
                let ast = ast.share();
                generalisation_candidates
                    .into_iter()
                    .filter_map(move |gc| {
                        self.check_generalisation(&ast, &gc).and_then(|vm| {
                            if vm.is_empty() {
                                None
                            } else {
                                Some((gc, vm))
                            }
                        })
                    })
                    .find_map(|(generalisation, variable_mask)| {
                        let mut context_search =
                            self.spawn(&cache, self.delta.clone());
                        // Stack overflow occurs if you remove this
                        context_search
                            .insert_variable_mask(variable_mask.clone())
                            .ok()?;
                        context_search.reduce_concept(&generalisation).map(
                            |(ast, reason)| {
                                (
                                    context_search
                                        .substitute(&ast, &variable_mask),
                                    C::RR::rule(
                                        self.to_ast(&generalisation),
                                        variable_mask.clone(),
                                        reason,
                                    ),
                                )
                            },
                        )
                    })
            },
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

    pub fn substitute(
        &self,
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
                self.find_example(right, truths).map(|substitutions| {
                    // TODO: determine whether substitutions.example should be considered
                    let true_syntax = self.to_ast(&true_id);
                    (
                        true_syntax,
                        C::RR::existence(
                            substitutions.generalisation,
                            right.clone(),
                        ),
                    )
                })
            },
            _ => None,
        }
    }

    fn find_example(
        &self,
        generalisation: &SharedSyntax<C>,
        truths: impl Iterator<Item = S::ConceptId>,
    ) -> Option<ExampleSubstitutions<Syntax<C>>> {
        self.find_examples(generalisation.clone(), truths).next()
    }

    #[allow(clippy::too_many_lines)]
    pub fn find_examples_of_inferred_reduction(
        &self,
        ast_to_reduce: &SharedSyntax<C>,
    ) -> ReductionResult<C::RR> {
        let implication_id =
            self.concrete_concept_id(ConcreteConceptType::Implication)?;
        let reduction_operator =
            self.concrete_ast(ConcreteConceptType::Reduction)?;
        let mut spawned_delta = NestedDelta::spawn(self.delta.clone());
        let variable_reduction_id = spawned_delta
            .insert_delta_for_new_concept(NewConceptDelta::BoundVariable);
        let variable_reduction_syntax =
            Syntax::<C>::new_leaf_variable(variable_reduction_id).share();
        let variable_condition_id = spawned_delta
            .insert_delta_for_new_concept(NewConceptDelta::BoundVariable);
        let variable_condition_syntax =
            Syntax::<C>::new_leaf_variable(variable_condition_id).share();
        let variable_result_id = spawned_delta
            .insert_delta_for_new_concept(NewConceptDelta::BoundVariable);
        let _variable_result_syntax =
            Syntax::<C>::new_leaf_variable(variable_result_id).share();
        let cache = <C as ContextCache>::SharedReductionCache::default();
        let mut spawned_context_search =
            self.spawn(&cache, D::from_nested(spawned_delta));
        if let Some(concept) = ast_to_reduce.get_concept() {
            spawned_context_search.concept_inferring.insert(concept);
        }
        let implication_syntax = spawned_context_search.to_ast(&implication_id);
        let implication_rule_fn =
            |condition: &SharedSyntax<C>,
             prereduction: &SharedSyntax<C>,
             reduction: &SharedSyntax<C>| {
                Syntax::<C>::new_pair(
                    condition.clone(),
                    Syntax::<C>::new_pair(
                        implication_syntax.clone(),
                        Syntax::<C>::new_pair(
                            prereduction.clone(),
                            Syntax::<C>::new_pair(
                                reduction_operator.clone(),
                                reduction.clone(),
                            )
                            .share(),
                        )
                        .share(),
                    )
                    .share(),
                )
            };
        let implication_rule_pattern = implication_rule_fn(
            &variable_condition_syntax,
            ast_to_reduce,
            &variable_reduction_syntax,
        );
        let true_id = spawned_context_search
            .concrete_concept_id(ConcreteConceptType::True)
            .expect("true concept must exist");
        let equivalent_concept =
            self.snap_shot.read_concept(self.delta.as_ref(), true_id);
        // TODO handle case when a concept implicitly reduces to `equivalent_concept`
        let truths = equivalent_concept
            .find_what_reduces_to_it()
            .chain(iter::once(true_id));

        let irp = implication_rule_pattern.share();
        let result = spawned_context_search
            .find_examples(irp, truths)
            .find_map(|substitutions| {
                substitutions
                    .generalisation
                    .get(&variable_condition_syntax)
                    .and_then(|condition_syntax| {
                        let substituted_condition = self.substitute(
                            condition_syntax,
                            &substitutions.example,
                        );
                        let (condition_normal_form, reason) =
                            spawned_context_search
                                .recursively_reduce(&substituted_condition);
                        spawned_context_search.is_concrete_type(
                            ConcreteConceptType::True,
                            &condition_normal_form.get_concept()?,
                        )?;
                        substitutions
                            .generalisation
                            .get(&variable_reduction_syntax)
                            .map(|result| {
                                (
                                    self.substitute(
                                        result,
                                        &substitutions.example,
                                    ),
                                    C::RR::inference(
                                        implication_rule_fn(
                                            condition_syntax,
                                            ast_to_reduce,
                                            result,
                                        )
                                        .share(),
                                        reason.unwrap_or_else(
                                            ReductionReason::explicit,
                                        ),
                                    ),
                                )
                            })
                    })
            });
        result
    }

    fn find_examples<'a>(
        &'a self,
        generalisation: SharedSyntax<C>,
        equivalence_set: impl Iterator<Item = S::ConceptId> + 'a, /* All concepts that are equal to generalisation */
    ) -> impl Iterator<Item = ExampleSubstitutions<Syntax<C>>> + 'a {
        let iterator: Box<
            dyn Iterator<Item = ExampleSubstitutions<Syntax<C>>>,
        >;
        if let Some((left, right)) = generalisation.get_expansion() {
            iterator = Box::new(self.find_examples_of_branched_generalisation(
                left,
                right,
                equivalence_set,
            ));
        } else {
            debug_assert!(
                self.contains_bound_variable_syntax(&generalisation),
                "Generalisation ({generalisation}) doesn't contain bound variables"
            );
            iterator = Box::new(equivalence_set
                .map(move |c| {
                    let example = self.to_ast(&c);
                    ExampleSubstitutions {
                        generalisation: hashmap! {generalisation.clone() => example},
                        example: hashmap!{}
                    }
                }));
        }
        iterator
    }

    #[allow(clippy::too_many_lines)]
    fn find_examples_of_branched_generalisation<'a>(
        &'a self,
        left: SharedSyntax<C>,
        right: SharedSyntax<C>,
        equivalence_set: impl Iterator<Item = S::ConceptId> + 'a,
    ) -> impl Iterator<Item = ExampleSubstitutions<Syntax<C>>> + 'a {
        let iterator: Box<
            dyn Iterator<Item = ExampleSubstitutions<Syntax<C>>>,
        > = match (
            self.contains_bound_variable_syntax(&left),
            self.contains_bound_variable_syntax(&right),
        ) {
            (true, true) => {
                Box::new(
                    equivalence_set
                        .filter_map(|equivalent_concept_id| {
                            self.composition_of_concept(&equivalent_concept_id)
                        })
                        .filter_map(
                            move |(equivalent_left_id, equivalent_right_id)| {
                                let equivalent_concept =
                                    self.snap_shot.read_concept(
                                        self.delta.as_ref(),
                                        equivalent_left_id,
                                    );
                                // TODO handle case when a concept implicitly reduces to `equivalent_concept`
                                let equivalent_left_equivalence_set =
                                    equivalent_concept
                                        .find_what_reduces_to_it()
                                        .chain(iter::once(equivalent_left_id));
                                // TODO try to find a case where this needs to be a flat_map method call
                                let maybe_example = self
                                    .find_examples(
                                        left.clone(),
                                        equivalent_left_equivalence_set,
                                    )
                                    .find_map(|left_example| {
                                        let mut right_clone = right.clone();
                                        let mutable_right =
                                            Syntax::<C>::make_mut(
                                                &mut right_clone,
                                            );
                                        substitute::<Syntax<C>>(
                                            mutable_right,
                                            &left_example.generalisation,
                                        );
                                        // `right_clone` now has any generalised variables matched in the example of `left` substituted for
                                        // but may still have some unmatched generalised variables
                                        let substituted_right = self
                                            .substitute(
                                                &right_clone,
                                                &left_example.example,
                                            );
                                        let equivalent_concept =
                                            self.snap_shot.read_concept(
                                                self.delta.as_ref(),
                                                equivalent_right_id,
                                            );
                                        // TODO handle case when a concept implicitly reduces to `equivalent_concept`
                                        let mut
                                        equivalent_right_equivalence_set =
                                            equivalent_concept
                                                .find_what_reduces_to_it()
                                                .chain(iter::once(
                                                    equivalent_right_id,
                                                ));
                                        if self.contains_bound_variable_syntax(
                                            &substituted_right,
                                        ) {
                                            self.find_examples(
                                    substituted_right,
                                    equivalent_right_equivalence_set,
                                )
                                .find_map(|right_example| {
                                // TODO find test case where this needs to be filter_map
                                    right_example
                                        .consistent_merge(left_example.clone())
                                })
                                        } else if self
                                            .recursively_reduce(
                                                &substituted_right,
                                            )
                                            .0
                                            .get_concept()
                                            .map_or(false, |id| {
                                                equivalent_right_equivalence_set
                                                    .any(
                                                        |equivalent_right_id| {
                                                            equivalent_right_id
                                                                == id
                                                        },
                                                    )
                                            })
                                        {
                                            Some(left_example)
                                        } else {
                                            None
                                        }
                                    });
                                maybe_example
                            },
                        ),
                )
            },
            (true, false) => Box::new(
                self.find_examples_of_half_generalisation(
                    left.clone(),
                    right.clone(),
                    equivalence_set,
                    Hand::Right,
                )
                .into_iter(),
            ),
            (false, true) => Box::new(
                self.find_examples_of_half_generalisation(
                    right.clone(),
                    left.clone(),
                    equivalence_set,
                    Hand::Left,
                )
                .into_iter(),
            ),
            (false, false) => Box::new(iter::empty()),
        };
        iterator
    }

    fn find_examples_of_half_generalisation<'a>(
        &'a self,
        generalised_part: SharedSyntax<C>,
        non_generalised_part: SharedSyntax<C>,
        mut equivalence_set_of_composition: impl Iterator<Item = S::ConceptId> + 'a,
        non_generalised_hand: Hand,
    ) -> Option<ExampleSubstitutions<Syntax<C>>> {
        let non_generalised_part_clone = non_generalised_part.clone();
        let generalised_part_clone = generalised_part;
        // TODO try to test if this needs to be a flat_map call
        equivalence_set_of_composition.find_map(move |equivalent_concept_id| {
            let equivalent_concept = self
                .snap_shot
                .read_concept(self.delta.as_ref(), equivalent_concept_id);
            let (left, right) = equivalent_concept.get_composition()?;
            let (equivalent_non_generalised_hand, equivalent_generalised_hand) = match non_generalised_hand {
                Hand::Left => (left, right),
                Hand::Right => (right, left)
            };
            if Some(equivalent_non_generalised_hand) != non_generalised_part_clone.get_concept() {
                if self.snap_shot.read_concept(self.delta.as_ref(), equivalent_non_generalised_hand).free_variable() {
                    return self.find_example(&generalised_part_clone, iter::once(equivalent_generalised_hand)).and_then(|subs| {
                        // Could have a more efficient method for this
                        subs.consistent_merge(ExampleSubstitutions{example: hashmap!{equivalent_non_generalised_hand => non_generalised_part_clone.clone()}, ..Default::default()})
                    })
                }
                return None;
            }
            self.find_example(&generalised_part_clone, iter::once(equivalent_generalised_hand)).or_else(|| {
        let non_generalised_id = non_generalised_part.get_concept()?;
                    let example_hand = match non_generalised_hand {
                        Hand::Left => (left == non_generalised_id).then_some(right)?,
                        Hand::Right => (right == non_generalised_id).then_some(left)?,
                    };
                    let example_hand_syntax = self.to_ast(&example_hand);
                    Syntax::<C>::check_example(
                        &example_hand_syntax,
                        &generalised_part_clone,
                    )
                    .or_else(|| {
                        // TODO handle case when a concept implicitly reduces to `non_generalised_hand`
                        let equivalence_set = iter::once(example_hand);
                        let non_generalised_hand_concept = self
                            .snap_shot
                            .read_concept(self.delta.as_ref(), example_hand);
                        self.find_example(&generalised_part_clone, equivalence_set.chain(non_generalised_hand_concept
                                .find_what_reduces_to_it()))
                    })
            })
        })
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
    pub fn to_ast(
        &self,
        concept_id: &(impl Into<S::ConceptId> + Copy),
    ) -> SharedSyntax<C> {
        let concept_id = (*concept_id).into();
        self.variable_mask.get(concept_id).cloned().unwrap_or_else(|| {
            self.caches.get_syntax_tree_or_else(concept_id, || {
                let concept =
                self.snap_shot.read_concept(self.delta.as_ref(), concept_id);
                let syntax = concept.get_string().map_or_else(
                    || self.snap_shot.get_label(self.delta.as_ref(), concept_id),
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
                        concept_id,
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
                if let Some((left, right)) = self.composition_of_concept(&con) {
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
    pub fn compare(
        &self,
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
            let determine_concrete_type_whilst_noting_reversed_reason =|(reversed_comparison, local_reversed_reason)| {
                reversed_reason = local_reversed_reason;
                self.concrete_type_of_ast(&reversed_comparison)
            };
            let determine_concrete_type_whilst_noting_reason =|(syntax_comparison, local_reason)| {
                reason = local_reason;
                self.concrete_type_of_ast(&syntax_comparison)
            };
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
                        determine_concrete_type_whilst_noting_reason,
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
                        determine_concrete_type_whilst_noting_reversed_reason,
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
                        panic!("{some_syntax:#?} is both greater than and less than {another_syntax:#?}!\nReason: {reason:#?}\n Reversed reason: {reversed_reason:#?}");
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
    ) -> Self
    where
        VML: VariableMaskList,
    {
        ContextSearch::<'s, 'v> {
            concept_inferring: self.concept_inferring.clone(),
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

    /// Checks if the concepts in the composition of `generalisation` matches the corresponding nodes in `ast` or is a free variable
    /// returns `None` if does not match otherwise returns a `HashMap` mapping free variable concept IDs to the matching nodes in `ast`
    /// TODO: refactor into method on struct with `self.snap_shot`, self.delta, `self.bound_variable_syntax` and `self.variable_mask`
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
            if let Some((gl, gr)) = self.composition_of_concept(generalisation)
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

    // TODO: move to separate struct that just has self.delta, self.snap_shot and self.variable_mask
    fn is_leaf_variable(&self, lv: &S::ConceptId) -> bool {
        self.is_free_variable(lv) && self.is_leaf_concept(lv)
    }

    // TODO: move to separate struct that just has self.delta, self.snap_shot and self.variable_mask
    fn is_free_variable(&self, v: &S::ConceptId) -> bool {
        self.snap_shot.read_concept(self.delta.as_ref(), *v).free_variable()
            && self
                .variable_mask
                .tail()
                .map_or(true, |vml| vml.get(*v).is_none()) // A hack required to prevent stack overflow
    }

    // TODO: move to separate struct that just has self.delta and self.snap_shot
    fn is_leaf_concept(&self, l: &S::ConceptId) -> bool {
        self.composition_of_concept(l).is_none()
    }

    // TODO: move to separate struct that just has self.delta and self.snap_shot
    fn composition_of_concept(
        &self,
        composition_id: &S::ConceptId,
    ) -> Option<(S::ConceptId, S::ConceptId)> {
        self.snap_shot
            .read_concept(self.delta.as_ref(), *composition_id)
            .get_composition()
    }

    // TODO: refactor into method of struct with access to self.snap_shot and self.delta
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

    // TODO: refactor into method of struct with access to self.snap_shot and self.delta
    fn concrete_type(
        &self,
        concept_id: &S::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.snap_shot.concrete_concept_type(self.delta.as_ref(), *concept_id)
    }

    // TODO: refactor into method of struct with access to self.snap_shot and self.delta
    fn concrete_type_of_ast(
        &self,
        ast: &SharedSyntax<C>,
    ) -> Option<ConcreteConceptType> {
        ast.get_concept().and_then(|c| self.concrete_type(&c))
    }

    // TODO: refactor into method of struct with access to self.snap_shot and self.delta
    fn concrete_concept_id(
        &self,
        cct: ConcreteConceptType,
    ) -> Option<S::ConceptId> {
        self.snap_shot.concrete_concept_id(self.delta.as_ref(), cct)
    }

    /// Error if `variable_mask` is already included
    /// TODO: refactor as method on `VariableMaskList`
    fn insert_variable_mask(
        &mut self,
        variable_mask: VariableMask<Syntax<C>>,
    ) -> Result<(), ()> {
        self.variable_mask =
            VML::push(&self.variable_mask, variable_mask).ok_or(())?.into();
        Ok(())
    }

    // TODO: move to separate struct with access to self.snap_shot, self.bound_variable_syntax, self.delta and self.caches
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

type MaybeReducedSyntaxWithReason<RR> = (RRSharedSyntax<RR>, Option<RR>);

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

impl<'c, 's, 'v, S, C, SDCD, VML, D, CCI: MixedConcept>
    From<ContextReferences<'c, 's, 'v, S, C, D>>
    for ContextSearch<'s, 'v, S, C, VML, SDCD, D, CCI>
where
    S: SnapShotReader<SDCD, ConceptId = CCI> + Sync + std::fmt::Debug,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>
        + Debug,
    VML: VariableMaskList<Syntax = Syntax<C>>,
    D: AsRef<NestedDelta<S::ConceptId, SDCD, D>>
        + SharedDelta<NestedDelta = NestedDelta<S::ConceptId, SDCD, D>>,
{
    fn from(
        ContextReferences {
            snap_shot,
            delta,
            cache,
            bound_variable_syntax,
        }: ContextReferences<'c, 's, 'v, S, C, D>,
    ) -> Self {
        Self {
            concept_inferring: HashSet::default(),
            bound_variable_syntax,
            snap_shot,
            variable_mask: VML::from(hashmap! {}).into(),
            delta,
            caches: cache.clone(),
            syntax_evaluating: hashset! {},
            phantom: PhantomData,
            phantom2: PhantomData,
        }
    }
}

pub struct ContextReferences<'c, 's, 'v, S, C: ContextCache, D> {
    pub snap_shot: &'s S,
    pub delta: D,
    pub cache: &'c C,
    pub bound_variable_syntax: &'v HashSet<SharedSyntax<C>>,
}

pub type ReductionTruthResult<RR> = Option<(bool, RR)>;
