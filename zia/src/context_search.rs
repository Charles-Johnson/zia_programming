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
    ast::{ExampleSubstitutions, GenericSyntaxTree, SyntaxKey},
    concepts::{format_string, ConceptTrait, ConcreteConceptType, Hand},
    consistent_merge::ConsistentMerge,
    context_cache::{GenericCache, ReductionCache},
    context_delta::{DirectConceptDelta, NestedDelta, NewConceptDelta},
    mixed_concept::{ConceptId, MixedConcept},
    nester::SharedReference,
    reduction_reason::{ReductionReason, ReductionResult, SharedSyntax},
    snap_shot::Reader as SnapShotReader,
    substitute::substitute,
    variable_mask_list::{VariableMask, VariableMaskList},
};
use dashmap::DashSet;
use log::debug;
use maplit::{hashmap, hashset};
use std::{
    collections::HashSet,
    fmt::Debug,
    iter::{self, empty},
    marker::PhantomData,
};

pub struct ContextSearch<'s, 'v, S, CCI: ConceptId, SR: SharedReference>
where
    S: SnapShotReader<SR, ConceptId = CCI> + Sync + std::fmt::Debug,
{
    snap_shot: &'s S,
    variable_mask: SR::Share<VariableMaskList<CCI, SR>>,
    delta: SR::Share<NestedDelta<CCI, SR>>,
    caches: GenericCache<CCI, SR>,
    syntax_evaluating: HashSet<SyntaxKey<CCI>>,
    concept_inferring: HashSet<S::ConceptId>,
    bound_variable_syntax: &'v HashSet<SyntaxKey<CCI>>,
    phantom: PhantomData<SR::Share<DirectConceptDelta<CCI>>>,
    phantom2: PhantomData<CCI>,
}
impl<S, CCI: MixedConcept, SR: SharedReference> Debug
    for ContextSearch<'_, '_, S, CCI, SR>
where
    S: SnapShotReader<SR, ConceptId = CCI> + Sync + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContextSearch")
            .field("snap_shot", &self.snap_shot)
            .field("variable_mask", &"[variable_mask]")
            .field("delta", self.delta.as_ref())
            .field("caches", &"[caches]")
            .field("syntax_evaluating", &self.syntax_evaluating)
            .field("concept_inferring", &self.concept_inferring)
            .field("bound_variable_syntax", &self.bound_variable_syntax)
            .field("phantom", &self.phantom)
            .field("phantom2", &self.phantom2)
            .finish()
    }
}

impl<'s, 'v, S, CCI: MixedConcept, SR: SharedReference>
    ContextSearch<'s, 'v, S, CCI, SR>
where
    S: SnapShotReader<SR, ConceptId = CCI> + Sync + std::fmt::Debug,
{
    fn infer_reduction(
        &self,
        concept: &S::MixedConcept<'_>,
    ) -> ReductionResult<CCI, SR> {
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
                        let implication_rule =
                            self.to_ast(&implication_rule_id);
                        if let Some((reduction, _)) =
                            self.reduce(&implication_rule)
                        {
                            if self.concrete_type_of_ast(&reduction)
                                == Some(ConcreteConceptType::True)
                            {
                                let condition = self.to_ast(&condition_id);
                                let (reduced_condition, reason) =
                                    self.reduce(&condition)?;
                                let x = reduced_condition.get_concept()?;
                                self.is_concrete_type(
                                    ConcreteConceptType::True,
                                    &x,
                                )
                                .map(|x| {
                                    (
                                        // TODO: this should be the "true" concept only if the implication result is equivalent to the `concept`
                                        self.to_ast(&x),
                                        ReductionReason::<CCI, SR>::inference(
                                            self.to_ast(&implication_rule_id),
                                            reason,
                                        ),
                                    )
                                })
                            } else {
                                None
                            }
                        } else {
                            None
                        }
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
    ) -> Option<SharedSyntax<CCI, SR>> {
        self.concrete_concept_id(cct).map(|id| self.to_ast(&id))
    }

    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, id: &S::ConceptId) -> ReductionResult<CCI, SR> {
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
            .map(|r| (r, ReductionReason::<CCI, SR>::explicit()))
        })
    }

    /// Reduces the syntax by using the reduction rules of associated concepts.
    pub fn reduce(
        &self,
        ast: &SharedSyntax<CCI, SR>,
    ) -> ReductionResult<CCI, SR> {
        debug!("reduce({})", ast.to_string());
        self.caches.get_reduction_or_else(ast, || {
            debug!("Cache miss: {}", ast.as_ref());
            let maybe_concept: Option<CCI> = ast.get_concept();
            if let Some(id) = maybe_concept {
                if self.concrete_type(&id).is_some() {
                    // TODO: cache this result without breaking tests
                    return None;
                }
            }
            let reduction_result = maybe_concept
                .and_then(|c| self.reduce_concept(&c))
                .or_else(|| {
                    let (ref left, ref right) = ast.get_expansion()?;
                    self.reduce_pair(left, right)
                });
            self.caches.insert_reduction(ast, &reduction_result);
            reduction_result
        })
    }

    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair(
        &self,
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
    ) -> ReductionResult<CCI, SR> {
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
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
    ) -> ReductionResult<CCI, SR> {
        debug!(
            "recursively_reduce_pair({}, {})",
            left.as_ref(),
            right.as_ref()
        );
        let left_result = self.reduce(left);
        let right_result = self.reduce(right);
        let maybe_subbed_r =
            right.get_concept().and_then(|r| self.variable_mask.get(r));
        let maybe_subbed_l =
            left.get_concept().and_then(|l| self.variable_mask.get(l));
        let cache = SR::share(ReductionCache::<CCI, SR>::default());
        match (left_result, right_result) {
            (None, None) => {
                let ast = SR::share(self.contract_pair(left, right));
                let generalisation_candidates =
                    self.find_generalisations(ast.as_ref())?;
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
                        let reduction_result = context_search
                            .reduce_concept(&generalisation)
                            .map(|(ast, reason)| {
                                (
                                    context_search
                                        .substitute(&ast, &variable_mask),
                                    ReductionReason::<CCI, SR>::rule(
                                        self.to_ast(&generalisation),
                                        variable_mask.clone(),
                                        reason,
                                    ),
                                )
                            });
                        for item in
                            context_search.caches.reductions.head.as_ref()
                        {
                            let Some(c) = item.key().concept else {
                                continue;
                            };
                            let ast = self.to_ast(&c);
                            self.caches.insert_reduction(&ast, item.value());
                        }
                        reduction_result
                    })
            },
            (Some((left_ast, left_reason)), None) => Some((
                self.contract_pair(&left_ast, maybe_subbed_r.unwrap_or(right))
                    .share(),
                ReductionReason::<CCI, SR>::partial(
                    hashmap! {left.key() => (left_ast, left_reason)},
                ),
            )),
            (None, Some((right_ast, right_reason))) => Some((
                self.contract_pair(maybe_subbed_l.unwrap_or(left), &right_ast)
                    .share(),
                ReductionReason::<CCI, SR>::partial(
                    hashmap! {right.key() => (right_ast, right_reason)},
                ),
            )),
            (
                Some((left_ast, left_reason)),
                Some((right_ast, right_reason)),
            ) => Some((
                self.contract_pair(&left_ast, &right_ast).share(),
                ReductionReason::<CCI, SR>::partial(hashmap! {
                    left.key() => (left_ast, left_reason),
                    right.key() => (right_ast, right_reason)
                }),
            )),
        }
    }

    pub fn substitute(
        &self,
        ast: &SharedSyntax<CCI, SR>,
        variable_mask: &VariableMask<CCI, SR>,
    ) -> SharedSyntax<CCI, SR> {
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
        lefthand: &SharedSyntax<CCI, SR>,
        righthand: &SharedSyntax<CCI, SR>,
    ) -> GenericSyntaxTree<CCI, SR> {
        lefthand
            .get_concept()
            .and_also(&righthand.get_concept())
            .and_then(|(lc, rc)| {
                let left_concept =
                    self.snap_shot.read_concept(self.delta.as_ref(), *lc);
                left_concept
                    .find_as_hand_in_composition_with(*rc, Hand::Left)
                    .map(|def| {
                        let syntax = GenericSyntaxTree::<CCI, SR>::from(
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

    fn find_generalisations<'a>(
        &'a self,
        ast: &GenericSyntaxTree<CCI, SR>,
    ) -> Option<impl Iterator<Item = S::ConceptId> + 'a> {
        GeneralisationFinder::<'a, S, CCI, SR>::new(
            ast,
            self.snap_shot,
            self.delta.clone(),
            SR::share(DashSet::default()),
        )
    }

    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(
        &self,
        ast: &SharedSyntax<CCI, SR>,
    ) -> (SharedSyntax<CCI, SR>, Option<ReductionReason<CCI, SR>>) {
        debug!("recursively_reduce({})", ast.as_ref());
        let mut maybe_reason: Option<ReductionReason<CCI, SR>> = None;
        let mut reduced_ast = ast.clone();
        while let Some((a, reason)) = self.reduce(&reduced_ast) {
            maybe_reason = Some(ReductionReason::<CCI, SR>::recursive_reason(
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
        leftleft: &SharedSyntax<CCI, SR>,
        leftright: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
    ) -> ReductionResult<CCI, SR> {
        debug!(
            "reduce_by_expanded_left_branch({}, {}, {})",
            leftleft.as_ref(),
            leftright.as_ref(),
            right.as_ref()
        );
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
                        ReductionReason::<CCI, SR>::existence(
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
        generalisation: &SharedSyntax<CCI, SR>,
        truths: impl Iterator<Item = S::ConceptId>,
    ) -> Option<ExampleSubstitutions<CCI, SR>> {
        debug!("find_example({})", generalisation.as_ref());
        self.find_examples(generalisation.clone(), truths).next()
    }

    #[allow(clippy::too_many_lines)]
    pub fn find_examples_of_inferred_reduction(
        &self,
        ast_to_reduce: &SharedSyntax<CCI, SR>,
    ) -> ReductionResult<CCI, SR> {
        debug!(
            "find_examples_of_inferred_reduction({})",
            ast_to_reduce.as_ref()
        );
        let implication_id =
            self.concrete_concept_id(ConcreteConceptType::Implication)?;
        let reduction_operator =
            self.concrete_ast(ConcreteConceptType::Reduction)?;
        let mut spawned_delta = NestedDelta::spawn(self.delta.clone());
        let variable_reduction_id = spawned_delta
            .insert_delta_for_new_concept(NewConceptDelta::BoundVariable);
        let variable_reduction_syntax =
            GenericSyntaxTree::<CCI, SR>::new_leaf_variable(
                variable_reduction_id,
            )
            .share();
        let variable_condition_id = spawned_delta
            .insert_delta_for_new_concept(NewConceptDelta::BoundVariable);
        let variable_condition_syntax =
            GenericSyntaxTree::<CCI, SR>::new_leaf_variable(
                variable_condition_id,
            )
            .share();
        let variable_result_id = spawned_delta
            .insert_delta_for_new_concept(NewConceptDelta::BoundVariable);
        let _variable_result_syntax =
            GenericSyntaxTree::<CCI, SR>::new_leaf_variable(variable_result_id)
                .share();
        let cache = SR::share(ReductionCache::<CCI, SR>::default());
        let mut spawned_context_search =
            self.spawn(&cache, SR::share(spawned_delta));
        if let Some(concept) = ast_to_reduce.get_concept() {
            spawned_context_search.concept_inferring.insert(concept);
        }
        let implication_syntax = spawned_context_search.to_ast(&implication_id);
        let implication_rule_fn =
            |condition: &SharedSyntax<CCI, SR>,
             prereduction: &SharedSyntax<CCI, SR>,
             reduction: &SharedSyntax<CCI, SR>| {
                GenericSyntaxTree::<CCI, SR>::new_pair(
                    condition.clone(),
                    GenericSyntaxTree::<CCI, SR>::new_pair(
                        implication_syntax.clone(),
                        GenericSyntaxTree::<CCI, SR>::new_pair(
                            prereduction.clone(),
                            GenericSyntaxTree::<CCI, SR>::new_pair(
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
                    .get(&variable_condition_syntax.key())
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
                            .get(&variable_reduction_syntax.key())
                            .map(|result| {
                                (
                                    self.substitute(
                                        result,
                                        &substitutions.example,
                                    ),
                                    ReductionReason::<CCI, SR>::inference(
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
        generalisation: SharedSyntax<CCI, SR>,
        equivalence_set: impl Iterator<Item = S::ConceptId> + 'a, /* All concepts that are equal to generalisation */
    ) -> impl Iterator<Item = ExampleSubstitutions<CCI, SR>> + 'a {
        debug!("find_examples({})", generalisation.as_ref());
        let iterator: Box<dyn Iterator<Item = ExampleSubstitutions<CCI, SR>>>;
        if let Some((left, right)) = generalisation.get_expansion() {
            iterator = Box::new(self.find_examples_of_branched_generalisation(
                left,
                right,
                equivalence_set,
            ));
        } else {
            debug_assert!(
                self.contains_bound_variable_syntax(&generalisation),
                "Generalisation ({}) doesn't contain bound variables",
                generalisation.as_ref()
            );
            iterator = Box::new(equivalence_set.map(move |c| {
                let example = self.to_ast(&c);
                ExampleSubstitutions {
                    generalisation: hashmap! {generalisation.key() => example},
                    example: hashmap! {},
                }
            }));
        }
        iterator
    }

    #[allow(clippy::too_many_lines)]
    fn find_examples_of_branched_generalisation<'a>(
        &'a self,
        left: SharedSyntax<CCI, SR>,
        right: SharedSyntax<CCI, SR>,
        equivalence_set: impl Iterator<Item = S::ConceptId> + 'a,
    ) -> impl Iterator<Item = ExampleSubstitutions<CCI, SR>> + 'a {
        let iterator: Box<dyn Iterator<Item = ExampleSubstitutions<CCI, SR>>> =
            match (
                self.contains_bound_variable_syntax(&left),
                self.contains_bound_variable_syntax(&right),
            ) {
                (true, true) => {
                    Box::new(
                        equivalence_set
                            .filter_map(|equivalent_concept_id| {
                                self.composition_of_concept(
                                    &equivalent_concept_id,
                                )
                            })
                            .filter_map(
                                move |(
                                    equivalent_left_id,
                                    equivalent_right_id,
                                )| {
                                    let equivalent_concept =
                                        self.snap_shot.read_concept(
                                            self.delta.as_ref(),
                                            equivalent_left_id,
                                        );
                                    // TODO handle case when a concept implicitly reduces to `equivalent_concept`
                                    let equivalent_left_equivalence_set =
                                        equivalent_concept
                                            .find_what_reduces_to_it()
                                            .chain(iter::once(
                                                equivalent_left_id,
                                            ));
                                    // TODO try to find a case where this needs to be a flat_map method call
                                    let maybe_example = self
                                    .find_examples(
                                        left.clone(),
                                        equivalent_left_equivalence_set,
                                    )
                                    .find_map(|left_example| {
                                        let mut right_clone = right.clone();
                                        let mutable_right = GenericSyntaxTree::<
                                            CCI,
                                            SR,
                                        >::make_mut(
                                            &mut right_clone
                                        );
                                        substitute::<CCI, SR>(
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
                                            .is_some_and(|id| {
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
                        &left,
                        right.clone(),
                        equivalence_set,
                        Hand::Right,
                    )
                    .into_iter(),
                ),
                (false, true) => Box::new(
                    self.find_examples_of_half_generalisation(
                        &right,
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
        generalised_part: &SharedSyntax<CCI, SR>,
        non_generalised_part: SharedSyntax<CCI, SR>,
        mut equivalence_set_of_composition: impl Iterator<Item = S::ConceptId> + 'a,
        non_generalised_hand: Hand,
    ) -> Option<ExampleSubstitutions<CCI, SR>> {
        debug!("find_examples_of_half_generalisation({}, {}, {non_generalised_hand:?})", generalised_part.as_ref(), non_generalised_part.as_ref());
        // TODO try to test if this needs to be a flat_map call
        equivalence_set_of_composition.find_map(move |equivalent_concept_id| {
            self.find_example_of_half_generalisation(
                generalised_part,
                &non_generalised_part,
                equivalent_concept_id,
                non_generalised_hand,
                |example_hand| {
                    // TODO handle case when a concept implicitly reduces to `non_generalised_hand`
                    let equivalence_set = iter::once(example_hand);
                    let non_generalised_hand_concept = self
                        .snap_shot
                        .read_concept(self.delta.as_ref(), example_hand);
                    self.find_example(
                        generalised_part,
                        equivalence_set.chain(
                            non_generalised_hand_concept
                                .find_what_reduces_to_it(),
                        ),
                    )
                },
            )
        })
    }

    fn find_example_of_half_generalisation(
        &self,
        generalised_part_clone: &SharedSyntax<CCI, SR>,
        non_generalised_part: &SharedSyntax<CCI, SR>,
        equivalent_concept_id: S::ConceptId,
        non_generalised_hand: Hand,
        or_else: impl FnOnce(S::ConceptId) -> Option<ExampleSubstitutions<CCI, SR>>,
    ) -> Option<ExampleSubstitutions<CCI, SR>> {
        // TODO cache this calculation
        let equivalent_concept = self
            .snap_shot
            .read_concept(self.delta.as_ref(), equivalent_concept_id);
        let (left, right) = equivalent_concept.get_composition()?;
        let (equivalent_non_generalised_hand, equivalent_generalised_hand) =
            match non_generalised_hand {
                Hand::Left => (left, right),
                Hand::Right => (right, left),
            };
        if Some(equivalent_non_generalised_hand)
            != non_generalised_part.get_concept()
        {
            if self
                .snap_shot
                .read_concept(
                    self.delta.as_ref(),
                    equivalent_non_generalised_hand,
                )
                .free_variable()
            {
                return self.find_example(generalised_part_clone, iter::once(equivalent_generalised_hand)).and_then(|subs| {
                        // Could have a more efficient method for this
                        subs.consistent_merge(ExampleSubstitutions{example: hashmap!{equivalent_non_generalised_hand => non_generalised_part.clone()}, ..Default::default()})
                    });
            }
            return None;
        }
        self.find_example(
            generalised_part_clone,
            iter::once(equivalent_generalised_hand),
        )
        .or_else(|| {
            let non_generalised_id = non_generalised_part.get_concept()?;
            let example_hand = match non_generalised_hand {
                Hand::Left => (left == non_generalised_id).then_some(right)?,
                Hand::Right => (right == non_generalised_id).then_some(left)?,
            };
            let example_hand_syntax = self.to_ast(&example_hand);
            GenericSyntaxTree::<CCI, SR>::check_example(
                &example_hand_syntax,
                generalised_part_clone,
            )
            .or_else(|| or_else(example_hand))
        })
    }

    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        left: &SharedSyntax<CCI, SR>,
        rightleft: &SharedSyntax<CCI, SR>,
        rightright: &SharedSyntax<CCI, SR>,
    ) -> ReductionResult<CCI, SR> {
        debug!(
            "reduce_by_expanded_right_branch({}, {}, {})",
            left.as_ref(),
            rightleft.as_ref(),
            rightright.as_ref()
        );
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
                let (x, reason) =
                    ReductionReason::<CCI, SR>::determine_reduction_truth(
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
    ) -> SharedSyntax<CCI, SR> {
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
                        GenericSyntaxTree::<CCI, SR>::from(s),
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
        ast: &SharedSyntax<CCI, SR>,
        other: &SharedSyntax<CCI, SR>,
    ) -> GenericSyntaxTree<CCI, SR> {
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
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
    ) -> GenericSyntaxTree<CCI, SR> {
        GenericSyntaxTree::<CCI, SR>::from(self.display_joint(left, right))
            .bind_pair(left.clone(), right.clone())
    }

    fn display_joint(
        &self,
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
    ) -> String {
        // TODO: find a better way of checking that a syntax tree does not a have a labeled root concept
        let mut left_string = left.to_string();
        if left_string.chars().any(char::is_whitespace) {
            left_string = left.get_expansion().map_or_else(
                || left.to_string(),
                |(l, r)| {
                    self.get_associativity(&r)
                        .display_joint_left(l.as_ref(), r.as_ref())
                },
            );
        }
        let mut right_string = right.to_string();
        if right_string.chars().any(char::is_whitespace) {
            right_string =
                right.get_expansion().map_or(right_string, |(l, r)| {
                    self.get_associativity(&l)
                        .display_joint_right(l.as_ref(), r.as_ref())
                });
        }
        left_string + " " + &right_string
    }

    pub fn get_associativity(
        &self,
        ast: &SharedSyntax<CCI, SR>,
    ) -> Associativity {
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
    pub fn expand(&self, ast: &SharedSyntax<CCI, SR>) -> SharedSyntax<CCI, SR> {
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
        some_syntax: &SharedSyntax<CCI, SR>,
        another_syntax: &SharedSyntax<CCI, SR>,
    ) -> (Comparison, ComparisonReason<CCI, SR>) {
        if some_syntax.key() == another_syntax.key() {
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
            let cache = SR::share(ReductionCache::<CCI,SR>::default());
            (
                match (
                    if self.syntax_evaluating.contains(&comparing_syntax.key()) {
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
                        .insert(comparing_syntax.key());
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
                        .contains(&comparing_reversed_syntax.key())
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
                        .insert(comparing_reversed_syntax.key());
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
                        panic!("{:#?} is both greater than and less than {:#?}!\nReason: {reason:#?}\n Reversed reason: {reversed_reason:#?}", some_syntax.as_ref(), another_syntax.as_ref());
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
                ReductionReason::<CCI,SR>::simplify_reasoning(reason, reversed_reason),
            )
        })
    }

    pub fn spawn<'b, 'c>(
        &'b self,
        cache: &'c SR::Share<ReductionCache<CCI, SR>>,
        delta: SR::Share<NestedDelta<CCI, SR>>,
    ) -> Self {
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
        ast: &SharedSyntax<CCI, SR>,
        generalisation: &S::ConceptId,
    ) -> Option<VariableMask<CCI, SR>> {
        (self.is_free_variable(generalisation)
            && !self.bound_variable_syntax.contains(&ast.key())
            && !ast.get_concept().is_some_and(|c| {
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
                                    if rmv.key() != lmv.key() {
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
                .map_or(true, |vml| vml.get(*v).is_none())
        // A hack required to prevent stack overflow
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
        ast: &SharedSyntax<CCI, SR>,
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
        variable_mask: VariableMask<CCI, SR>,
    ) -> Result<(), ()> {
        self.variable_mask = SR::share(
            VariableMaskList::push(&self.variable_mask, variable_mask)
                .ok_or(())?,
        );
        Ok(())
    }

    // TODO: move to separate struct with access to self.snap_shot, self.bound_variable_syntax, self.delta and self.caches
    fn contains_bound_variable_syntax(
        &self,
        syntax: &SharedSyntax<CCI, SR>,
    ) -> bool {
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
                self.bound_variable_syntax.contains(&syntax.key())
                    || syntax.get_concept().is_some_and(|c| {
                        self.snap_shot
                            .read_concept(self.delta.as_ref(), c)
                            .bounded_variable()
                    })
            },
        )
    }
}

struct GeneralisationFinder<
    'a,
    S: SnapShotReader<SR, ConceptId = CI>,
    CI: ConceptId + 'static,
    SR: SharedReference,
> {
    snap_shot: &'a S,
    generalisations: SR::Share<DashSet<CI>>,
    delta: SR::Share<NestedDelta<CI, SR>>,
    concepts_used: bool,
    composition_id_iter: Box<dyn Iterator<Item = CI>>,
    left_syntax: SR::Share<GenericSyntaxTree<CI, SR>>,
    right_syntax: SR::Share<GenericSyntaxTree<CI, SR>>,
    left_finder: Option<Box<Self>>,
    right_finder: Option<Box<Self>>,
}

impl<
        'a,
        S: SnapShotReader<SR, ConceptId = CI>,
        CI: ConceptId + 'static,
        SR: SharedReference,
    > GeneralisationFinder<'a, S, CI, SR>
{
    fn new(
        ast: &GenericSyntaxTree<CI, SR>,
        snap_shot: &'a S,
        delta: SR::Share<NestedDelta<CI, SR>>,
        generalisations: SR::Share<DashSet<CI>>,
    ) -> Option<Self> {
        ast.get_expansion().map(move |(l, r)| {
            let left_finder = GeneralisationFinder::new(
                l.as_ref(),
                snap_shot,
                delta.clone(),
                generalisations.clone(),
            )
            .map(Box::new);
            let right_finder = GeneralisationFinder::new(
                r.as_ref(),
                snap_shot,
                delta.clone(),
                generalisations.clone(),
            )
            .map(Box::new);
            Self {
                generalisations,
                snap_shot,
                delta,
                composition_id_iter: Box::new(empty()),
                left_syntax: l,
                right_syntax: r,
                left_finder,
                right_finder,
                concepts_used: false,
            }
        })
    }
}

impl<
        'a,
        S: SnapShotReader<SR, ConceptId = CI>,
        CI: ConceptId + 'static,
        SR: SharedReference,
    > Iterator for GeneralisationFinder<'a, S, CI, SR>
{
    type Item = CI;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            for id in self.composition_id_iter.by_ref() {
                if self.generalisations.insert(id) {
                    return Some(id);
                }
            }

            // Need to replenish composition_id_iter
            self.composition_id_iter = match (
                if self.concepts_used {
                    None
                } else {
                    self.left_syntax
                        .get_concept()
                        .map(|c| {
                            self.snap_shot.read_concept(self.delta.as_ref(), c)
                        })
                        .map(|l| l.into_iter_composition_ids(Hand::Left))
                },
                if self.concepts_used {
                    None
                } else {
                    self.right_syntax
                        .get_concept()
                        .map(|c| {
                            self.snap_shot.read_concept(self.delta.as_ref(), c)
                        })
                        .map(|r| r.into_iter_composition_ids(Hand::Right))
                },
            ) {
                (Some(iter), None) | (None, Some(iter)) => {
                    self.concepts_used = true;
                    Box::new(iter)
                },
                (Some(l_iter), Some(r_iter)) => {
                    self.concepts_used = true;
                    Box::new(l_iter.chain(r_iter))
                },
                (None, None) => {
                    self.concepts_used = true;
                    match (
                        self.left_finder.as_deref_mut().and_then(|finder| {
                            let g = finder.next()?;

                            let concept = self
                                .snap_shot
                                .read_concept(self.delta.as_ref(), g);
                            Some(concept.into_iter_composition_ids(Hand::Left))
                        }),
                        self.right_finder.as_mut().and_then(|finder| {
                            finder.next().map(|g| {
                                self.snap_shot
                                    .read_concept(self.delta.as_ref(), g)
                                    .into_iter_composition_ids(Hand::Right)
                            })
                        }),
                    ) {
                        (None, None) => return None,
                        (Some(iter), None) | (None, Some(iter)) => {
                            Box::new(iter)
                        },
                        (Some(l_iter), Some(r_iter)) => {
                            Box::new(l_iter.chain(r_iter))
                        },
                    }
                },
            };
        }
    }
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

#[derive(PartialEq, Debug)]
pub enum ComparisonReason<CI: ConceptId, SR: SharedReference> {
    SameSyntax,
    Reduction {
        reason: Option<ReductionReason<CI, SR>>,
        reversed_reason: Option<ReductionReason<CI, SR>>,
    },
    NoGreaterThanConcept,
}

impl<CI: ConceptId, SR: SharedReference> Eq for ComparisonReason<CI, SR> {}

impl<'c, 's, 'v, S, CCI: ConceptId, SR: SharedReference>
    From<ContextReferences<'c, 's, 'v, S, SR, CCI>>
    for ContextSearch<'s, 'v, S, CCI, SR>
where
    S: SnapShotReader<SR, ConceptId = CCI> + Sync + std::fmt::Debug,
{
    fn from(
        ContextReferences {
            snap_shot,
            delta,
            cache,
            bound_variable_syntax,
        }: ContextReferences<'c, 's, 'v, S, SR, CCI>,
    ) -> Self {
        Self {
            concept_inferring: HashSet::default(),
            bound_variable_syntax,
            snap_shot,
            variable_mask: SR::share(VariableMaskList::from(hashmap! {})),
            delta,
            caches: cache.clone(),
            syntax_evaluating: hashset! {},
            phantom: PhantomData,
            phantom2: PhantomData,
        }
    }
}

pub struct ContextReferences<'c, 's, 'v, S, SR: SharedReference, CCI: ConceptId>
{
    pub snap_shot: &'s S,
    pub delta: SR::Share<NestedDelta<CCI, SR>>,
    pub cache: &'c GenericCache<CCI, SR>,
    pub bound_variable_syntax: &'v HashSet<SyntaxKey<CCI>>,
}

pub type ReductionTruthResult<RR> = Option<(bool, RR)>;
