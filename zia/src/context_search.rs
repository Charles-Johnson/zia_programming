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
    concepts::{format_string, Concept, ConcreteConceptType, Hand},
    context_cache::ContextCache,
    context_delta::{ContextDelta, DirectConceptDelta},
    snap_shot::Reader as SnapShotReader,
};
use log::debug;
use maplit::{hashmap, hashset};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    sync::Arc,
};

pub type SharedSyntax<C> =
    <<C as ContextCache>::Syntax as SyntaxTree>::SharedSyntax;
#[derive(Debug)]
pub struct ContextSearch<'a, S, C, SDCD>
where
    S: SnapShotReader<SDCD>,
    C: ContextCache,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
{
    snap_shot: &'a S,
    variable_mask: Arc<VariableMaskList<C::Syntax>>,
    delta: &'a ContextDelta<S::ConceptId, SDCD>,
    caches: C,
    syntax_evaluating: HashSet<SharedSyntax<C>>,
    bound_variable_syntax: &'a HashSet<SharedSyntax<C>>,
}

pub type ReductionResult<Syntax> = Option<
    Reduction<
        <Syntax as SyntaxTree>::ConceptId,
        <Syntax as SyntaxTree>::SharedSyntax,
    >,
>;

type Reduction<ConceptId, SharedSyntax> =
    (SharedSyntax, ReductionReason<ConceptId, SharedSyntax>);

#[derive(Clone, PartialEq, Debug)]
pub enum ReductionReason<ConceptId: Eq + Hash, SharedSyntax: Eq + Hash> {
    Comparison(Arc<ComparisonReason<ConceptId, SharedSyntax>>),
    Explicit,
    Rule {
        generalisation: SharedSyntax,
        variable_mask: VariableMask<ConceptId, SharedSyntax>,
        reason: Arc<ReductionReason<ConceptId, SharedSyntax>>,
    },
    Inference {
        implication: SharedSyntax,
        reason: Arc<ReductionReason<ConceptId, SharedSyntax>>,
    },
    Default {
        operator: ConceptId,
    },
    Partial(HashMap<SharedSyntax, Reduction<ConceptId, SharedSyntax>>),
    Existence {
        substitutions: Substitutions<SharedSyntax>,
        generalisation: SharedSyntax,
    },
    Recursive {
        syntax: SharedSyntax,
        reason: Arc<ReductionReason<ConceptId, SharedSyntax>>,
        from: Arc<ReductionReason<ConceptId, SharedSyntax>>,
    },
    SyntaxCannotReduceToItself,
    LeftReducesToRight {
        reason: Arc<ReductionReason<ConceptId, SharedSyntax>>,
        left: SharedSyntax,
        right: SharedSyntax,
    },
    RightReducesToLeft {
        reason: Arc<ReductionReason<ConceptId, SharedSyntax>>,
        left: SharedSyntax,
        right: SharedSyntax,
    },
}

type Substitutions<SharedSyntax> = HashMap<SharedSyntax, SharedSyntax>;

fn substitute<Syntax: SyntaxTree>(
    syntax: &mut Syntax,
    substitutions: &Substitutions<Syntax::SharedSyntax>,
) {
    if let Some(substitution) = substitutions.get(syntax) {
        *syntax = substitution.as_ref().clone();
    } else if let Some((left, right)) = syntax.get_expansion_mut() {
        substitute(left, substitutions);
        substitute(right, substitutions);
    }
}

impl<'a, S, C, SDCD> ContextSearch<'a, S, C, SDCD>
where
    S: SnapShotReader<SDCD> + Sync + std::fmt::Debug,
    Self: Iteration<
        ConceptId = S::ConceptId,
        Syntax = <C as ContextCache>::Syntax,
    >,
    C: ContextCache,
    C::Syntax: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
{
    fn infer_reduction(
        &self,
        concept: &Concept<S::ConceptId>,
    ) -> ReductionResult<C::Syntax> {
        debug!("infer_reduction({:#?})", concept);
        let implication_id =
            self.concrete_concept_id(ConcreteConceptType::Implication)?;
        let composition_id = concept
            .find_as_righthand_in_composition_with_lefthand(implication_id)?;
        let composition_concept =
            self.snap_shot.read_concept(self.delta, composition_id);
        let result = composition_concept.get_righthand_of().iter().find_map(
            |(condition_id, implication_rule_id)| {
                let condition = self.to_ast(*condition_id);
                let (reduced_condition, reason) = self.reduce(&condition)?;
                let x = reduced_condition.get_concept()?;
                self.is_concrete_type(ConcreteConceptType::True, x).map(|x| {
                    (
                        self.to_ast(x),
                        ReductionReason::Inference {
                            implication: self.to_ast(*implication_rule_id),
                            reason: reason.into(),
                        },
                    )
                })
            },
        );
        result
    }

    fn is_concrete_type(
        &self,
        cct: ConcreteConceptType,
        concept_id: S::ConceptId,
    ) -> Option<S::ConceptId> {
        if Some(cct) == self.concrete_type(concept_id) {
            Some(concept_id)
        } else {
            None
        }
    }

    fn concrete_type(
        &self,
        concept_id: S::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.snap_shot.concrete_concept_type(self.delta, concept_id)
    }

    fn concrete_type_of_ast(
        &self,
        ast: &SharedSyntax<C>,
    ) -> Option<ConcreteConceptType> {
        ast.get_concept().and_then(|c| self.concrete_type(c))
    }

    fn concrete_concept_id(
        &self,
        cct: ConcreteConceptType,
    ) -> Option<S::ConceptId> {
        self.snap_shot.concrete_concept_id(self.delta, cct)
    }

    pub fn concrete_ast(
        &self,
        cct: ConcreteConceptType,
    ) -> Option<SharedSyntax<C>> {
        self.concrete_concept_id(cct).map(|id| self.to_ast(id))
    }

    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, id: S::ConceptId) -> ReductionResult<C::Syntax> {
        debug!("reduce_concept({})", id);
        let concept = self.snap_shot.read_concept(self.delta, id);
        self.infer_reduction(&concept).or_else(|| {
            let n = concept.get_reduction()?;
            if self.is_leaf_variable(n) {
                self.variable_mask.get(n).cloned()
            } else {
                Some(self.to_ast(n))
            }
            .map(|r| (r, ReductionReason::Explicit))
        })
    }

    // If (operator right)  cannot by trying be reduced by other means, then it should reduce to default_concept
    fn reduce_otherwise_default(
        &self,
        ast: &SharedSyntax<C>,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
        operator_id: S::ConceptId,
        default_concept_id: S::ConceptId,
    ) -> ReductionResult<C::Syntax> {
        let mut reduced_pair: ReductionResult<C::Syntax> = None;
        let mut operator_composition_check = || {
            let cache = <C as ContextCache>::SharedReductionCache::default();
            let mut context_search = self.spawn(&cache);
            context_search.syntax_evaluating.insert(ast.clone());
            reduced_pair = context_search.reduce_pair(left, right);
            let operator_concept =
                self.snap_shot.read_concept(self.delta, operator_id);
            let find = |c| {
                operator_concept
                    .find_as_lefthand_in_composition_with_righthand(c)
            };
            reduced_pair.is_none()
                && right.get_concept().and_then(find).is_none()
        };
        if self.syntax_evaluating.contains(ast) || operator_composition_check()
        {
            Some((
                self.to_ast(default_concept_id),
                ReductionReason::Default {
                    operator: operator_id,
                },
            ))
        } else {
            reduced_pair
        }
    }

    /// Reduces the syntax by using the reduction rules of associated concepts.
    pub fn reduce(&self, ast: &SharedSyntax<C>) -> ReductionResult<C::Syntax> {
        debug!("reduce({})", ast.to_string());
        self.caches.get_reduction_or_else(ast, || {
            let reduction_result = ast
                .get_concept()
                .and_then(|c| self.reduce_concept(c))
                .or_else(|| {
                    let (ref left, ref right) = ast.get_expansion()?;
                    left.get_concept()
                        .and_then(|lc| match self.concrete_type(lc) {
                            Some(ConcreteConceptType::Precedence) => {
                                let default_concept_id = self
                                    .concrete_concept_id(
                                        ConcreteConceptType::Default,
                                    )?;
                                self.reduce_otherwise_default(
                                    ast,
                                    left,
                                    right,
                                    lc,
                                    default_concept_id,
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
                                    lc,
                                    default_concept_id,
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
    ) -> ReductionResult<C::Syntax> {
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
    ) -> ReductionResult<C::Syntax> {
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
                    let mut context_search = self.spawn(&cache);
                    // Stack overflow occurs if you remove this
                    context_search
                        .insert_variable_mask(variable_mask.clone())
                        .ok()?;
                    context_search.reduce_concept(*generalisation).map(
                        |(ast, reason)| {
                            (
                                context_search.substitute(&ast, variable_mask),
                                ReductionReason::Rule {
                                    generalisation: self
                                        .to_ast(*generalisation),
                                    variable_mask: variable_mask.clone(),
                                    reason: Arc::new(reason),
                                },
                            )
                        },
                    )
                }),
            (Some((left_ast, left_reason)), None) => Some((
                self.contract_pair(&left_ast, maybe_subbed_r.unwrap_or(right))
                    .share(),
                if let ReductionReason::Partial(_) = &left_reason {
                    left_reason
                } else {
                    ReductionReason::Partial(
                        hashmap! {left.clone() => (left_ast, left_reason)},
                    )
                },
            )),
            (None, Some((right_ast, right_reason))) => Some((
                self.contract_pair(maybe_subbed_l.unwrap_or(left), &right_ast)
                    .share(),
                if let ReductionReason::Partial(_) = &right_reason {
                    right_reason
                } else {
                    ReductionReason::Partial(
                        hashmap! {right.clone() => (right_ast, right_reason)},
                    )
                },
            )),
            (
                Some((left_ast, left_reason)),
                Some((right_ast, right_reason)),
            ) => Some((
                self.contract_pair(&left_ast, &right_ast).share(),
                match (left_reason.clone(), right_reason.clone()) {
                    (
                        ReductionReason::Partial(mut l_hm),
                        ReductionReason::Partial(r_hm),
                    ) => {
                        l_hm.extend(r_hm);
                        ReductionReason::Partial(l_hm)
                    },
                    (ReductionReason::Partial(mut hm), _) => {
                        hm.insert(right.clone(), (right_ast, right_reason));
                        ReductionReason::Partial(hm)
                    },
                    (_, ReductionReason::Partial(mut hm)) => {
                        hm.insert(left.clone(), (left_ast, left_reason));
                        ReductionReason::Partial(hm)
                    },
                    _ => ReductionReason::Partial(hashmap! {
                        left.clone() => (left_ast, left_reason),
                        right.clone() => (right_ast, right_reason)
                    }),
                },
            )),
        }
    }

    pub fn substitute(
        &self,
        ast: &SharedSyntax<C>,
        variable_mask: &VariableMask<S::ConceptId, SharedSyntax<C>>,
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
    ) -> Generalisations<S::ConceptId, SharedSyntax<C>> {
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
    ) -> C::Syntax {
        lefthand
            .get_concept()
            .and_also(&righthand.get_concept())
            .and_then(|(lc, rc)| {
                let left_concept = self.snap_shot.read_concept(self.delta, *lc);
                left_concept
                    .find_as_lefthand_in_composition_with_righthand(*rc)
                    .map(|def| {
                        let syntax = C::Syntax::from(
                            self.snap_shot
                                .get_label(self.delta, def)
                                .unwrap_or_else(|| {
                                    self.display_joint(lefthand, righthand)
                                }),
                        );
                        self.snap_shot
                            .bind_concept_to_syntax(self.delta, syntax, def)
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
        generalisation: S::ConceptId,
    ) -> Option<VariableMask<S::ConceptId, SharedSyntax<C>>> {
        (self.is_free_variable(generalisation)
            && !self.bound_variable_syntax.contains(ast)
            && !ast.get_concept().map_or(false, |c| {
                self.snap_shot.read_concept(self.delta, c).bounded_variable()
            }))
        .then(|| {
            if let Some((gl, gr)) = self
                .snap_shot
                .read_concept(self.delta, generalisation)
                .get_composition()
            {
                let (l, r) = ast.get_expansion()?;
                match (self.is_free_variable(gl), self.is_free_variable(gr)) {
                    (true, true) => self
                        .check_generalisation(&l, gl)
                        .and_also_move(self.check_generalisation(&r, gr))
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
                        self.check_generalisation(&l, gl)
                    },
                    (false, true) if l.get_concept() == Some(gl) => {
                        self.check_generalisation(&r, gr)
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
                Some(hashmap! {generalisation => ast.clone()})
            }
        })
        .flatten()
    }

    fn find_generalisations(&self, ast: &C::Syntax) -> HashSet<S::ConceptId> {
        let mut generalisations = HashSet::<S::ConceptId>::new();
        if let Some((l, r)) = ast.get_expansion() {
            if let Some(c) = l.get_concept() {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, c)
                        .get_lefthand_of()
                        .values(),
                );
            }
            if let Some(c) = r.get_concept() {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, c)
                        .get_righthand_of()
                        .values(),
                );
            }
            self.find_generalisations(&l).iter().for_each(|g| {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, *g)
                        .get_lefthand_of()
                        .values(),
                )
            });
            self.find_generalisations(&r).iter().for_each(|g| {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, *g)
                        .get_righthand_of()
                        .values(),
                )
            });
            generalisations
        } else {
            generalisations
        }
    }

    fn is_leaf_variable(&self, lv: S::ConceptId) -> bool {
        self.is_free_variable(lv) && self.is_leaf_concept(lv)
    }

    fn is_free_variable(&self, v: S::ConceptId) -> bool {
        self.snap_shot.read_concept(self.delta, v).free_variable()
            && self
                .variable_mask
                .tail
                .as_ref()
                .map_or(true, |vml| vml.get(v).is_none()) // A hack required to prevent stack overflow
    }

    fn is_leaf_concept(&self, l: S::ConceptId) -> bool {
        self.snap_shot.read_concept(self.delta, l).get_composition().is_none()
    }

    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(
        &self,
        ast: &SharedSyntax<C>,
    ) -> MaybeReducedSyntaxWithReason<S::ConceptId, SharedSyntax<C>> {
        debug!("recursively_reduce({})", ast);
        let mut maybe_reason: Option<
            ReductionReason<S::ConceptId, SharedSyntax<C>>,
        > = None;
        let mut reduced_ast = ast.clone();
        while let Some((ref a, ref reason)) = self.reduce(&reduced_ast) {
            maybe_reason = Some(maybe_reason.map_or(reason.clone(), |from| {
                ReductionReason::Recursive {
                    from: from.into(),
                    reason: reason.clone().into(),
                    syntax: reduced_ast,
                }
            }));
            reduced_ast = a.clone();
        }
        (reduced_ast, maybe_reason)
    }

    fn reduce_by_expanded_left_branch(
        &self,
        leftleft: &SharedSyntax<C>,
        leftright: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ReductionResult<C::Syntax> {
        let cct = self.concrete_type_of_ast(leftright)?;
        match cct {
            ConcreteConceptType::ExistsSuchThat
                if leftleft.is_leaf_variable() =>
            {
                let true_id = self
                    .concrete_concept_id(ConcreteConceptType::True)
                    .expect("true concept must exist");
                let true_concept =
                    self.snap_shot.read_concept(self.delta, true_id);
                let truths = true_concept.find_what_reduces_to_it();
                self.find_example(right, &truths.copied().collect()).map(
                    |substitutions| {
                        let true_syntax = self.to_ast(true_id);
                        (
                            true_syntax,
                            ReductionReason::Existence {
                                substitutions,
                                generalisation: right.clone(),
                            },
                        )
                    },
                )
            }
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
    fn find_examples(
        &self,
        // needs to contain bounded variables
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
                            .read_concept(self.delta, *equivalent_concept_id);
                        equivalent_concept.get_composition()
                    }).flat_map(|(equivalent_left_id, equivalent_right_id)| {
                        let equivalent_left = self
                            .snap_shot
                            .read_concept(self.delta, equivalent_left_id);
                        // TODO handle case when a concept implicitly reduces to `equivalent_left`
                        let mut equivalent_left_equivalence_set: HashSet<S::ConceptId> =
                            equivalent_left
                                .find_what_reduces_to_it()
                                .copied()
                                .collect();
                        equivalent_left_equivalence_set.insert(equivalent_left_id);
                        let left_examples = self.find_examples(
                            &left,
                            &equivalent_left_equivalence_set,
                        );
                        let equivalent_right = self
                            .snap_shot
                            .read_concept(self.delta, equivalent_right_id);
                        // TODO handle case when a concept implicitly reduces to `equivalent_right`
                        let mut equivalent_right_equivalence_set: HashSet<
                            S::ConceptId,
                        > = equivalent_right
                            .find_what_reduces_to_it()
                            .copied()
                            .collect();
                        equivalent_right_equivalence_set
                            .insert(equivalent_right_id);
                        let right_examples = self.find_examples(
                            &right,
                            &equivalent_right_equivalence_set,
                        );
                        left_examples.into_iter().flat_map(|left_example| {
                            let mut right_clone = right.clone();
                            let mut mutable_right = C::Syntax::make_mut(&mut right_clone);
                            substitute::<C::Syntax>(&mut mutable_right, &left_example);
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
                            let mut mutable_left = C::Syntax::make_mut(&mut left_clone);
                            substitute::<C::Syntax>(&mut mutable_left, &right_example);
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
                    let example = self.to_ast(*c);
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
        if let Some(non_generalised_id) = non_generalised_part.get_concept() {
            let non_generalised_concept =
                self.snap_shot.read_concept(self.delta, non_generalised_id);
            let examples =
                non_generalised_concept.get_hand_of(non_generalised_hand);
            examples
                .iter()
                .filter_map(|(generalised_hand, composition)| {
                    equivalence_set_of_composition
                        .contains(composition)
                        .then(|| *generalised_hand)
                })
                .filter_map(|generalised_hand| {
                    C::Syntax::check_example(
                        &self.to_ast(generalised_hand),
                        generalisated_part,
                    )
                    .or_else(|| {
                        // TODO handle case when a concept implicitly reduces to `non_generalised_hand`
                        let mut equivalence_set = hashset! {generalised_hand};
                        let non_generalised_hand_concept = self
                            .snap_shot
                            .read_concept(self.delta, generalised_hand);
                        equivalence_set.extend(
                            non_generalised_hand_concept
                                .find_what_reduces_to_it(),
                        );
                        self.find_example(generalisated_part, &equivalence_set)
                    })
                })
                .collect()
        } else {
            vec![]
        }
    }

    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        left: &SharedSyntax<C>,
        rightleft: &SharedSyntax<C>,
        rightright: &SharedSyntax<C>,
    ) -> ReductionResult<C::Syntax> {
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
                .map(|ast| {
                    (
                        ast,
                        ReductionReason::Comparison(Arc::new(
                            comparison_reason,
                        )),
                    )
                })
            },
            ConcreteConceptType::Reduction => {
                let (x, reason) =
                    self.determine_reduction_truth(left, rightright)?;
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

    fn determine_reduction_truth(
        &self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ReductionTruthResult<S::ConceptId, SharedSyntax<C>> {
        if left == right {
            Some((false, ReductionReason::SyntaxCannotReduceToItself))
        } else {
            match (
                self.determine_evidence_of_reduction(left, right),
                self.determine_evidence_of_reduction(right, left),
            ) {
                (Some(_), Some(_)) => panic!(
                    "{:#?} and {:#?} reduce to each other?!",
                    left, right
                ),
                (Some(reason), None) => Some((
                    true,
                    ReductionReason::LeftReducesToRight {
                        reason: reason.into(),
                        left: left.clone(),
                        right: right.clone(),
                    },
                )),
                (None, Some(reason)) => Some((
                    false,
                    ReductionReason::RightReducesToLeft {
                        reason: reason.into(),
                        left: left.clone(),
                        right: right.clone(),
                    },
                )),
                (None, None) => None,
            }
        }
    }

    fn determine_evidence_of_reduction(
        &self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> Option<ReductionReason<S::ConceptId, SharedSyntax<C>>> {
        self.reduce(left).and_then(|(reduced_left, reason)| {
            if &reduced_left == right {
                Some(reason)
            } else {
                self.determine_evidence_of_reduction(&reduced_left, right).map(
                    |new_reason| ReductionReason::Recursive {
                        syntax: left.clone(),
                        reason: new_reason.into(),
                        from: reason.into(),
                    },
                )
            }
        })
    }

    /// Returns the syntax for a concept. Panics if there is no concept with the given `concept_id`
    pub fn to_ast(&self, concept_id: S::ConceptId) -> SharedSyntax<C> {
        self.variable_mask.get(concept_id).cloned().unwrap_or_else(|| {
            self.caches.get_syntax_tree_or_else(concept_id, || {
                let concept =
                    self.snap_shot.read_concept(self.delta, concept_id);
                let syntax = if let Some(s) = concept.get_string().map_or_else(
                    || self.snap_shot.get_label(self.delta, concept_id),
                    |s| Some(format_string(&s)),
                ) {
                    self.snap_shot
                        .bind_concept_to_syntax(
                            self.delta,
                            C::Syntax::from(s),
                            concept_id,
                        )
                } else if let Some((left, right)) = concept.get_composition() {
                    self.combine(&self.to_ast(left), &self.to_ast(right))
                } else {
                    self.snap_shot
                        .new_syntax_from_concept_that_has_no_label_or_composition(&concept)
                }.share();
                self.caches.insert_syntax_tree(&concept, &syntax);
                syntax
            })
        })
    }

    pub fn combine(
        &self,
        ast: &SharedSyntax<C>,
        other: &SharedSyntax<C>,
    ) -> C::Syntax {
        ast.get_concept()
            .and_also(&other.get_concept())
            .and_then(|(l, r)| {
                let left_concept = self.snap_shot.read_concept(self.delta, *l);
                left_concept
                    .find_as_lefthand_in_composition_with_righthand(*r)
                    .map(|concept| {
                        let syntax = self.join(ast, other);
                        self.snap_shot
                            .bind_concept_to_syntax(self.delta, syntax, concept)
                    })
            })
            .unwrap_or_else(|| self.join(ast, other))
    }

    fn join(
        &self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> C::Syntax {
        C::Syntax::from(self.display_joint(left, right))
            .bind_pair(left.clone(), right.clone())
    }

    fn display_joint(
        &self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> String {
        let left_string = left.get_expansion().map_or_else(
            || left.to_string(),
            |(l, r)| self.get_associativity(&r).display_joint_left(l, r),
        );
        let right_string = right.get_expansion().map_or_else(
            || right.to_string(),
            |(l, r)| self.get_associativity(&l).display_joint_right(l, r),
        );
        left_string + " " + &right_string
    }

    pub fn get_associativity(&self, ast: &SharedSyntax<C>) -> Associativity {
        self.concrete_concept_id(ConcreteConceptType::Associativity).map_or(
            Associativity::Right,
            |associativity_concept_id| {
                let assoc_of_ast = self
                    .combine(&self.to_ast(associativity_concept_id), ast)
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
        if let Some(con) = ast.get_concept() {
            if let Some((left, right)) =
                self.snap_shot.read_concept(self.delta, con).get_composition()
            {
                self.combine(
                    &self.expand(&self.to_ast(left)),
                    &self.expand(&self.to_ast(right)),
                )
                .share()
            } else {
                self.to_ast(con)
            }
        } else if let Some((ref left, ref right)) = ast.get_expansion() {
            self.combine(&self.expand(left), &self.expand(right)).share()
        } else {
            ast.clone()
        }
    }

    #[allow(clippy::clippy::too_many_lines)]
    pub fn compare(
        &self,
        some_syntax: &SharedSyntax<C>,
        another_syntax: &SharedSyntax<C>,
    ) -> (Comparison, ComparisonReason<S::ConceptId, SharedSyntax<C>>) {
        if some_syntax == another_syntax {
            return (Comparison::EqualTo, ComparisonReason::SameSyntax);
        }
        if let Some(greater_than_concept_id) =
            self.concrete_concept_id(ConcreteConceptType::GreaterThan)
        {
            let greater_than_syntax = self.to_ast(greater_than_concept_id);
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
                        let mut context_search = self.spawn(&cache);
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
                        let mut context_search = self.spawn(&cache);
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
                simplify_reasoning(reason, reversed_reason),
            )
        } else {
            (Comparison::Incomparable, ComparisonReason::NoGreaterThanConcept)
        }
    }

    pub fn spawn(
        &self,
        cache: &<C as ContextCache>::SharedReductionCache,
    ) -> Self {
        Self {
            bound_variable_syntax: self.bound_variable_syntax,
            caches: self.caches.spawn(cache),
            delta: self.delta,
            snap_shot: self.snap_shot,
            syntax_evaluating: self.syntax_evaluating.clone(),
            variable_mask: self.variable_mask.clone(),
        }
    }

    /// Error if `variable_mask` is already included
    fn insert_variable_mask(
        &mut self,
        variable_mask: VariableMask<S::ConceptId, SharedSyntax<C>>,
    ) -> Result<(), ()> {
        self.variable_mask = Arc::new(
            VariableMaskList::push(&self.variable_mask, variable_mask)
                .ok_or(())?,
        );
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
                            .read_concept(self.delta, c)
                            .bounded_variable()
                    })
            },
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Example<Syntax: SyntaxTree> {
    generalisation: Syntax::SharedSyntax,
    substitutions: HashMap<Syntax::SharedSyntax, Match<Syntax>>,
}

type MaybeReducedSyntaxWithReason<ConceptId, SharedSyntax> =
    (SharedSyntax, Option<ReductionReason<ConceptId, SharedSyntax>>);

#[derive(Clone, Debug, PartialEq)]
pub struct Match<Syntax: SyntaxTree> {
    value: Syntax::SharedSyntax,
    reduction: Syntax::SharedSyntax,
    reason: ReductionReason<Syntax::ConceptId, Syntax::SharedSyntax>,
}

fn simplify_reasoning<
    ConceptId: Eq + Hash + Clone,
    SharedSyntax: Clone + Eq + Hash,
>(
    reason: Option<ReductionReason<ConceptId, SharedSyntax>>,
    reversed_reason: Option<ReductionReason<ConceptId, SharedSyntax>>,
) -> ComparisonReason<ConceptId, SharedSyntax> {
    match (&reason, &reversed_reason) {
        (Some(ReductionReason::Comparison(cr)), rr) => match cr.as_ref() {
            ComparisonReason::Reduction {
                reason: comparsion_reason,
                reversed_reason: reversed_comparison_reason,
            } if rr == reversed_comparison_reason => {
                ComparisonReason::Reduction {
                    reversed_reason: reversed_comparison_reason.clone(),
                    reason: comparsion_reason.clone(),
                }
            },
            _ => ComparisonReason::Reduction {
                reason,
                reversed_reason,
            },
        },
        (r, Some(ReductionReason::Comparison(cr))) => match cr.as_ref() {
            ComparisonReason::Reduction {
                reason: reversed_comparison_reason,
                reversed_reason: comparison_reason,
            } if r == comparison_reason => ComparisonReason::Reduction {
                reversed_reason: reversed_comparison_reason.clone(),
                reason: comparison_reason.clone(),
            },
            _ => ComparisonReason::Reduction {
                reason,
                reversed_reason,
            },
        },
        _ => ComparisonReason::Reduction {
            reason,
            reversed_reason,
        },
    }
}

#[derive(PartialEq, Debug)]
pub enum Comparison {
    GreaterThan,
    LessThan,
    Incomparable,
    EqualTo,
    GreaterThanOrEqualTo,
    LessThanOrEqualTo,
}

#[derive(PartialEq, Debug)]
pub enum ComparisonReason<ConceptId: Eq + Hash, SharedSyntax: Eq + Hash> {
    SameSyntax,
    Reduction {
        reason: Option<ReductionReason<ConceptId, SharedSyntax>>,
        reversed_reason: Option<ReductionReason<ConceptId, SharedSyntax>>,
    },
    NoGreaterThanConcept,
}

impl<'a, S, C, SDCD> From<ContextReferences<'a, S, C, SDCD>>
    for ContextSearch<'a, S, C, SDCD>
where
    C: ContextCache,
    S: SnapShotReader<SDCD>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
{
    fn from(
        ContextReferences {
            snap_shot,
            delta,
            cache,
            bound_variable_syntax,
        }: ContextReferences<'a, S, C, SDCD>,
    ) -> ContextSearch<'a, S, C, SDCD> {
        // simple_logger::init().unwrap_or(());
        ContextSearch::<'a> {
            bound_variable_syntax,
            snap_shot,
            variable_mask: VariableMaskList::from(hashmap! {}).into(),
            delta,
            caches: cache.clone(),
            syntax_evaluating: hashset! {},
        }
    }
}

pub struct ContextReferences<'a, S, C, SDCD>
where
    S: SnapShotReader<SDCD>,
    C: ContextCache,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
{
    pub snap_shot: &'a S,
    pub delta: &'a ContextDelta<S::ConceptId, SDCD>,
    pub cache: &'a C,
    pub bound_variable_syntax: &'a HashSet<SharedSyntax<C>>,
}

type VariableMask<ConceptId, SharedSyntax> = HashMap<ConceptId, SharedSyntax>;

#[derive(Clone, PartialEq, Debug)]
struct VariableMaskList<Syntax: SyntaxTree> {
    head: VariableMask<Syntax::ConceptId, Syntax::SharedSyntax>,
    tail: Option<Arc<VariableMaskList<Syntax>>>,
}

impl<Syntax: SyntaxTree>
    From<VariableMask<Syntax::ConceptId, Syntax::SharedSyntax>>
    for VariableMaskList<Syntax>
{
    fn from(
        head: VariableMask<Syntax::ConceptId, Syntax::SharedSyntax>,
    ) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl<Syntax: SyntaxTree> VariableMaskList<Syntax> {
    /// returns None if `head` is equal to one of the nodes.
    /// This prevents cycles in reduction evaluations
    fn push(
        list: &Arc<Self>,
        head: VariableMask<Syntax::ConceptId, Syntax::SharedSyntax>,
    ) -> Option<Self> {
        (!list.contains(&head)).then(|| Self {
            head,
            tail: Some(list.clone()),
        })
    }

    fn contains(
        &self,
        node: &VariableMask<Syntax::ConceptId, Syntax::SharedSyntax>,
    ) -> bool {
        &self.head == node
            || self.tail.as_ref().map_or(false, |vml| vml.contains(node))
    }

    fn get(
        &self,
        concept_id: Syntax::ConceptId,
    ) -> Option<&Syntax::SharedSyntax> {
        self.head
            .get(&concept_id)
            .or_else(|| self.tail.as_ref().and_then(|vml| vml.get(concept_id)))
    }
}

pub trait Iteration {
    type Syntax: SyntaxTree;
    type ConceptId;
    fn filter_generalisations_from_candidates(
        &self,
        example: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> Generalisations<
        Self::ConceptId,
        <Self::Syntax as SyntaxTree>::SharedSyntax,
    >;
}

pub type Generalisations<ConceptId, SharedSyntax> =
    Vec<(ConceptId, VariableMask<ConceptId, SharedSyntax>)>;

type ReductionTruthResult<ConceptId, SharedSyntax> =
    Option<(bool, ReductionReason<ConceptId, SharedSyntax>)>;
