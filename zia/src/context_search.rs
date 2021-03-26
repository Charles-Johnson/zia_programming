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
    ast::SyntaxTree,
    concepts::{format_string, Concept, ConcreteConceptType},
    context_cache::{CacheList, ContextCache},
    context_delta::ContextDelta,
    context_snap_shot::Associativity,
    snap_shot::Reader as SnapShotReader,
};
use log::debug;
use maplit::{hashmap, hashset};
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::Arc,
};

#[derive(Debug)]
pub struct ContextSearch<'a, S> {
    snap_shot: &'a S,
    variable_mask: Arc<VariableMaskList>,
    delta: &'a ContextDelta,
    caches: Arc<CacheList<'a>>,
    syntax_evaluating: HashSet<Arc<SyntaxTree>>,
}

pub type ReductionResult = Option<(Arc<SyntaxTree>, ReductionReason)>;

#[derive(Clone, PartialEq, Debug)]
pub enum ReductionReason {
    Comparison(Arc<ComparisonReason>),
    Explicit,
    Rule {
        generalisation: Arc<SyntaxTree>,
        variable_mask: VariableMask,
        reason: Arc<ReductionReason>,
    },
    Inference {
        implication: Arc<SyntaxTree>,
        reason: Arc<ReductionReason>,
    },
    Default {
        operator: usize,
    },
    Partial(HashMap<Arc<SyntaxTree>, (Arc<SyntaxTree>, ReductionReason)>),
    Existence {
        example: Arc<SyntaxTree>,
        reason: Arc<ReductionReason>,
    },
    NonExistence {
        example: Arc<SyntaxTree>,
        reason: Arc<ReductionReason>,
    },
    Absence,
    Recursive {
        syntax: Arc<SyntaxTree>,
        reason: Arc<ReductionReason>,
        from: Arc<ReductionReason>,
    },
    SyntaxCannotReduceToItself,
    LeftReducesToRight {
        reason: Arc<ReductionReason>,
        left: Arc<SyntaxTree>,
        right: Arc<SyntaxTree>,
    },
    RightReducesToLeft {
        reason: Arc<ReductionReason>,
        left: Arc<SyntaxTree>,
        right: Arc<SyntaxTree>,
    },
}

impl<'a, S: SnapShotReader + Sync + std::fmt::Debug> ContextSearch<'a, S> {
    fn infer_reduction(&self, concept: &Concept) -> ReductionResult {
        debug!("infer_reduction({:#?})", concept);
        concept.get_righthand_of().iter().find_map(|ro| {
            let roc = self.snap_shot.read_concept(self.delta, *ro);
            roc.get_composition().and_then(|(l, _)| self.is_concrete_type(ConcreteConceptType::Implication, l))
                .and_then(|_| {
                    roc.get_righthand_of().iter().find_map(|roro| {
                        self.snap_shot
                            .read_concept(self.delta, *roro)
                            .get_composition()
                            .and_then(|(condition_id, _)| {
                                let condition = self.to_ast(condition_id);
                                self.reduce(&condition)
                                    .and_then(|(reduced_condition, reason)| {
                                        reduced_condition.get_concept().and_then(|x| self.is_concrete_type(ConcreteConceptType::True, x))
                                            .map(|x| (self.to_ast(x), ReductionReason::Inference{
                                                implication: self.to_ast(*roro),
                                                reason: reason.into()
                                            }))
                                    })
                            })
                    })
                }
            )
        })
    }

    fn is_concrete_type(
        &self,
        cct: ConcreteConceptType,
        concept_id: usize,
    ) -> Option<usize> {
        if Some(cct) == self.concrete_type(concept_id) {
            Some(concept_id)
        } else {
            None
        }
    }

    fn concrete_type(&self, concept_id: usize) -> Option<ConcreteConceptType> {
        self.snap_shot.concrete_concept_type(self.delta, concept_id)
    }

    fn concrete_type_of_ast(
        &self,
        ast: &Arc<SyntaxTree>,
    ) -> Option<ConcreteConceptType> {
        ast.get_concept().and_then(|c| self.concrete_type(c))
    }

    fn concrete_concept_id(&self, cct: ConcreteConceptType) -> Option<usize> {
        self.snap_shot.concrete_concept_id(self.delta, cct)
    }

    pub fn concrete_ast(
        &self,
        cct: ConcreteConceptType,
    ) -> Option<Arc<SyntaxTree>> {
        self.concrete_concept_id(cct).map(|id| self.to_ast(id))
    }

    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, id: usize) -> ReductionResult {
        debug!("reduce_concept({})", id);
        let concept = self.snap_shot.read_concept(self.delta, id);
        self.infer_reduction(&concept).or_else(|| {
            concept
                .get_reduction()
                .and_then(|n| {
                    if self.is_leaf_variable(n) {
                        self.variable_mask.get(n).cloned()
                    } else {
                        Some(self.to_ast(n))
                    }
                })
                .map(|r| (r, ReductionReason::Explicit))
        })
    }

    // If (operator right)  cannot by trying be reduced by other means, then it should reduce to default_concept
    fn reduce_otherwise_default(
        &self,
        ast: &Arc<SyntaxTree>,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
        operator_id: usize,
        default_concept_id: usize,
    ) -> ReductionResult {
        let mut reduced_pair: ReductionResult = None;
        if self.syntax_evaluating.contains(ast) || {
            let cache = ContextCache::default();
            let mut context_search = self.spawn(&cache);
            context_search.syntax_evaluating.insert(ast.clone());
            reduced_pair = context_search.reduce_pair(left, right);
            let find = |c| self.find_composition(operator_id, c);
            reduced_pair.is_none()
                && right.get_concept().and_then(find).is_none()
        } {
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
    pub fn reduce(&self, ast: &Arc<SyntaxTree>) -> ReductionResult {
        debug!("reduce({})", ast.to_string());
        self.caches.get_reduction_or_else(ast, || {
            let reduction_result = ast
                .get_concept()
                .and_then(|c| self.reduce_concept(c))
                .or_else(|| {
                    ast.get_expansion().and_then(|(ref left, ref right)| {
                        left.get_concept()
                            .and_then(|lc| match self.concrete_type(lc) {
                                Some(ConcreteConceptType::Precedence) => self
                                    .concrete_concept_id(
                                        ConcreteConceptType::Default,
                                    )
                                    .and_then(|default_concept_id| {
                                        self.reduce_otherwise_default(
                                            ast,
                                            left,
                                            right,
                                            lc,
                                            default_concept_id,
                                        )
                                    }),
                                Some(ConcreteConceptType::Associativity) => {
                                    self.concrete_concept_id(
                                        ConcreteConceptType::Right,
                                    )
                                    .and_then(|default_concept_id| {
                                        self.reduce_otherwise_default(
                                            ast,
                                            left,
                                            right,
                                            lc,
                                            default_concept_id,
                                        )
                                    })
                                },
                                _ => None,
                            })
                            .or_else(|| self.reduce_pair(left, right))
                    })
                });
            self.caches.insert_reduction(ast, &reduction_result);
            reduction_result
        })
    }

    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ReductionResult {
        debug!("reduce_pair({}, {})", left.to_string(), right.to_string());
        right
            .get_expansion()
            .and_then(|(ref rightleft, ref rightright)| {
                self.reduce_by_expanded_right_branch(
                    left, rightleft, rightright,
                )
            })
            .or_else(|| {
                left.get_expansion().and_then(|(leftleft, leftright)| {
                    self.reduce_by_expanded_left_branch(
                        &leftleft, &leftright, right,
                    )
                })
            })
            .or_else(|| self.recursively_reduce_pair(left, right))
    }

    fn recursively_reduce_pair(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ReductionResult {
        debug!("recursively_reduce_pair({}, {})", left, right);
        let left_result = self.reduce(left);
        let right_result = self.reduce(right);
        let maybe_subbed_r =
            right.get_concept().and_then(|r| self.variable_mask.get(r));
        let maybe_subbed_l =
            left.get_concept().and_then(|l| self.variable_mask.get(l));
        let cache = ContextCache::default();
        match (left_result, right_result) {
            (None, None) => self
                .filter_generalisations_for_pair(left, right)
                .iter()
                .find_map(|(generalisation, variable_mask)| {
                    let mut context_search = self.spawn(&cache);
                    // Stack overflow occurs if you remove this
                    if let Err(_) = context_search
                        .insert_variable_mask(variable_mask.clone())
                    {
                        return None;
                    }
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
                self.contract_pair(&left_ast, maybe_subbed_r.unwrap_or(right)),
                if let ReductionReason::Partial(_) = &left_reason {
                    left_reason
                } else {
                    ReductionReason::Partial(
                        hashmap! {left.clone() => (left_ast, left_reason)},
                    )
                },
            )),
            (None, Some((right_ast, right_reason))) => Some((
                self.contract_pair(maybe_subbed_l.unwrap_or(left), &right_ast),
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
                self.contract_pair(&left_ast, &right_ast),
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
        ast: &Arc<SyntaxTree>,
        variable_mask: &VariableMask,
    ) -> Arc<SyntaxTree> {
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
                    },
                )
            })
    }

    fn filter_generalisations_for_pair(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> Vec<(usize, VariableMask)> {
        let ast = &self.contract_pair(left, right);
        let generalisation_candidates = self.find_generalisations(ast);
        debug!("filter_generalisations_for_pair({}, {}): generalisation_candidates = {:#?}", left.to_string(), right.to_string(), generalisation_candidates);
        let result = generalisation_candidates
            .par_iter()
            .filter_map(|gc| {
                self.check_generalisation(ast, *gc).and_then(|vm| {
                    if vm.is_empty() {
                        None
                    } else {
                        Some((*gc, vm))
                    }
                })
            })
            .collect();
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
        lefthand: &Arc<SyntaxTree>,
        righthand: &Arc<SyntaxTree>,
    ) -> Arc<SyntaxTree> {
        lefthand
            .get_concept()
            .and_also(&righthand.get_concept())
            .and_then(|(lc, rc)| {
                self.find_composition(*lc, *rc).map(|def| {
                    let syntax = SyntaxTree::from(
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
            .into()
    }

    fn check_generalisation(
        &self,
        ast: &Arc<SyntaxTree>,
        generalisation: usize,
    ) -> Option<VariableMask> {
        self.is_free_variable(generalisation)
            .then(|| {
                if let Some((gl, gr)) = self
                    .snap_shot
                    .read_concept(self.delta, generalisation)
                    .get_composition()
                {
                    ast.get_expansion().and_then(|(l, r)| {
                        match (
                            self.is_free_variable(gl),
                            self.is_free_variable(gr),
                        ) {
                            (true, true) => self
                                .check_generalisation(&l, gl)
                                .and_also_move(
                                    self.check_generalisation(&r, gr),
                                )
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
                    })
                } else {
                    Some(hashmap! {generalisation => ast.clone()})
                }
            })
            .flatten()
    }

    fn find_generalisations(&self, ast: &Arc<SyntaxTree>) -> HashSet<usize> {
        let mut generalisations = HashSet::new();
        if let Some((l, r)) = ast.get_expansion() {
            if let Some(c) = l.get_concept() {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, c)
                        .get_lefthand_of(),
                );
            }
            if let Some(c) = r.get_concept() {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, c)
                        .get_righthand_of(),
                );
            }
            self.find_generalisations(&l).iter().for_each(|g| {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, *g)
                        .get_lefthand_of(),
                )
            });
            self.find_generalisations(&r).iter().for_each(|g| {
                generalisations.extend(
                    self.snap_shot
                        .read_concept(self.delta, *g)
                        .get_righthand_of(),
                )
            });
            generalisations
        } else {
            generalisations
        }
    }

    fn is_leaf_variable(&self, lv: usize) -> bool {
        self.is_free_variable(lv) && self.is_leaf_concept(lv)
    }

    fn is_free_variable(&self, v: usize) -> bool {
        self.snap_shot.read_concept(self.delta, v).variable()
            && self
                .variable_mask
                .tail
                .as_ref()
                .map_or(true, |vml| vml.get(v).is_none()) // A hack required to prevent stack overflow
    }

    fn is_leaf_concept(&self, l: usize) -> bool {
        self.snap_shot.read_concept(self.delta, l).get_composition().is_none()
    }

    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(
        &self,
        ast: &Arc<SyntaxTree>,
    ) -> (Arc<SyntaxTree>, Option<ReductionReason>) {
        debug!("recursively_reduce({})", ast);
        let mut maybe_reason: Option<ReductionReason> = None;
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
        leftleft: &Arc<SyntaxTree>,
        leftright: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ReductionResult {
        self.concrete_type_of_ast(leftright).and_then(|cct| match cct {
            ConcreteConceptType::ExistsSuchThat
                if leftleft
                    .get_concept()
                    .map_or(false, |c| self.is_free_variable(c)) =>
            {
                let true_id = self
                    .concrete_concept_id(ConcreteConceptType::True)
                    .expect("true concept must exist");
                let true_concept =
                    self.snap_shot.read_concept(self.delta, true_id);
                let truths = true_concept.find_what_reduces_to_it();
                self.find_example(right, &truths.copied().collect()).map(
                    |(example, reason)| {
                        (
                            self.to_ast(true_id),
                            ReductionReason::Existence {
                                example,
                                reason,
                            },
                        )
                    },
                )
            }
            _ => None,
        })
    }

    fn find_example(
        &self,
        generalisation: &Arc<SyntaxTree>,
        truths: &HashSet<usize>,
    ) -> Option<(Arc<SyntaxTree>, Arc<ReductionReason>)> {
        if generalisation.is_variable() {
            if let Some((left, right)) = generalisation.get_expansion() {
                match (left.is_variable(), right.is_variable()) {
                    (true, true) => todo!("recurse until non variable is found so that examples can be recovered"),
                    (true, false) => {
                        let right_id = right.get_concept().unwrap();
                        let right_concept = self.snap_shot.read_concept(self.delta, right_id);
                        let examples = right_concept.get_lefthand_of();
                        examples.iter().filter(|e| truths.contains(e)).filter(|e| {
                            let comp_concept = self.snap_shot.read_concept(self.delta, **e);
                            let (left_id, _) = comp_concept.get_composition().expect("a concept is the lefthand of another concept without a composition");
                            self.check_generalisation(&self.to_ast(left_id), left.get_concept().unwrap()).is_some()
                        }).next().map(|id| (
                            self.to_ast(*id),
                            Arc::new(ReductionReason::Explicit)
                        ))
                    },
                    (false, true) => {
                        let left_id = left.get_concept().unwrap();
                        let left_concept = self.snap_shot.read_concept(self.delta, left_id);
                        let examples = left_concept.get_lefthand_of();
                        examples.iter().filter(|e| truths.contains(e)).filter(|e| {
                            let comp_concept = self.snap_shot.read_concept(self.delta, **e);
                            let (_, right_id) = comp_concept.get_composition().expect("a concept is the righthand of another concept without a composition");
                            self.check_generalisation(&self.to_ast(right_id), right.get_concept().unwrap()).is_some()
                        }).next().map(|id| (
                            self.to_ast(*id),
                            Arc::new(ReductionReason::Explicit)
                        ))
                    },
                    (false, false) => unreachable!("a variable's expansion must include at least one variable")
                }
            } else {
                truths.iter().next().map(|c| {
                    (self.to_ast(*c), Arc::new(ReductionReason::Explicit))
                })
            }
        } else {
            if let Some(c) = generalisation.get_concept() {
                if truths.contains(&c) {
                    return Some((
                        generalisation.clone(),
                        Arc::new(ReductionReason::Explicit),
                    ));
                }
            }
            None
        }
    }

    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        left: &Arc<SyntaxTree>,
        rightleft: &Arc<SyntaxTree>,
        rightright: &Arc<SyntaxTree>,
    ) -> ReductionResult {
        self.concrete_type_of_ast(rightleft).and_then(|cct| match cct {
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
            ConcreteConceptType::Reduction => self
                .determine_reduction_truth(left, rightright)
                .and_then(|(x, reason)| {
                    if x {
                        self.concrete_ast(ConcreteConceptType::True)
                            .map(|ast| (ast, reason))
                    } else {
                        self.concrete_ast(ConcreteConceptType::False)
                            .map(|ast| (ast, reason))
                    }
                }),
            _ => None,
        })
    }

    fn determine_reduction_truth(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> Option<(bool, ReductionReason)> {
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
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> Option<ReductionReason> {
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
    pub fn to_ast(&self, concept_id: usize) -> Arc<SyntaxTree> {
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
                            SyntaxTree::from(s),
                            concept_id,
                        )
                        .into()
                } else if let Some((left, right)) = concept.get_composition() {
                    self.combine(&self.to_ast(left), &self.to_ast(right))
                } else {
                    self.snap_shot
                        .new_syntax_from_concept(self.delta, concept_id)
                        .into()
                };
                self.caches.insert_syntax_tree(&concept, &syntax);
                syntax
            })
        })
    }

    pub fn combine(
        &self,
        ast: &Arc<SyntaxTree>,
        other: &Arc<SyntaxTree>,
    ) -> Arc<SyntaxTree> {
        let syntax = ast
            .get_concept()
            .and_also(&other.get_concept())
            .and_then(|(l, r)| {
                self.find_composition(*l, *r).map(|concept| {
                    let syntax = self.join(ast, other);
                    self.snap_shot
                        .bind_concept_to_syntax(self.delta, syntax, concept)
                })
            })
            .unwrap_or_else(|| self.join(ast, other));
        syntax.into()
    }

    fn join(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> SyntaxTree {
        SyntaxTree::from(self.display_joint(left, right))
            .bind_pair(left.clone(), right.clone())
    }

    fn display_joint(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
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

    pub fn get_associativity(&self, ast: &Arc<SyntaxTree>) -> Associativity {
        self.concrete_concept_id(ConcreteConceptType::Associativity).map_or(
            Associativity::Right,
            |associativity_concept_id| {
                let assoc_of_ast =
                    self.combine(&self.to_ast(associativity_concept_id), ast);
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
    pub fn expand(&self, ast: &Arc<SyntaxTree>) -> Arc<SyntaxTree> {
        if let Some(con) = ast.get_concept() {
            if let Some((left, right)) =
                self.snap_shot.read_concept(self.delta, con).get_composition()
            {
                self.combine(
                    &self.expand(&self.to_ast(left)),
                    &self.expand(&self.to_ast(right)),
                )
            } else {
                self.to_ast(con)
            }
        } else if let Some((ref left, ref right)) = ast.get_expansion() {
            self.combine(&self.expand(left), &self.expand(right))
        } else {
            ast.clone()
        }
    }

    pub fn find_composition(&self, left: usize, right: usize) -> Option<usize> {
        let left_concept = self.snap_shot.read_concept(self.delta, left);
        let right_concept = self.snap_shot.read_concept(self.delta, right);
        left_concept.find_definition(&right_concept)
    }

    pub fn compare(
        &self,
        some_syntax: &Arc<SyntaxTree>,
        another_syntax: &Arc<SyntaxTree>,
    ) -> (Comparison, ComparisonReason) {
        if some_syntax == another_syntax {
            return (Comparison::EqualTo, ComparisonReason::SameSyntax);
        }
        if let Some(greater_than_concept_id) =
            self.concrete_concept_id(ConcreteConceptType::GreaterThan)
        {
            let greater_than_syntax = self.to_ast(greater_than_concept_id);
            let comparing_syntax = self.combine(
                some_syntax,
                &self.combine(&greater_than_syntax, another_syntax),
            );
            let comparing_reversed_syntax = self.combine(
                another_syntax,
                &self.combine(&greater_than_syntax, some_syntax),
            );
            let mut reason = None;
            let mut reversed_reason = None;
            let cache = ContextCache::default();
            (
                match (
                    if self.syntax_evaluating.contains(&comparing_syntax) {
                        self.caches
                            .get_reduction_or_else(&comparing_syntax, || None)
                            .map(|(s, r)| (s, Some(r)))
                    } else {
                        let mut context_search = self.spawn(&cache);
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
                                &comparing_reversed_syntax,
                                || None,
                            )
                            .map(|(s, r)| (s, Some(r)))
                    } else {
                        let mut context_search = self.spawn(&cache);
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

    pub fn spawn(&self, cache: &'a ContextCache) -> Self {
        Self {
            caches: Arc::new(CacheList::spawn(&self.caches, cache)),
            delta: self.delta,
            snap_shot: self.snap_shot,
            syntax_evaluating: self.syntax_evaluating.clone(),
            variable_mask: self.variable_mask.clone(),
        }
    }

    /// Error if `variable_mask` is already included
    fn insert_variable_mask(
        &mut self,
        variable_mask: VariableMask,
    ) -> Result<(), ()> {
        self.variable_mask = Arc::new(
            VariableMaskList::push(&self.variable_mask, variable_mask)
                .ok_or(())?,
        );
        Ok(())
    }
}

fn simplify_reasoning(
    reason: Option<ReductionReason>,
    reversed_reason: Option<ReductionReason>,
) -> ComparisonReason {
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
pub enum ComparisonReason {
    SameSyntax,
    Reduction {
        reason: Option<ReductionReason>,
        reversed_reason: Option<ReductionReason>,
    },
    NoGreaterThanConcept,
}

impl<'a, S> From<ContextReferences<'a, S>> for ContextSearch<'a, S> {
    fn from(
        (snap_shot, delta, cache): ContextReferences<'a, S>,
    ) -> ContextSearch<'a, S> {
        // simple_logger::init().unwrap_or(());
        ContextSearch::<'a> {
            snap_shot,
            variable_mask: VariableMaskList::from(hashmap! {}).into(),
            delta,
            caches: Arc::new(cache.into()),
            syntax_evaluating: hashset! {},
        }
    }
}

type ContextReferences<'a, S> = (&'a S, &'a ContextDelta, &'a ContextCache);

type VariableMask = HashMap<usize, Arc<SyntaxTree>>;

#[derive(Clone, PartialEq, Debug)]
struct VariableMaskList {
    head: VariableMask,
    tail: Option<Arc<VariableMaskList>>,
}

impl From<VariableMask> for VariableMaskList {
    fn from(head: VariableMask) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl VariableMaskList {
    /// returns None if `head` is equal to one of the nodes.
    /// This prevents cycles in reduction evaluations
    fn push(list: &Arc<Self>, head: VariableMask) -> Option<Self> {
        (!list.contains(&head)).then(|| Self {
            head,
            tail: Some(list.clone()),
        })
    }

    fn contains(&self, node: &VariableMask) -> bool {
        &self.head == node
            || self.tail.as_ref().map_or(false, |vml| vml.contains(node))
    }

    fn get(&self, concept_id: usize) -> Option<&Arc<SyntaxTree>> {
        self.head
            .get(&concept_id)
            .or_else(|| self.tail.as_ref().and_then(|vml| vml.get(concept_id)))
    }
}
