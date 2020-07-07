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
    concepts::{format_string, Concept},
    context_delta::ContextDelta,
    context_snap_shot::Associativity,
    snap_shot::Reader as SnapShotReader,
};
use dashmap::DashMap;
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
    variable_mask: VariableMask,
    delta: &'a ContextDelta,
    cache: &'a ContextCache,
    syntax_evaluating: HashSet<Arc<SyntaxTree>>,
}

#[derive(Debug, Default, Clone)]
pub struct ContextCache {
    reductions: DashMap<Arc<SyntaxTree>, ReductionResult>,
    syntax_trees: DashMap<usize, Arc<SyntaxTree>>,
}

type ReductionResult = Option<(Arc<SyntaxTree>, ReductionReason)>;

#[derive(Clone, PartialEq, Debug)]
pub enum ReductionReason {
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

impl<'a, S: SnapShotReader + Sync> ContextSearch<'a, S> {
    fn infer_reduction(&self, concept: &Concept) -> ReductionResult {
        debug!("infer_reduction({:#?})", concept);
        concept.get_righthand_of().iter().find_map(|ro| {
            let roc = self.snap_shot.read_concept(self.delta, *ro);
            roc.get_definition().and_then(|(l, _)| {
                if l == S::implication_id() {
                    roc.get_righthand_of().iter().find_map(|roro| {
                        self.snap_shot
                            .read_concept(self.delta, *roro)
                            .get_definition()
                            .and_then(|(condition_id, _)| {
                                let condition = self.to_ast(condition_id);
                                self.reduce(&condition)
                                    .and_then(|(reduced_condition, reason)| {
                                        reduced_condition.get_concept().and_then(|x| {
                                            if x == S::true_id() {
                                                Some((self.to_ast(x), ReductionReason::Inference{
                                                    implication: self.to_ast(*roro),
                                                    reason: reason.into()
                                                }))
                                            } else {
                                                None
                                            }
                                        })
                                    })
                            })
                    })
                } else {
                    None
                }
            })
        })
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
                        self.variable_mask.get(&n).cloned()
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
        if self.syntax_evaluating.get(ast).is_some() || {
            let mut context_search = self.clone();
            context_search.syntax_evaluating.insert(ast.clone());
            reduced_pair = context_search.reduce_pair(left, right);
            let find = |c| self.find_definition(operator_id, c);
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
        self.cache.reductions.get(ast).map_or_else(
            || {
                let result = ast
                    .get_concept()
                    .and_then(|c| self.reduce_concept(c))
                    .or_else(|| {
                        ast.get_expansion().and_then(|(ref left, ref right)| {
                            left.get_concept()
                                .and_then(|lc| {
                                    if lc == S::precedence_id() {
                                        self.reduce_otherwise_default(
                                            ast,
                                            left,
                                            right,
                                            lc,
                                            S::default_id(),
                                        )
                                    } else if lc == S::assoc_id() {
                                        self.reduce_otherwise_default(
                                            ast,
                                            left,
                                            right,
                                            lc,
                                            S::right_id(),
                                        )
                                    } else {
                                        None
                                    }
                                })
                                .or_else(|| self.reduce_pair(left, right))
                        })
                    });
                if !ast.is_variable()
                    && (&result).as_ref().map_or(true, |(r, _)| r != ast)
                {
                    self.cache.reductions.insert(ast.clone(), result.clone());
                }
                result
            },
            |r| r.as_ref().cloned(),
        )
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
            .or_else(|| self.recursively_reduce_pair(left, right))
    }

    fn recursively_reduce_pair(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ReductionResult {
        let left_result = self.reduce(left);
        let right_result = self.reduce(right);
        let maybe_subbed_r =
            right.get_concept().and_then(|r| self.variable_mask.get(&r));
        let maybe_subbed_l =
            left.get_concept().and_then(|l| self.variable_mask.get(&l));
        match (left_result, right_result) {
            (None, None) => self
                .filter_generalisations_for_pair(left, right)
                .iter()
                .find_map(|(generalisation, variable_mask)| {
                    let mut context_search = self.clone();
                    context_search.variable_mask.extend(variable_mask.clone());
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
        let generalisation_candidates =
            self.find_generalisations(&self.contract_pair(left, right));
        debug!("filter_generalisations_for_pair({}, {}): generalisation_candidates = {:#?}", left.to_string(), right.to_string(), generalisation_candidates);
        let result = generalisation_candidates
            .par_iter()
            .filter_map(|gc| {
                self.check_generalisation(&self.contract_pair(left, right), *gc)
                    .and_then(|vm| {
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
                self.find_definition(*lc, *rc).map(|def| {
                    SyntaxTree::from(
                        self.snap_shot.get_label(self.delta, def).map_or_else(
                            || self.display_joint(lefthand, righthand),
                            |label| label,
                        ),
                    )
                    .bind_concept(def)
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
        if self.is_free_variable(generalisation) {
            if let Some((gl, gr)) = self
                .snap_shot
                .read_concept(self.delta, generalisation)
                .get_definition()
            {
                if let Some((l, r)) = ast.get_expansion() {
                    let gen_left_var = self.is_free_variable(gl);
                    let gen_right_var = self.is_free_variable(gr);
                    if gen_left_var && gen_right_var {
                        if let (Some(lm), Some(mut rm)) = (
                            self.check_generalisation(&l, gl),
                            self.check_generalisation(&r, gr),
                        ) {
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
                        } else {
                            None
                        }
                    } else if gen_left_var {
                        if r.get_concept().map_or(false, |c| c == gr) {
                            self.check_generalisation(&l, gl)
                        } else {
                            None
                        }
                    } else if gen_right_var {
                        if l.get_concept().map_or(false, |c| c == gl) {
                            self.check_generalisation(&r, gr)
                        } else {
                            None
                        }
                    } else if l.get_concept().map_or(false, |c| c == gl)
                        && r.get_concept().map_or(false, |c| c == gr)
                    {
                        Some(hashmap! {})
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                Some(hashmap! {generalisation => ast.clone()})
            }
        } else {
            None
        }
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
            && self.variable_mask.get(&v).is_none()
    }

    fn is_leaf_concept(&self, l: usize) -> bool {
        self.snap_shot.read_concept(self.delta, l).get_definition().is_none()
    }

    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(
        &self,
        ast: &Arc<SyntaxTree>,
    ) -> (Arc<SyntaxTree>, Option<ReductionReason>) {
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

    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        left: &Arc<SyntaxTree>,
        rightleft: &Arc<SyntaxTree>,
        rightright: &Arc<SyntaxTree>,
    ) -> ReductionResult {
        rightleft.get_concept().and_then(|rlc| match rlc {
            x if x == S::reduction_id() => self
                .determine_reduction_truth(left, rightright)
                .map(|(x, reason)| {
                    if x {
                        (self.to_ast(S::true_id()), reason)
                    } else {
                        (self.to_ast(S::false_id()), reason)
                    }
                }),
            x if x == S::exists_such_that_id()
                && left
                    .get_concept()
                    .map_or(false, |c| self.is_free_variable(c)) =>
            {
                let mut might_exist = false;
                let results: Vec<Option<(bool, ReductionReason)>> = (0..self
                    .snap_shot
                    .lowest_unoccupied_concept_id(self.delta))
                    .into_par_iter()
                    .map(|i| {
                        if self.is_free_variable(i) {
                            None
                        } else {
                            let mut context_search = self.clone();
                            let example = self.to_ast(i);
                            let variable_concept = left.get_concept().unwrap();
                            context_search.variable_mask.insert(
                                variable_concept,
                                example.clone(),
                            );
                            let (truth_value, maybe_reason) = self
                                .recursively_reduce(
                                    &context_search.substitute(rightright, &hashmap! {variable_concept => example.clone()}),
                                );
                            match (truth_value.get_concept(), maybe_reason) {
                                (Some(x), Some(reason))
                                    if x == S::true_id() =>
                                {
                                    Some((
                                        true,
                                        ReductionReason::Existence {
                                            example,
                                            reason: reason.into(),
                                        },
                                    ))
                                },
                                (Some(x), Some(reason))
                                    if x == S::false_id() =>
                                {
                                    Some((
                                        false,
                                        ReductionReason::NonExistence {
                                            example,
                                            reason: reason.into(),
                                        },
                                    ))
                                },
                                _ => None,
                            }
                        }
                    })
                    .collect();
                for result in results {
                    match result {
                        Some((true, reason)) => {
                            return Some((self.to_ast(S::true_id()), reason))
                        },
                        Some((false, _)) => (),
                        _ => might_exist = true,
                    };
                }
                if might_exist {
                    None
                } else {
                    Some((self.to_ast(S::false_id()), ReductionReason::Absence))
                }
            }
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
        self.variable_mask.get(&concept_id).cloned().unwrap_or_else(|| {
            self.cache.syntax_trees.get(&concept_id).map_or_else(
                || {
                    let concept =
                        self.snap_shot.read_concept(self.delta, concept_id);
                    let syntax = if let Some(s) =
                        concept.get_string().map_or_else(
                            || self.snap_shot.get_label(self.delta, concept_id),
                            |s| Some(format_string(&s)),
                        ) {
                        SyntaxTree::from(s).bind_concept(concept_id).into()
                    } else if let Some((left, right)) = concept.get_definition()
                    {
                        self.combine(&self.to_ast(left), &self.to_ast(right))
                    } else {
                        SyntaxTree::new_concept(concept_id).into()
                    };
                    if !concept.variable() {
                        self.cache
                            .syntax_trees
                            .insert(concept_id, syntax.clone());
                    }
                    syntax
                },
                |r| r.value().clone(),
            )
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
                self.find_definition(*l, *r)
                    .map(|concept| self.join(ast, other).bind_concept(concept))
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
        let assoc_of_ast = self.combine(&self.to_ast(S::assoc_id()), ast);
        if Some(S::left_id())
            == self.reduce(&assoc_of_ast).and_then(|(ast, _)| ast.get_concept())
        {
            Associativity::Left
        } else {
            Associativity::Right
        }
    }

    /// Expands syntax by definition of its associated concept.
    pub fn expand(&self, ast: &Arc<SyntaxTree>) -> Arc<SyntaxTree> {
        if let Some(con) = ast.get_concept() {
            if let Some((left, right)) =
                self.snap_shot.read_concept(self.delta, con).get_definition()
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

    pub fn find_definition(&self, left: usize, right: usize) -> Option<usize> {
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
        let greater_than_syntax =
            SyntaxTree::new_concept(S::greater_than_id()).into();
        let comparing_syntax = self.combine(
            some_syntax,
            &self.combine(&greater_than_syntax, another_syntax),
        );
        let comparing_reversed_syntax = self.combine(
            another_syntax,
            &self.combine(&greater_than_syntax, some_syntax),
        );
        let (syntax_comparison, reason) =
            self.recursively_reduce(&comparing_syntax);
        let (reversed_comparison, reversed_reason) =
            self.recursively_reduce(&comparing_reversed_syntax);
        (
            match (
                syntax_comparison.get_concept(),
                reversed_comparison.get_concept(),
            ) {
                (Some(x), Some(y))
                    if x == S::false_id() && y == S::false_id() =>
                {
                    Comparison::EqualTo
                },
                (Some(x), Some(y))
                    if x == S::true_id() && y == S::true_id() =>
                {
                    panic!("{:#?} is both greater than and less than {:#?}!\nReason: {:#?}\n Reversed reason: {:#?}", some_syntax, another_syntax, reason, reversed_reason);
                }
                (Some(x), _) if x == S::true_id() => Comparison::GreaterThan,
                (_, Some(x)) if x == S::true_id() => Comparison::LessThan,
                (Some(x), _) if x == S::false_id() => {
                    Comparison::LessThanOrEqualTo
                },
                (_, Some(x)) if x == S::false_id() => {
                    Comparison::GreaterThanOrEqualTo
                },
                _ => Comparison::Incomparable,
            },
            ComparisonReason::Reduction {
                reason,
                reversed_reason,
            },
        )
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
}

impl<'a, S> From<ContextReferences<'a, S>> for ContextSearch<'a, S> {
    fn from(
        (snap_shot, delta, cache): ContextReferences<'a, S>,
    ) -> ContextSearch<'a, S> {
        ContextSearch::<'a> {
            snap_shot,
            variable_mask: hashmap! {},
            delta,
            cache,
            syntax_evaluating: hashset! {},
        }
    }
}

type ContextReferences<'a, S> = (&'a S, &'a ContextDelta, &'a ContextCache);

impl<'a, S> Clone for ContextSearch<'a, S> {
    fn clone(&self) -> ContextSearch<'a, S> {
        ContextSearch {
            snap_shot: self.snap_shot,
            variable_mask: self.variable_mask.clone(),
            delta: self.delta,
            cache: self.cache,
            syntax_evaluating: self.syntax_evaluating.clone(),
        }
    }
}

type VariableMask = HashMap<usize, Arc<SyntaxTree>>;
