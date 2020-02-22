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
    ast::SyntaxTree,
    constants::{
        ASSOC, DEFAULT, EXISTS_SUCH_THAT, FALSE, IMPLICATION, PRECEDENCE,
        REDUCTION, RIGHT, TRUE,
    },
    context::is_variable,
    context_delta::ContextDelta,
    snap_shot::SnapShot,
};
use dashmap::DashMap;
use maplit::hashmap;
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

#[derive(Debug)]
pub struct ContextSearch<'a> {
    snap_shot: &'a SnapShot,
    variable_mask: VariableMask,
    delta: &'a ContextDelta,
    cache: &'a ContextCache,
}

pub type ContextCache = DashMap<Arc<SyntaxTree>, Option<Arc<SyntaxTree>>>;

impl<'a> ContextSearch<'a> {
    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, id: usize) -> Option<Arc<SyntaxTree>> {
        let concept = self.snap_shot.read_concept(self.delta, id);
        concept
            .get_righthand_of()
            .iter()
            .find_map(|ro| {
                let roc = self.snap_shot.read_concept(self.delta, *ro);
                if let Some((IMPLICATION, _)) = roc.get_definition() {
                    roc.get_righthand_of().iter().find_map(|roro| {
                        self.snap_shot
                            .read_concept(self.delta, *roro)
                            .get_definition()
                            .and_then(|(condition, _)| {
                                if let Some(TRUE) = self
                                    .reduce(
                                        &self
                                            .snap_shot
                                            .to_ast(self.delta, condition),
                                    )
                                    .and_then(|condition_ast| {
                                        condition_ast.get_concept()
                                    })
                                {
                                    Some(
                                        self.snap_shot.to_ast(self.delta, TRUE),
                                    )
                                } else {
                                    None
                                }
                            })
                    })
                } else {
                    None
                }
            })
            .or_else(|| {
                concept.get_reduction().and_then(|n| {
                    if self.is_leaf_variable(n) {
                        self.variable_mask.get(&n).cloned()
                    } else {
                        Some(self.snap_shot.to_ast(self.delta, n))
                    }
                })
            })
    }

    /// Reduces the syntax by using the reduction rules of associated concepts.
    pub fn reduce(&self, ast: &Arc<SyntaxTree>) -> Option<Arc<SyntaxTree>> {
        self.cache.get(ast).map_or_else(
            || {
                let result = ast
                    .get_concept()
                    .and_then(|c| self.reduce_concept(c))
                    .or_else(|| {
                        ast.get_expansion().and_then(|(ref left, ref right)| {
                            self.reduce_pair(left, right)
                        })
                    });
                if !ast.is_variable()
                    && (&result).as_ref().map_or(true, |r| r != ast)
                {
                    self.cache.insert(ast.clone(), result.clone());
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
    ) -> Option<Arc<SyntaxTree>> {
        left.get_concept()
            .and_then(|lc| match lc {
                ASSOC => Some(self.snap_shot.to_ast(self.delta, RIGHT)),
                PRECEDENCE
                    if right
                        .get_concept()
                        .and_then(|c| {
                            self.snap_shot
                                .find_definition(self.delta, PRECEDENCE, c)
                        })
                        .is_none() =>
                {
                    Some(self.snap_shot.to_ast(self.delta, DEFAULT))
                },
                _ => {
                    self.variable_mask.get(&lc).and_then(|ast| self.reduce(ast))
                },
            })
            .or_else(|| {
                right
                    .get_expansion()
                    .and_then(|(ref rightleft, ref rightright)| {
                        self.reduce_by_expanded_right_branch(
                            left, rightleft, rightright,
                        )
                    })
                    .or_else(|| self.recursively_reduce_pair(left, right))
            })
    }

    fn recursively_reduce_pair(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> Option<Arc<SyntaxTree>> {
        let left_result = self.reduce(left);
        let right_result = self.reduce(right);
        let maybe_subbed_r =
            right.get_concept().and_then(|r| self.variable_mask.get(&r));
        let maybe_subbed_l =
            left.get_concept().and_then(|l| self.variable_mask.get(&l));
        if let (None, None) = (&left_result, &right_result) {
            self.filter_generalisations_for_pair(left, right).iter().find_map(
                |(generalisation, variable_to_syntax)| {
                    let mut context_search = self.clone();
                    context_search
                        .variable_mask
                        .extend(variable_to_syntax.clone());
                    let gen_ast = context_search
                        .snap_shot
                        .to_ast(self.delta, *generalisation);
                    context_search
                        .reduce(&gen_ast)
                        .map(|ast| context_search.substitute(&ast))
                },
            )
        } else {
            let l = left_result
                .unwrap_or_else(|| maybe_subbed_l.unwrap_or(left).clone());
            let r = right_result
                .unwrap_or_else(|| maybe_subbed_r.unwrap_or(right).clone());
            Some(self.snap_shot.contract_pair(self.delta, &l, &r))
        }
    }

    fn substitute(&self, ast: &Arc<SyntaxTree>) -> Arc<SyntaxTree> {
        ast.get_concept()
            .and_then(|c| self.variable_mask.get(&c).cloned())
            .unwrap_or_else(|| {
                ast.get_expansion().map_or_else(
                    || ast.clone(),
                    |(l, r)| {
                        self.snap_shot.contract_pair(
                            self.delta,
                            &self.substitute(&l),
                            &self.substitute(&r),
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
        let generalisation_candidates = self.find_generalisations(
            &self.snap_shot.contract_pair(self.delta, left, right),
        );
        generalisation_candidates
            .par_iter()
            .filter_map(|gc| {
                self.check_generalisation(
                    &self.snap_shot.contract_pair(self.delta, left, right),
                    *gc,
                )
                .and_then(|vm| {
                    if vm.is_empty() {
                        None
                    } else {
                        Some((*gc, vm))
                    }
                })
            })
            .collect()
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
        self.snap_shot.has_variable(self.delta, v)
            && self.variable_mask.get(&v).is_none()
    }

    fn is_leaf_concept(&self, l: usize) -> bool {
        self.snap_shot.read_concept(self.delta, l).get_definition().is_none()
    }

    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(&self, ast: &Arc<SyntaxTree>) -> Arc<SyntaxTree> {
        match self.reduce(ast) {
            Some(ref a) => self.recursively_reduce(a),
            None => ast.clone(),
        }
    }

    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        left: &Arc<SyntaxTree>,
        rightleft: &Arc<SyntaxTree>,
        rightright: &Arc<SyntaxTree>,
    ) -> Option<Arc<SyntaxTree>> {
        rightleft.get_concept().and_then(|rlc| match rlc {
            REDUCTION => {
                self.determine_reduction_truth(left, rightright).map(|x| {
                    if x {
                        self.snap_shot.to_ast(self.delta, TRUE)
                    } else {
                        self.snap_shot.to_ast(self.delta, FALSE)
                    }
                })
            },
            EXISTS_SUCH_THAT if is_variable(&left.to_string()) => {
                let mut might_exist = false;
                let results: Vec<Option<bool>> =
                    (0..self.snap_shot.concept_len(self.delta))
                        .into_par_iter()
                        .map(|i| {
                            if self.is_free_variable(i) {
                                None
                            } else {
                                let mut context_search = self.clone();
                                context_search.variable_mask.insert(
                                    left.get_concept().unwrap(),
                                    self.snap_shot.to_ast(self.delta, i),
                                );
                                let mut truth_value =
                                    context_search.substitute(rightright);
                                while let Some(reduced_rightright) =
                                    self.reduce(&truth_value)
                                {
                                    truth_value = reduced_rightright;
                                }
                                match truth_value.get_concept() {
                                    Some(TRUE) => Some(true),
                                    Some(FALSE) => Some(false),
                                    _ => None,
                                }
                            }
                        })
                        .collect();
                for result in results {
                    match result {
                        Some(true) => {
                            return Some(
                                self.snap_shot.to_ast(self.delta, TRUE),
                            )
                        },
                        Some(false) => (),
                        _ => might_exist = true,
                    };
                }
                if might_exist {
                    None
                } else {
                    Some(self.snap_shot.to_ast(self.delta, FALSE))
                }
            },
            _ => None,
        })
    }

    fn determine_reduction_truth(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> Option<bool> {
        if left == right {
            Some(false)
        } else {
            self.determine_evidence_of_reduction(left, right).or_else(|| {
                self.determine_evidence_of_reduction(right, left).map(|x| !x)
            })
        }
    }

    fn determine_evidence_of_reduction(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> Option<bool> {
        self.reduce(left).and_then(|reduced_left| {
            if &reduced_left == right {
                Some(true)
            } else {
                self.determine_evidence_of_reduction(&reduced_left, right)
            }
        })
    }
}

impl<'a> From<ContextReferences<'a>> for ContextSearch<'a> {
    fn from(
        (snap_shot, delta, cache): ContextReferences<'a>,
    ) -> ContextSearch<'a> {
        ContextSearch::<'a> {
            snap_shot,
            variable_mask: hashmap! {},
            delta,
            cache,
        }
    }
}

type ContextReferences<'a> = (&'a SnapShot, &'a ContextDelta, &'a ContextCache);

impl<'a> Clone for ContextSearch<'a> {
    fn clone(&self) -> ContextSearch<'a> {
        ContextSearch {
            snap_shot: self.snap_shot,
            variable_mask: self.variable_mask.clone(),
            delta: self.delta,
            cache: self.cache,
        }
    }
}

type VariableMask = HashMap<usize, Arc<SyntaxTree>>;
