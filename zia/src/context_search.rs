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
use maplit::hashmap;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

/// Reduces the syntax by using the reduction rules of associated concepts.
pub fn reduce(
    context_search: &ContextSearch,
    ast: &Rc<SyntaxTree>,
) -> Option<Rc<SyntaxTree>> {
    ast.get_concept().and_then(|c| context_search.reduce_concept(c)).or_else(
        || {
            ast.get_expansion().and_then(|(ref left, ref right)| {
                context_search.reduce_pair(left, right)
            })
        },
    )
}

#[derive(Debug)]
pub struct ContextSearch<'a> {
    snap_shot: &'a SnapShot,
    variable_mask: VariableMask,
    delta: &'a ContextDelta,
}

impl<'a> ContextSearch<'a> {
    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, id: usize) -> Option<Rc<SyntaxTree>> {
        let concept = self.snap_shot.read_concept(self.delta, id);
        concept
            .get_righthand_of()
            .iter()
            .filter_map(|ro| {
                let roc = self.snap_shot.read_concept(self.delta, *ro);
                if let Some((IMPLICATION, _)) = roc.get_definition() {
                    roc.get_righthand_of()
                        .iter()
                        .filter_map(|roro| {
                            self.snap_shot
                                .read_concept(self.delta, *roro)
                                .get_definition()
                                .and_then(|(condition, _)| {
                                    if let Some(TRUE) = reduce(
                                        self,
                                        &self
                                            .snap_shot
                                            .to_ast(self.delta, condition),
                                    )
                                    .and_then(|condition_ast| {
                                        condition_ast.get_concept()
                                    }) {
                                        Some(
                                            self.snap_shot
                                                .to_ast(self.delta, TRUE),
                                        )
                                    } else {
                                        None
                                    }
                                })
                        })
                        .nth(0)
                } else {
                    None
                }
            })
            .nth(0)
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

    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair(
        &self,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<Rc<SyntaxTree>> {
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
                _ => self
                    .variable_mask
                    .get(&lc)
                    .and_then(|ast| reduce(self, ast)),
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
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<Rc<SyntaxTree>> {
        let left_result = reduce(self, left);
        let right_result = reduce(self, right);
        let maybe_subbed_r =
            right.get_concept().and_then(|r| self.variable_mask.get(&r));
        let maybe_subbed_l =
            left.get_concept().and_then(|l| self.variable_mask.get(&l));
        if let (None, None) = (&left_result, &right_result) {
            self.filter_generalisations_for_pair(left, right)
                .iter()
                .filter_map(|(generalisation, variable_to_syntax)| {
                    let mut context_search = self.clone();
                    context_search
                        .variable_mask
                        .extend(variable_to_syntax.clone());
                    let gen_ast = context_search
                        .snap_shot
                        .to_ast(self.delta, *generalisation);
                    reduce(&context_search, &gen_ast)
                        .map(|ast| context_search.substitute(&ast))
                })
                .nth(0)
        } else {
            let l = left_result
                .unwrap_or_else(|| maybe_subbed_l.unwrap_or(left).clone());
            let r = right_result
                .unwrap_or_else(|| maybe_subbed_r.unwrap_or(right).clone());
            Some(self.snap_shot.contract_pair(self.delta, &l, &r))
        }
    }

    fn substitute(&self, ast: &Rc<SyntaxTree>) -> Rc<SyntaxTree> {
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
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Vec<(usize, VariableMask)> {
        let generalisation_candidates = self.find_generalisations(
            &self.snap_shot.contract_pair(self.delta, left, right),
        );
        generalisation_candidates
            .iter()
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
        ast: &Rc<SyntaxTree>,
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

    fn find_generalisations(&self, ast: &Rc<SyntaxTree>) -> HashSet<usize> {
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
    pub fn recursively_reduce(&self, ast: &Rc<SyntaxTree>) -> Rc<SyntaxTree> {
        match reduce(self, ast) {
            Some(ref a) => self.recursively_reduce(a),
            None => ast.clone(),
        }
    }

    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        left: &Rc<SyntaxTree>,
        rightleft: &Rc<SyntaxTree>,
        rightright: &Rc<SyntaxTree>,
    ) -> Option<Rc<SyntaxTree>> {
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
                for truth_value in (0..self.snap_shot.concept_len(self.delta))
                    .filter_map(|i| {
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
                                reduce(self, &truth_value)
                            {
                                truth_value = reduced_rightright;
                            }
                            Some(truth_value)
                        }
                    })
                {
                    match truth_value.get_concept() {
                        Some(TRUE) => return Some(truth_value.clone()),
                        Some(FALSE) => (),
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
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
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
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<bool> {
        reduce(self, left).and_then(|reduced_left| {
            if &reduced_left == right {
                Some(true)
            } else {
                self.determine_evidence_of_reduction(&reduced_left, right)
            }
        })
    }
}

impl<'a> From<(&'a SnapShot, &'a ContextDelta)> for ContextSearch<'a> {
    fn from(context: (&'a SnapShot, &'a ContextDelta)) -> ContextSearch<'a> {
        ContextSearch::<'a> {
            snap_shot: context.0,
            variable_mask: hashmap! {},
            delta: context.1,
        }
    }
}

impl<'a> Clone for ContextSearch<'a> {
    fn clone(&self) -> ContextSearch<'a> {
        ContextSearch {
            snap_shot: self.snap_shot,
            variable_mask: self.variable_mask.clone(),
            delta: self.delta,
        }
    }
}

type VariableMask = HashMap<usize, Rc<SyntaxTree>>;
