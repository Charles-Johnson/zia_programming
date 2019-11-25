/*  Library for the Zia programming language.
    Copyright (C) 2019 Charles Johnson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

use ast::SyntaxTree;
use constants::{ASSOC, FALSE, REDUCTION, RIGHT, TRUE};
use context_delta::ContextDelta;
use snap_shot::SnapShot;
use std::{collections::HashMap, rc::Rc};

pub struct ContextSearch<'a> {
    snap_shot: &'a SnapShot,
    variable_mask: HashMap<usize, Rc<SyntaxTree>>,
    delta: &'a ContextDelta,
}

impl<'a> ContextSearch<'a> {
    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, id: usize) -> Option<Rc<SyntaxTree>> {
        if self.snap_shot.has_variable(self.delta, id) {
            if let Some(ast) = self.variable_mask.get(&id) {
                return self.reduce(ast);
            }
        }
        let concept = self.snap_shot.read_concept(self.delta, id);
        concept
            .get_reduction()
            .and_then(|n| if self.snap_shot.has_variable(self.delta, n) {
                self.variable_mask.get(&n).cloned()
            } else {
                Some(self.snap_shot.to_ast(self.delta, n))
            })
            .or_else(|| {
                concept.get_definition().and_then(|(left, right)| {
                    let left_result = self.reduce_concept(left);
                    let right_result = self.reduce_concept(right);
                    match (
                        left_result,
                        right_result,
                        self.variable_mask.get(&left),
                        self.variable_mask.get(&right),
                    ) {
                        (None, _, None, _) => None,
                        (_, None, _, None) => None,
                        (Some(l), Some(r), _, _) => {
                            Some(self.snap_shot.contract_pair(self.delta, &l, &r))
                        }
                        (Some(l), None, _, Some(original_r)) => {
                            Some(self.snap_shot.contract_pair(self.delta, &l, original_r))
                        }
                        (None, Some(r), Some(original_l), _) => {
                            Some(self.snap_shot.contract_pair(self.delta, original_l, &r))
                        }
                        (None, None, Some(original_l), Some(original_r)) => Some(
                            self.snap_shot
                                .contract_pair(self.delta, original_l, original_r),
                        ),
                    }
                })
            })
    }
    /// Reduces the syntax by using the reduction rules of associated concepts.
    pub fn reduce(&self, ast: &Rc<SyntaxTree>) -> Option<Rc<SyntaxTree>> {
        ast.get_concept()
            .and_then(|c| self.reduce_concept(c))
            .or_else(|| {
                ast.get_expansion()
                    .and_then(|(ref left, ref right)| self.reduce_pair(left, right))
            })
    }
    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair(&self, left: &Rc<SyntaxTree>, right: &Rc<SyntaxTree>) -> Option<Rc<SyntaxTree>> {
        left.get_concept()
            .and_then(|lc| match lc {
                ASSOC => Some(self.snap_shot.to_ast(self.delta, RIGHT)),
                _ => self.variable_mask.get(&lc).and_then(|ast| self.reduce(ast)),
            })
            .or_else(|| {
                right
                    .get_expansion()
                    .and_then(|(ref rightleft, ref rightright)| {
                        self.reduce_by_expanded_right_branch(left, rightleft, rightright)
                    })
                    .or_else(|| {
                        self.snap_shot
                            .match_left_right(
                                self.delta,
                                self.reduce(left),
                                self.reduce(right),
                                left,
                                right,
                            )
                            .or_else(|| {
                                self.filter_generalisations_for_pair(left, right)
                                    .iter()
                                    .filter_map(|(generalisation, variable, syntax)| {
                                        let mut context_search = self.clone();
                                        context_search
                                            .variable_mask
                                            .insert(*variable, syntax.clone());
                                        context_search.reduce_concept(*generalisation)
                                    })
                                    .nth(0)
                            })
                    })
            })
    }
    fn filter_generalisations_for_pair(
        &self,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Vec<(usize, usize, Rc<SyntaxTree>)> {
        let mut generalisations = left
            .get_concept()
            .map(|lc| {
                self.snap_shot
                    .read_concept(self.delta, lc)
                    .get_lefthand_of()
                    .iter()
                    .filter_map(|lo| {
                        if self.snap_shot.has_variable(self.delta, *lo) {
                            // Left hand branch of syntax's concept is also the left hand of a variable concept
                            self.snap_shot
                                .read_concept(self.delta, *lo)
                                .get_definition()
                                .and_then(|(_, r)| {
                                    if self.is_leaf_variable(r) {
                                        Some((*lo, r, right.clone()))
                                    } else {
                                        None
                                    }
                                })
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .unwrap_or_else(|| Vec::default());
        generalisations.extend(
            right
                .get_concept()
                .map(|rc| {
                    self.snap_shot
                        .read_concept(self.delta, rc)
                        .get_righthand_of()
                        .iter()
                        .filter_map(|ro| {
                            if self.snap_shot.has_variable(self.delta, *ro) {
                                // Right hand branch of syntax's concept is also the right hand of a variable concept
                                self.snap_shot
                                    .read_concept(self.delta, *ro)
                                    .get_definition()
                                    .and_then(|(l, _)| {
                                        if self.is_leaf_variable(l) {
                                            Some((*ro, l, left.clone()))
                                        } else {
                                            None
                                        }
                                    })
                            } else {
                                None
                            }
                        })
                        .collect()
                })
                .unwrap_or_else(|| Vec::default()),
        );
        generalisations.extend(
            right
                .get_expansion()
                .map(|(rightleft, rightright)| {
                    rightleft
                        .get_concept()
                        .map(|rlc| {
                            let mut variable_in_expressions = Vec::new();
                            for lefthand_of in self
                                .snap_shot
                                .read_concept(self.delta, rlc)
                                .get_lefthand_of()
                            {
                                let left_concept =
                                    self.snap_shot.read_concept(self.delta, *lefthand_of);
                                for righthand_of in left_concept.get_righthand_of().clone() {
                                    if self.snap_shot.has_variable(self.delta, righthand_of) {
                                        if let Some((l, _)) = self
                                            .snap_shot
                                            .read_concept(self.delta, righthand_of)
                                            .get_definition()
                                        {
                                            variable_in_expressions.push((
                                                righthand_of,
                                                l,
                                                left.clone(),
                                            ));
                                        }
                                    }
                                }
                            }
                            variable_in_expressions
                        })
                        .unwrap_or_else(|| Vec::default())
                })
                .unwrap_or_else(|| Vec::default()),
        );
        dbg!(generalisations)
    }
    fn is_leaf_variable(&self, lv: usize) -> bool {
        self.snap_shot.has_variable(self.delta, lv)
            && self
                .snap_shot
                .read_concept(self.delta, lv)
                .get_definition()
                .is_none()
    }
    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(&self, ast: &Rc<SyntaxTree>) -> Rc<SyntaxTree> {
        match self.reduce(ast) {
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
            REDUCTION => self.determine_reduction_truth(left, &rightright).map(|x| {
                if x {
                    self.snap_shot.to_ast(self.delta, TRUE)
                } else {
                    self.snap_shot.to_ast(self.delta, FALSE)
                }
            }),
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
            self.determine_evidence_of_reduction(left, right)
                .or_else(|| {
                    self.determine_evidence_of_reduction(right, left)
                        .map(|x| !x)
                })
        }
    }
    fn determine_evidence_of_reduction(
        &self,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
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
