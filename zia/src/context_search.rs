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
}

impl<'a> ContextSearch<'a> {
    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(&self, deltas: &ContextDelta, concept: usize) -> Option<Rc<SyntaxTree>> {
        self.variable_mask
            .get(&concept)
            .and_then(|ast| self.reduce(deltas, ast))
            .or_else(|| {
                self.snap_shot
                    .read_concept(deltas, concept)
                    .get_reduction()
                    .map(|n| self.snap_shot.to_ast(deltas, n))
                    .or_else(|| {
                        self.snap_shot
                            .read_concept(deltas, concept)
                            .get_definition()
                            .and_then(|(left, right)| {
                                let left_result = self.reduce_concept(deltas, left);
                                let right_result = self.reduce_concept(deltas, right);
                                self.snap_shot.match_left_right(
                                    deltas,
                                    left_result,
                                    right_result,
                                    &self.snap_shot.to_ast(deltas, left),
                                    &self.snap_shot.to_ast(deltas, right),
                                )
                            })
                    })
            })
    }
    /// Reduces the syntax by using the reduction rules of associated concepts.
    pub fn reduce(&self, deltas: &ContextDelta, ast: &Rc<SyntaxTree>) -> Option<Rc<SyntaxTree>> {
        ast.get_concept()
            .and_then(|c| self.reduce_concept(deltas, c))
            .or_else(|| {
                ast.get_expansion()
                    .and_then(|(ref left, ref right)| self.reduce_pair(deltas, left, right))
            })
    }
    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<Rc<SyntaxTree>> {
        left.get_concept()
            .and_then(|lc| match lc {
                ASSOC => Some(self.snap_shot.to_ast(deltas, RIGHT)),
                _ => self
                    .variable_mask
                    .get(&lc)
                    .and_then(|ast| self.reduce(deltas, ast)),
            })
            .or_else(|| {
                right
                    .get_expansion()
                    .and_then(|(ref rightleft, ref rightright)| {
                        self.reduce_by_expanded_right_branch(deltas, left, rightleft, rightright)
                    })
                    .or_else(|| {
                        self.snap_shot
                            .match_left_right(
                                deltas,
                                self.reduce(deltas, left),
                                self.reduce(deltas, right),
                                left,
                                right,
                            )
                            .or_else(|| {
                                self.snap_shot
                                    .filter_generalisations_for_pair(deltas, left, right)
                                    .iter()
                                    .filter_map(|(generalisation, variable, syntax)| {
                                        let context_search = ContextSearch {
                                            variable_mask: hashmap! {*variable => syntax.clone()},
                                            snap_shot: self.snap_shot.clone(),
                                        };
                                        context_search.reduce_concept(deltas, *generalisation)
                                    })
                                    .nth(0)
                            })
                    })
            })
    }
    /// Reduces the syntax as much as possible (returns the normal form syntax).
    pub fn recursively_reduce(
        &self,
        deltas: &ContextDelta,
        ast: &Rc<SyntaxTree>,
    ) -> Rc<SyntaxTree> {
        match self.reduce(deltas, ast) {
            Some(ref a) => self.recursively_reduce(deltas, a),
            None => ast.clone(),
        }
    }
    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        rightleft: &Rc<SyntaxTree>,
        rightright: &Rc<SyntaxTree>,
    ) -> Option<Rc<SyntaxTree>> {
        rightleft.get_concept().and_then(|rlc| match rlc {
            REDUCTION => self
                .determine_reduction_truth(deltas, left, &rightright)
                .map(|x| {
                    if x {
                        self.snap_shot.to_ast(deltas, TRUE)
                    } else {
                        self.snap_shot.to_ast(deltas, FALSE)
                    }
                }),
            _ => None,
        })
    }
    fn determine_reduction_truth(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<bool> {
        if left == right {
            Some(false)
        } else {
            self.determine_evidence_of_reduction(deltas, left, right)
                .or_else(|| {
                    self.determine_evidence_of_reduction(deltas, right, left)
                        .map(|x| !x)
                })
        }
    }
    fn determine_evidence_of_reduction(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<bool> {
        self.reduce(deltas, left).and_then(|reduced_left| {
            if &reduced_left == right {
                Some(true)
            } else {
                self.determine_evidence_of_reduction(deltas, &reduced_left, right)
            }
        })
    }
}

impl<'a> From<&'a SnapShot> for ContextSearch<'a> {
    fn from(snap_shot: &SnapShot) -> ContextSearch {
        ContextSearch {
            snap_shot,
            variable_mask: hashmap! {},
        }
    }
}
