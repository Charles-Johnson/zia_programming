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
        concept.get_reduction().and_then(|n| {
            if self.is_leaf_variable(n) {
                self.variable_mask.get(&n).cloned()
            } else {
                Some(self.snap_shot.to_ast(self.delta, n))
            }
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
                        let left_result = self.reduce(left);
                        let right_result = self.reduce(right);
                        let maybe_subbed_r =
                            right.get_concept().and_then(|r| self.variable_mask.get(&r));
                        let maybe_subbed_l =
                            left.get_concept().and_then(|l| self.variable_mask.get(&l));
                        if let (None, None) = (&left_result, &right_result) {
                            None
                        } else {
                            let l = left_result
                                .unwrap_or_else(|| maybe_subbed_l.unwrap_or(left).clone());
                            let r = right_result
                                .unwrap_or_else(|| maybe_subbed_r.unwrap_or(right).clone());
                            Some(self.snap_shot.contract_pair(self.delta, &l, &r))
                        }
                    })
                    .or_else(|| {
                        self.filter_generalisations_for_pair(left, right)
                            .iter()
                            .filter_map(|(generalisation, variable_to_syntax)| {
                                let mut context_search = self.clone();
                                context_search
                                    .variable_mask
                                    .extend(variable_to_syntax.clone());
                                let gen_ast =
                                    context_search.snap_shot.to_ast(self.delta, *generalisation);
                                context_search
                                    .reduce(&gen_ast)
                                    .map(|ast| context_search.substitute(&ast))
                            })
                            .nth(0)
                    })
            })
    }
    fn substitute(&self, ast: &Rc<SyntaxTree>) -> Rc<SyntaxTree> {
        ast.get_concept()
            .and_then(|c| self.variable_mask.get(&c).cloned())
            .unwrap_or_else(|| {
                ast.get_expansion()
                    .map(|(l, r)| {
                        self.snap_shot.contract_pair(
                            self.delta,
                            &self.substitute(&l),
                            &self.substitute(&r),
                        )
                    })
                    .unwrap_or_else(|| ast.clone())
            })
    }
    fn filter_generalisations_for_pair(
        &self,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Vec<(usize, VariableMask)> {
        let mut generalisations = left
            .get_concept()
            .map(|lc| {
                self.snap_shot
                    .read_concept(self.delta, lc)
                    .get_lefthand_of()
                    .iter()
                    .filter_map(|lo| {
                        self.check_generalisation(
                            &self.snap_shot.contract_pair(self.delta, left, right),
                            *lo,
                        )
                        .and_then(|vm| {
                            if vm.is_empty() {
                                None
                            } else {
                                Some((*lo, vm))
                            }
                        })
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
                            self.check_generalisation(
                                &self.snap_shot.contract_pair(self.delta, left, right),
                                *ro,
                            )
                            .and_then(|vm| {
                                if vm.is_empty() {
                                    None
                                } else {
                                    Some((*ro, vm))
                                }
                            })
                        })
                        .collect()
                })
                .unwrap_or_else(|| Vec::default()),
        );
        generalisations.extend(
            right
                .get_expansion()
                .map(|(rightleft, _)| {
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
                                    if let Some(vm) = self.check_generalisation(
                                        &self.snap_shot.contract_pair(self.delta, left, right),
                                        righthand_of,
                                    ) {
                                        if !vm.is_empty() {
                                            variable_in_expressions.push((righthand_of, vm));
                                        }
                                    }
                                }
                            }
                            variable_in_expressions
                        })
                        .unwrap_or_default()
                })
                .unwrap_or_default(),
        );
        generalisations
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
                    let glv = self.is_free_variable(gl);
                    let grv = self.is_free_variable(gr);
                    if glv && grv {
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
                    } else if glv {
                        if r.get_concept().map(|c| c == gr).unwrap_or(false) {
                            self.check_generalisation(&l, gl)
                        } else {
                            None
                        }
                    } else if grv {
                        if l.get_concept().map(|c| c == gl).unwrap_or(false) {
                            self.check_generalisation(&r, gr)
                        } else {
                            None
                        }
                    } else if l.get_concept().map(|c| c == gl).unwrap_or(false)
                        && r.get_concept().map(|c| c == gr).unwrap_or(false)
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
    // fn find_generalisations(&self, ast: &Rc<SyntaxTree>) -> Vec<usize> {
    //     if let Some((l, r)) = ast.get_expansion() {

    //     } else {
    //         Vec::default()
    //     }
    // }
    fn is_leaf_variable(&self, lv: usize) -> bool {
        self.is_free_variable(lv) && self.is_leaf_concept(lv)
    }
    fn is_free_variable(&self, v: usize) -> bool {
        self.snap_shot.has_variable(self.delta, v) && self.variable_mask.get(&v).is_none()
    }
    fn is_leaf_concept(&self, l: usize) -> bool {
        self.snap_shot
            .read_concept(self.delta, l)
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

type VariableMask = HashMap<usize, Rc<SyntaxTree>>;
