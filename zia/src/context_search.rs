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
    context::is_variable,
    context_delta::ContextDelta,
    context_snap_shot::{parse_line, Associativity},
    errors::{ZiaError, ZiaResult},
    snap_shot::Reader as SnapShotReader,
};
use dashmap::DashMap;
use log::debug;
use maplit::{hashmap, hashset};
use rayon::prelude::*;
use std::{
    collections::{HashMap, HashSet},
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
    reductions: DashMap<Arc<SyntaxTree>, Option<Arc<SyntaxTree>>>,
    syntax_trees: DashMap<usize, Arc<SyntaxTree>>,
}

impl<'a, S: SnapShotReader + Sync> ContextSearch<'a, S> {
    fn infer_reduction(&self, concept: &Concept) -> Option<Arc<SyntaxTree>> {
        concept.get_righthand_of().iter().find_map(|ro| {
            let roc = self.snap_shot.read_concept(self.delta, *ro);
            roc.get_definition().and_then(|(l, _)| {
                if l == S::implication_id() {
                    roc.get_righthand_of().iter().find_map(|roro| {
                        self.snap_shot
                            .read_concept(self.delta, *roro)
                            .get_definition()
                            .and_then(|(condition, _)| {
                                self.reduce(&self.to_ast(condition))
                                    .and_then(|condition_ast| {
                                        condition_ast.get_concept()
                                    })
                                    .and_then(|x| {
                                        if x == S::true_id() {
                                            Some(self.to_ast(x))
                                        } else {
                                            None
                                        }
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
    fn reduce_concept(&self, id: usize) -> Option<Arc<SyntaxTree>> {
        let concept = self.snap_shot.read_concept(self.delta, id);
        self.infer_reduction(&concept).or_else(|| {
            concept.get_reduction().and_then(|n| {
                if self.is_leaf_variable(n) {
                    self.variable_mask.get(&n).cloned()
                } else {
                    Some(self.to_ast(n))
                }
            })
        })
    }

    fn reduce_otherwise_default(
        &self,
        ast: &Arc<SyntaxTree>,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
        left_concept_id: usize,
        default_concept_id: usize,
    ) -> Option<Arc<SyntaxTree>> {
        let find = |c| self.find_definition(left_concept_id, c);
        if self.syntax_evaluating.get(ast).is_some() {
            Some(self.to_ast(default_concept_id))
        } else {
            let mut context_search = self.clone();
            context_search.syntax_evaluating.insert(ast.clone());
            context_search.reduce_pair(left, right).or_else(|| {
                if right.get_concept().and_then(find).is_none() {
                    Some(self.to_ast(default_concept_id))
                } else {
                    None
                }
            })
        }
    }

    /// Reduces the syntax by using the reduction rules of associated concepts.
    pub fn reduce(&self, ast: &Arc<SyntaxTree>) -> Option<Arc<SyntaxTree>> {
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
                    && (&result).as_ref().map_or(true, |r| r != ast)
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
    ) -> Option<Arc<SyntaxTree>> {
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
                    context_search
                        .reduce_concept(*generalisation)
                        .map(|ast| context_search.substitute(&ast))
                },
            )
        } else {
            let l = left_result
                .unwrap_or_else(|| maybe_subbed_l.unwrap_or(left).clone());
            let r = right_result
                .unwrap_or_else(|| maybe_subbed_r.unwrap_or(right).clone());
            Some(self.contract_pair(&l, &r))
        }
    }

    fn substitute(&self, ast: &Arc<SyntaxTree>) -> Arc<SyntaxTree> {
        ast.get_concept()
            .and_then(|c| self.variable_mask.get(&c).cloned())
            .unwrap_or_else(|| {
                ast.get_expansion().map_or_else(
                    || ast.clone(),
                    |(l, r)| {
                        self.contract_pair(
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
        Arc::new(
            lefthand
                .get_concept()
                .and_also(&righthand.get_concept())
                .and_then(|(lc, rc)| {
                    self.find_definition(*lc, *rc).map(|def| {
                        SyntaxTree::from(
                            self.snap_shot
                                .get_label(self.delta, def)
                                .map_or_else(
                                    || self.display_joint(lefthand, righthand),
                                    |label| label,
                                ),
                        )
                        .bind_concept(def)
                    })
                })
                .unwrap_or_else(|| {
                    self.display_joint(lefthand, righthand).into()
                })
                .bind_pair(lefthand, righthand),
        )
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
            x if x == S::reduction_id() => {
                self.determine_reduction_truth(left, rightright).map(|x| {
                    if x {
                        self.to_ast(S::true_id())
                    } else {
                        self.to_ast(S::false_id())
                    }
                })
            },
            x if x == S::exists_such_that_id()
                && is_variable(&left.to_string()) =>
            {
                let mut might_exist = false;
                let results: Vec<Option<bool>> = (0..self
                    .snap_shot
                    .concept_len(self.delta))
                    .into_par_iter()
                    .map(|i| {
                        if self.is_free_variable(i) {
                            None
                        } else {
                            let mut context_search = self.clone();
                            context_search.variable_mask.insert(
                                left.get_concept().unwrap(),
                                self.to_ast(i),
                            );
                            let mut truth_value =
                                context_search.substitute(rightright);
                            debug!(
                                "Reducing expression: {}",
                                truth_value.to_string()
                            );
                            while let Some(reduced_rightright) =
                                self.reduce(&truth_value)
                            {
                                debug!(
                                    "{} reduces to {}",
                                    truth_value.to_string(),
                                    reduced_rightright.to_string()
                                );
                                truth_value = reduced_rightright;
                            }
                            match truth_value.get_concept() {
                                Some(x) if x == S::true_id() => Some(true),
                                Some(x) if x == S::false_id() => Some(false),
                                _ => None,
                            }
                        }
                    })
                    .collect();
                for result in results {
                    match result {
                        Some(true) => return Some(self.to_ast(S::true_id())),
                        Some(false) => (),
                        _ => might_exist = true,
                    };
                }
                if might_exist {
                    None
                } else {
                    Some(self.to_ast(S::false_id()))
                }
            }
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
                        Arc::new(SyntaxTree::from(s).bind_concept(concept_id))
                    } else if let Some((left, right)) = concept.get_definition()
                    {
                        self.combine(&self.to_ast(left), &self.to_ast(right))
                    } else {
                        Arc::new(SyntaxTree::new_concept(concept_id))
                    };
                    self.cache.syntax_trees.insert(concept_id, syntax.clone());
                    syntax
                },
                |r| r.value().clone(),
            )
        })
    }

    fn combine(
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
        Arc::new(syntax)
    }

    fn join(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> SyntaxTree {
        SyntaxTree::from(self.display_joint(left, right)).bind_pair(left, right)
    }

    fn display_joint(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> String {
        let left_string = left.get_expansion().map_or_else(
            || left.to_string(),
            |(l, r)| match self.get_associativity(&r) {
                Associativity::Left => l.to_string() + " " + &r.to_string(),
                Associativity::Right => {
                    "(".to_string()
                        + &l.to_string()
                        + " "
                        + &r.to_string()
                        + ")"
                },
            },
        );
        let right_string = right.get_expansion().map_or_else(
            || right.to_string(),
            |(l, r)| match self.get_associativity(&l) {
                Associativity::Left => {
                    "(".to_string()
                        + &l.to_string()
                        + " "
                        + &r.to_string()
                        + ")"
                },
                Associativity::Right => l.to_string() + " " + &r.to_string(),
            },
        );
        left_string + " " + &right_string
    }

    fn get_associativity(&self, ast: &Arc<SyntaxTree>) -> Associativity {
        let assoc_of_ast = self.combine(&self.to_ast(S::assoc_id()), ast);
        if Some(S::left_id())
            == self.reduce(&assoc_of_ast).and_then(|ast| ast.get_concept())
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

    pub fn ast_from_expression(&self, s: &str) -> ZiaResult<Arc<SyntaxTree>> {
        let tokens: Vec<String> = parse_line(s)?;
        self.ast_from_tokens(&tokens)
    }

    fn ast_from_tokens(&self, tokens: &[String]) -> ZiaResult<Arc<SyntaxTree>> {
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token(&tokens[0]),
            2 => self.ast_from_pair(&tokens[0], &tokens[1]),
            _ => {
                let TokenSubsequence {
                    syntax: lp_syntax,
                    positions: lp_indices,
                } = self.lowest_precedence_info(tokens)?;
                if lp_indices.is_empty() {
                    return Err(ZiaError::AmbiguousExpression);
                }
                let assoc = lp_syntax.iter().try_fold(None, |assoc, syntax| {
                    match (self.get_associativity(syntax), assoc) {
                        (x, Some(y)) => {
                            if x == y {
                                Ok(Some(x))
                            } else {
                                Err(ZiaError::AmbiguousExpression)
                            }
                        },
                        (x, None) => Ok(Some(x)),
                    }
                });
                match assoc? {
                    Some(Associativity::Right) => {
                        let tail = lp_indices
                            .iter()
                            .rev()
                            .try_fold((None, None), |state, lp_index| {
                                self.associativity_try_fold_handler(
                                    tokens,
                                    state,
                                    *lp_index,
                                    &Associativity::Right,
                                )
                            })?
                            .0
                            .unwrap(); // Already checked that lp_indices is non-empty;
                        if lp_indices[0] == 0 {
                            Ok(tail)
                        } else {
                            let head =
                                self.ast_from_tokens(&tokens[..lp_indices[0]])?;
                            Ok(self.combine(&head, &tail))
                        }
                    },
                    Some(Associativity::Left) => lp_indices
                        .iter()
                        .try_fold((None, None), |state, lp_index| {
                            self.associativity_try_fold_handler(
                                tokens,
                                state,
                                *lp_index,
                                &Associativity::Left,
                            )
                        })?
                        .0
                        .ok_or(ZiaError::AmbiguousExpression),
                    None => Err(ZiaError::AmbiguousExpression),
                }
            },
        }
    }

    fn associativity_try_fold_handler(
        &self,
        tokens: &[String],
        state: (Option<Arc<SyntaxTree>>, Option<usize>),
        lp_index: usize,
        assoc: &Associativity,
    ) -> ZiaResult<(Option<Arc<SyntaxTree>>, Option<usize>)> {
        let prev_lp_index = state.1;
        let slice = match assoc {
            Associativity::Left => match prev_lp_index {
                Some(i) => &tokens[i..lp_index],
                None => &tokens[..lp_index],
            },
            Associativity::Right => match prev_lp_index {
                Some(i) => &tokens[lp_index..i],
                None => &tokens[lp_index..],
            },
        };
        // Required otherwise self.ast_from_tokens will return Err(ZiaError::EmprtyParentheses)
        if slice.is_empty() {
            return Err(ZiaError::AmbiguousExpression);
        }
        let edge_index = match assoc {
            Associativity::Left => slice.len() - 1,
            Associativity::Right => 0,
        };
        let lp_with_the_rest = if lp_index == edge_index {
            let edge_syntax = self.ast_from_token(&slice[edge_index])?;
            if slice.len() == 1 {
                edge_syntax
            } else {
                match assoc {
                    Associativity::Left => self.combine(
                        &if slice.len() < 3 {
                            self.ast_from_token(&slice[slice.len() - 1])?
                        } else {
                            self.ast_from_tokens(&slice[..slice.len() - 1])?
                        },
                        &edge_syntax,
                    ),
                    Associativity::Right => self.combine(
                        &edge_syntax,
                        &if slice.len() < 3 {
                            self.ast_from_token(&slice[1])?
                        } else {
                            self.ast_from_tokens(&slice[1..])?
                        },
                    ),
                }
            }
        } else {
            self.ast_from_tokens(slice)?
        };
        let edge = state.0;
        Ok((
            Some(match edge {
                None => lp_with_the_rest,
                Some(e) => match assoc {
                    Associativity::Left => self.combine(&e, &lp_with_the_rest),
                    Associativity::Right => self.combine(&lp_with_the_rest, &e),
                },
            }),
            Some(lp_index),
        ))
    }

    /// Determine the syntax and the positions in the token sequence of the concepts with the lowest precedence
    fn lowest_precedence_info(
        &self,
        tokens: &[String],
    ) -> ZiaResult<TokenSubsequence> {
        let precedence_syntax =
            Arc::new(SyntaxTree::new_concept(S::precedence_id()));
        let greater_than_syntax =
            Arc::new(SyntaxTree::new_concept(S::greater_than_id()));
        let (syntax, positions, _number_of_tokens) = tokens.iter().try_fold(
            // Initially assume no concepts have the lowest precedence
            (Vec::<Arc<SyntaxTree>>::new(), Vec::<usize>::new(), None),
            |(mut lowest_precedence_syntax, mut lp_indices, prev_index),
             token| {
                // Increment index
                let this_index = prev_index.map(|x| x + 1).or(Some(0));
                let raw_syntax_of_token = SyntaxTree::from(token);
                let (precedence_of_token, syntax_of_token) = if let Some(c) =
                    self.snap_shot.concept_from_label(self.delta, token)
                {
                    let syntax_of_token =
                        Arc::new(raw_syntax_of_token.bind_concept(c));
                    (
                        self.combine(&precedence_syntax, &syntax_of_token),
                        syntax_of_token,
                    )
                } else {
                    (
                        Arc::new(SyntaxTree::new_concept(S::default_id())),
                        Arc::new(raw_syntax_of_token),
                    )
                };
                // Compare current token's precedence with each currently assumed lowest syntax
                for syntax in lowest_precedence_syntax.clone() {
                    let precedence_of_syntax = if syntax.get_concept().is_some()
                    {
                        self.combine(&precedence_syntax, &syntax)
                    } else {
                        Arc::new(SyntaxTree::new_concept(S::default_id()))
                    };
                    let comparing_between_tokens = self.combine(
                        &precedence_of_syntax,
                        &self.combine(
                            &greater_than_syntax,
                            &precedence_of_token,
                        ),
                    );
                    match self
                        .recursively_reduce(&comparing_between_tokens)
                        .get_concept()
                    {
                        // syntax of token has an even lower precedence than some previous lowest precendence syntax
                        // reset lowest precedence syntax with just this one
                        Some(x) if x == S::true_id() => {
                            return Ok((
                                vec![syntax_of_token],
                                vec![this_index.unwrap()],
                                this_index,
                            ))
                        },
                        // syntax of token has a higher precedence than some previous lowest precendence syntax
                        // keep existing lowest precedence syntax as-is
                        Some(x) if x == S::false_id() => {
                            return Ok((
                                lowest_precedence_syntax,
                                lp_indices,
                                this_index,
                            ))
                        },
                        _ => {
                            let comparing_between_tokens_reversed = self
                                .combine(
                                    &precedence_of_token,
                                    &self.combine(
                                        &greater_than_syntax,
                                        &precedence_of_syntax,
                                    ),
                                );
                            match self
                                .recursively_reduce(
                                    &comparing_between_tokens_reversed,
                                )
                                .get_concept()
                            {
                                // syntax of token has an even lower precedence than some previous lowest precendence syntax
                                // reset lowest precedence syntax with just this one
                                Some(x) if x == S::false_id() => {
                                    return Ok((
                                        vec![syntax_of_token],
                                        vec![this_index.unwrap()],
                                        this_index,
                                    ))
                                },
                                // syntax of token has a higher precedence than some previous lowest precendence syntax
                                // keep existing lowest precedence syntax as-is
                                Some(x) if x == S::true_id() => {
                                    return Ok((
                                        lowest_precedence_syntax,
                                        lp_indices,
                                        this_index,
                                    ))
                                },
                                // Cannot determine if token has higher or lower precedence than this syntax
                                // Check other syntax with lowest precedence
                                _ => (),
                            };
                        },
                    };
                }
                // syntax of token has neither higher or lower precedence than the lowest precedence syntax
                lowest_precedence_syntax.push(syntax_of_token);
                lp_indices.push(this_index.unwrap());
                Ok((lowest_precedence_syntax, lp_indices, this_index))
            },
        )?;
        Ok(TokenSubsequence {
            syntax,
            positions,
        })
    }

    fn ast_from_pair(
        &self,
        left: &str,
        right: &str,
    ) -> ZiaResult<Arc<SyntaxTree>> {
        let lefthand = self.ast_from_token(left)?;
        let righthand = self.ast_from_token(right)?;
        Ok(self.combine(&lefthand, &righthand))
    }

    fn ast_from_token(&self, t: &str) -> ZiaResult<Arc<SyntaxTree>> {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression(t)
        } else {
            Ok(Arc::new(self.snap_shot.ast_from_symbol(self.delta, t)))
        }
    }

    pub fn find_definition(&self, left: usize, right: usize) -> Option<usize> {
        let left_concept = self.snap_shot.read_concept(self.delta, left);
        let right_concept = self.snap_shot.read_concept(self.delta, right);
        left_concept.find_definition(&right_concept)
    }
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

struct TokenSubsequence {
    syntax: Vec<Arc<SyntaxTree>>,
    positions: Vec<usize>,
}
