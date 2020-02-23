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
    concepts::format_string,
    constants::{
        ASSOC, DEFAULT, EXISTS_SUCH_THAT, FALSE, GREATER_THAN, IMPLICATION,
        LEFT, PRECEDENCE, REDUCTION, RIGHT, TRUE,
    },
    context::is_variable,
    context_delta::ContextDelta,
    errors::{ZiaError, ZiaResult},
    snap_shot::{parse_line, Associativity, SnapShot},
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
                                    .reduce(&self.to_ast(condition))
                                    .and_then(|condition_ast| {
                                        condition_ast.get_concept()
                                    })
                                {
                                    Some(self.to_ast(TRUE))
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
                        Some(self.to_ast(n))
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
                ASSOC => Some(self.to_ast(RIGHT)),
                PRECEDENCE
                    if right
                        .get_concept()
                        .and_then(|c| {
                            self.snap_shot
                                .find_definition(self.delta, PRECEDENCE, c)
                        })
                        .is_none() =>
                {
                    Some(self.to_ast(DEFAULT))
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
                    let gen_ast = context_search.to_ast(*generalisation);
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
        generalisation_candidates
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
            .collect()
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
                .and_then(|lc| {
                    righthand.get_concept().and_then(|rc| {
                        self.snap_shot.find_definition(self.delta, lc, rc).map(
                            |def| {
                                self.snap_shot
                                    .get_label(self.delta, def)
                                    .map_or_else(
                                        || {
                                            self.display_joint(
                                                lefthand, righthand,
                                            )
                                        },
                                        |label| label,
                                    )
                                    .parse::<SyntaxTree>()
                                    .unwrap()
                                    .bind_concept(def)
                            },
                        )
                    })
                })
                .unwrap_or_else(|| {
                    self.display_joint(lefthand, righthand)
                        .parse::<SyntaxTree>()
                        .unwrap()
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
            REDUCTION => {
                self.determine_reduction_truth(left, rightright).map(|x| {
                    if x {
                        self.to_ast(TRUE)
                    } else {
                        self.to_ast(FALSE)
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
                                    self.to_ast(i),
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
                        Some(true) => return Some(self.to_ast(TRUE)),
                        Some(false) => (),
                        _ => might_exist = true,
                    };
                }
                if might_exist {
                    None
                } else {
                    Some(self.to_ast(FALSE))
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

    /// Returns the syntax for a concept.
    pub fn to_ast(&self, concept_id: usize) -> Arc<SyntaxTree> {
        let concept = self.snap_shot.read_concept(self.delta, concept_id);
        if let Some(s) = concept.get_string().map_or_else(
            || self.snap_shot.get_label(self.delta, concept_id),
            |s| Some(format_string(&s)),
        ) {
            Arc::new(s.parse::<SyntaxTree>().unwrap().bind_concept(concept_id))
        } else {
            let (left, right) = concept.get_definition().unwrap_or_else(|| {
                panic!("Unlabelled concept ({:#?}) with no definition", concept)
            });
            self.combine(&self.to_ast(left), &self.to_ast(right))
        }
    }

    pub fn combine(
        &self,
        ast: &Arc<SyntaxTree>,
        other: &Arc<SyntaxTree>,
    ) -> Arc<SyntaxTree> {
        let syntax = ast
            .get_concept()
            .and_then(|l| {
                other.get_concept().and_then(|r| {
                    self.snap_shot.find_definition(self.delta, l, r).map(
                        |concept| self.join(ast, other).bind_concept(concept),
                    )
                })
            })
            .unwrap_or_else(|| self.join(ast, other));
        Arc::new(syntax)
    }

    fn join(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> SyntaxTree {
        self.display_joint(left, right)
            .parse::<SyntaxTree>()
            .unwrap()
            .bind_pair(left, right)
    }

    pub fn display_joint(
        &self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> String {
        let left_string = left.get_expansion().map_or_else(
            || left.to_string(),
            |(l, r)| match self.get_associativity(&r).unwrap() {
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
            |(l, r)| match self.get_associativity(&l).unwrap() {
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

    pub fn get_associativity(
        &self,
        ast: &Arc<SyntaxTree>,
    ) -> Option<Associativity> {
        let assoc_of_ast = self.combine(&self.to_ast(ASSOC), ast);
        self.reduce(&assoc_of_ast).and_then(|ast| match ast.get_concept() {
            Some(LEFT) => Some(Associativity::Left),
            Some(RIGHT) => Some(Associativity::Right),
            _ => None,
        })
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
                        (Some(x), Some(y)) => {
                            if x == y {
                                Ok(Some(x))
                            } else {
                                Err(ZiaError::AmbiguousExpression)
                            }
                        },
                        (Some(x), None) => Ok(Some(x)),
                        (None, _) => Err(ZiaError::AmbiguousExpression),
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
        let precedence_syntax = self.to_ast(PRECEDENCE);
        let greater_than_syntax = self.to_ast(GREATER_THAN);
        let (syntax, positions, _number_of_tokens) = tokens.iter().try_fold(
            // Initially assume no concepts have the lowest precedence
            (Vec::<Arc<SyntaxTree>>::new(), Vec::<usize>::new(), None),
            |(mut lowest_precedence_syntax, mut lp_indices, prev_index),
             token| {
                // Increment index
                let this_index = prev_index.map(|x| x + 1).or(Some(0));
                let syntax_of_token = self.ast_from_token(token)?;
                let precedence_of_token =
                    self.combine(&precedence_syntax, &syntax_of_token);
                // Compare current token's precedence with each currently assumed lowest syntax
                for syntax in lowest_precedence_syntax.clone() {
                    let precedence_of_syntax =
                        self.combine(&precedence_syntax, &syntax);
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
                        Some(TRUE) => {
                            return Ok((
                                vec![syntax_of_token],
                                vec![this_index.unwrap()],
                                this_index,
                            ))
                        },
                        // syntax of token has a higher precedence than some previous lowest precendence syntax
                        // keep existing lowest precedence syntax as-is
                        Some(FALSE) => {
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
                                Some(FALSE) => {
                                    return Ok((
                                        vec![syntax_of_token],
                                        vec![this_index.unwrap()],
                                        this_index,
                                    ))
                                },
                                // syntax of token has a higher precedence than some previous lowest precendence syntax
                                // keep existing lowest precedence syntax as-is
                                Some(TRUE) => {
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

struct TokenSubsequence {
    syntax: Vec<Arc<SyntaxTree>>,
    positions: Vec<usize>,
}
