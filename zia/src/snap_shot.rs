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

use ast::SyntaxTree;
use concepts::Concept;
use constants::{
    ASSOC, FALSE, GREATER_THAN, LABEL, LEFT, PRECEDENCE, RIGHT, TRUE,
};
use context_delta::{ConceptDelta, ContextDelta, StringDelta};
use context_search::ContextSearch;
use delta::Apply;
use errors::{ZiaError, ZiaResult};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    rc::Rc,
};

/// A container for adding, reading, writing and removing concepts of generic type `T`.
#[derive(Default, Debug, Clone)]
pub struct SnapShot {
    /// Relates a String value to the index where the concept corresponding to the String is stored
    /// in the `concepts` field.
    string_map: HashMap<String, usize>,
    /// Concepts may be stored at an index of this vector as `Some(T)`. If that concept is removed
    /// from the context, `None` will be left at its index.
    concepts: Vec<Option<Concept>>,
    /// Keeps track of indices of the `concepts` field that have `None`.
    gaps: Vec<usize>,
    variables: HashSet<usize>,
}

#[derive(Debug, PartialEq)]
enum Associativity {
    Left,
    Right,
}

impl SnapShot {
    pub fn has_variable(&self, delta: &ContextDelta, concept: usize) -> bool {
        let in_previous_variables = self.variables.contains(&concept);
        delta.concept.get(&concept).map_or(
            in_previous_variables,
            |(cd, v, _)| match cd {
                ConceptDelta::Insert(_) => *v,
                ConceptDelta::Remove(_) => false,
                ConceptDelta::Update(_) => in_previous_variables,
            },
        )
    }

    pub fn get_concept(&self, id: usize) -> Option<&Concept> {
        match self.concepts.get(id) {
            Some(Some(c)) => Some(c),
            _ => None,
        }
    }

    fn write_concept(&mut self, id: usize) -> &mut Concept {
        match self.concepts[id] {
            Some(ref mut c) => c,
            None => panic!("No concept with id = {}", id),
        }
    }

    pub fn read_concept(&self, delta: &ContextDelta, id: usize) -> Concept {
        delta
            .concept
            .get(&id)
            .and_then(|(cd, _, _)| match cd {
                ConceptDelta::Insert(c) => Some(c.clone()),
                ConceptDelta::Remove(_) => None,
                ConceptDelta::Update(d) => {
                    let mut concept = self
                        .get_concept(id)
                        .expect("Deltas imply that a concept that doesn't exist will be updated!")
                        .clone();
                    concept.apply(d.clone());
                    Some(concept)
                }
            })
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .unwrap_or_else(|| panic!("No concept with id = {}", id))
                    .clone()
            })
    }

    pub fn add_concept_delta(
        &self,
        delta: &ContextDelta,
        concept: Concept,
        variable: bool,
    ) -> (ContextDelta, usize) {
        let mut added_gaps = Vec::<usize>::new();
        let mut removed_gaps = HashSet::<usize>::new();
        let mut new_concept_length = self.concepts.len();
        for (id, (cd, _, _)) in &delta.concept {
            if let ConceptDelta::Insert(_) = cd {
                if *id >= new_concept_length {
                    new_concept_length = *id + 1
                }
            }
        }
        for (id, (cd, _, _)) in &delta.concept {
            match cd {
                ConceptDelta::Insert(_) => {
                    removed_gaps.insert(*id);
                },
                ConceptDelta::Remove(_) => {
                    added_gaps.push(*id);
                    removed_gaps.remove(id);
                },
                ConceptDelta::Update(_) => (),
            }
        }
        let index: usize;
        let mut gap_index = if self.gaps.is_empty() {
            None
        } else {
            Some(self.gaps.len() - 1)
        };
        loop {
            match (added_gaps.pop(), gap_index) {
                (Some(id), _) => {
                    if removed_gaps.contains(&id) {
                        continue;
                    } else {
                        index = id;
                        break;
                    }
                },
                (None, Some(gi)) => {
                    if removed_gaps.contains(&self.gaps[gi]) {
                        if gi == 0 {
                            index = new_concept_length;
                            break;
                        } else {
                            gap_index = Some(gi - 1);
                            continue;
                        }
                    } else {
                        index = self.gaps[gi];
                        break;
                    }
                },
                (None, None) => {
                    index = new_concept_length;
                    break;
                },
            };
        }
        (
            ContextDelta {
                concept: hashmap! {index => (ConceptDelta::Insert(concept), variable, false)},
                string: hashmap! {},
            },
            index,
        )
    }

    pub fn add_string_delta(string_id: usize, string: &str) -> ContextDelta {
        ContextDelta {
            string: hashmap! {string.to_string() => StringDelta::Insert(string_id)},
            concept: HashMap::default(),
        }
    }

    fn add_string(&mut self, string_id: usize, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }

    pub fn get_normal_form(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> Option<usize> {
        self.read_concept(deltas, concept)
            .get_reduction()
            .map(|n| self.get_normal_form(deltas, n).unwrap_or(n))
    }

    pub fn get_concept_of_label(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> Option<usize> {
        self.read_concept(deltas, concept)
            .get_righthand_of()
            .iter()
            .filter(|candidate| {
                self.read_concept(deltas, **candidate)
                    .get_definition()
                    .expect("Candidate should have a definition!")
                    .0
                    == LABEL
            })
            .nth(0)
            .cloned()
    }

    pub fn is_disconnected(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> bool {
        self.read_concept(deltas, concept).get_reduction().is_none()
            && self.read_concept(deltas, concept).get_definition().is_none()
            && self.read_concept(deltas, concept).get_lefthand_of().is_empty()
            && self.righthand_of_without_label_is_empty(deltas, concept)
            && self
                .read_concept(deltas, concept)
                .find_what_reduces_to_it()
                .next()
                .is_none()
    }

    fn righthand_of_without_label_is_empty(
        &self,
        deltas: &ContextDelta,
        con: usize,
    ) -> bool {
        self.read_concept(deltas, con)
            .get_righthand_of()
            .iter()
            .filter_map(|concept| {
                self.read_concept(deltas, *concept)
                    .get_definition()
                    .filter(|(left, _)| *left != LABEL)
            })
            .nth(0)
            .is_none()
    }

    fn blindly_remove_concept(&mut self, id: usize) {
        self.concepts[id] = None;
        self.gaps.push(id);
    }

    fn remove_string(&mut self, string: &str) {
        self.string_map.remove(string).expect("No string to remove!");
    }

    pub fn ast_from_expression(
        &self,
        deltas: &ContextDelta,
        s: &str,
    ) -> ZiaResult<Rc<SyntaxTree>> {
        let tokens: Vec<String> = parse_line(s)?;
        self.ast_from_tokens(deltas, &tokens)
    }

    fn ast_from_tokens(
        &self,
        deltas: &ContextDelta,
        tokens: &[String],
    ) -> ZiaResult<Rc<SyntaxTree>> {
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token(deltas, &tokens[0]),
            2 => self.ast_from_pair(deltas, &tokens[0], &tokens[1]),
            _ => {
                let precedence_syntax = self.to_ast(deltas, PRECEDENCE);
                let greater_than_syntax = self.to_ast(deltas, GREATER_THAN);
                let (lp_syntax, lp_indices, _number_of_tokens) =
                    tokens.iter().try_fold(
                        (
                            Vec::<Rc<SyntaxTree>>::new(),
                            Vec::<usize>::new(),
                            None,
                        ),
                        |(lowest_precedence_syntax, lp_indices, prev_index),
                         token| {
                            let this_index =
                                prev_index.map(|x| x + 1).or(Some(0));
                            let syntax_of_token =
                                self.ast_from_token(deltas, token)?;
                            let precedence_of_token = self.combine(
                                deltas,
                                &precedence_syntax,
                                &syntax_of_token,
                            );
                            for syntax in lowest_precedence_syntax.clone() {
                                let precedence_of_syntax = self.combine(
                                    deltas,
                                    &precedence_syntax,
                                    &syntax,
                                );
                                let comparing_between_tokens = self.combine(
                                    deltas,
                                    &precedence_of_syntax,
                                    &self.combine(
                                        deltas,
                                        &greater_than_syntax,
                                        &precedence_of_token,
                                    ),
                                );
                                match dbg!(ContextSearch::from((self, deltas))
                                    .recursively_reduce(&dbg!(
                                        comparing_between_tokens
                                    ))
                                    .get_concept())
                                {
                                    // syntax of token has an even lower precedence than some previous lowest precendence syntax
                                    Some(TRUE) => {
                                        return Ok((
                                            vec![syntax_of_token],
                                            vec![this_index.unwrap()],
                                            this_index,
                                        ))
                                    },
                                    // syntax of token has a higher precedence than some previous lowest precendence syntax
                                    Some(FALSE) => {
                                        return Ok((
                                            lowest_precedence_syntax,
                                            lp_indices,
                                            this_index,
                                        ))
                                    },
                                    _ => {
                                        let comparing_between_tokens_reversed =
                                            self.combine(
                                                deltas,
                                                &precedence_of_token,
                                                &self.combine(
                                                    deltas,
                                                    &greater_than_syntax,
                                                    &precedence_of_syntax,
                                                ),
                                            );
                                        match dbg!(ContextSearch::from((
                                            self, deltas,
                                        ))
                                        .recursively_reduce(&dbg!(
                                            comparing_between_tokens_reversed
                                        ),))
                                        .get_concept()
                                        {
                                            // syntax of token has an even lower precedence than some previous lowest precendence syntax
                                            Some(FALSE) => {
                                                return Ok((
                                                    vec![syntax_of_token],
                                                    vec![this_index.unwrap()],
                                                    this_index,
                                                ))
                                            },
                                            // syntax of token has a higher precedence than some previous lowest precendence syntax
                                            Some(TRUE) => {
                                                return Ok((
                                                    lowest_precedence_syntax,
                                                    lp_indices,
                                                    this_index,
                                                ))
                                            },
                                            _ => (),
                                        };
                                    },
                                };
                            }
                            // syntax of token has neither higher or lower precedence than the lowest precedence syntax
                            let mut hps = lowest_precedence_syntax;
                            hps.push(syntax_of_token);
                            let mut hi = lp_indices;
                            hi.push(this_index.unwrap());
                            Ok((hps, hi, this_index))
                        },
                    )?;
                if lp_indices.is_empty() {
                    return Err(ZiaError::AmbiguousExpression);
                }
                let assoc = lp_syntax.iter().try_fold(None, |assoc, syntax| {
                    match (self.get_associativity(deltas, syntax), assoc) {
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
                            .try_fold(
                                (None, None),
                                |(tail, prev_lp_index), lp_index| {
                                    let slice = match prev_lp_index {
                                        Some(i) => &tokens[*lp_index..i],
                                        None => &tokens[*lp_index..],
                                    };
                                    let lp_with_the_rest =
                                        self.ast_from_tokens(deltas, slice)?;
                                    Ok((
                                        Some(match tail {
                                            None => lp_with_the_rest,
                                            Some(t) => self.combine(
                                                deltas,
                                                &lp_with_the_rest,
                                                &t,
                                            ),
                                        }),
                                        Some(*lp_index),
                                    ))
                                },
                            )?
                            .0
                            .unwrap(); // Already checked that lp_indices is non-empty;
                        if lp_indices[0] == 0 {
                            Ok(tail)
                        } else {
                            let head = self.ast_from_tokens(
                                deltas,
                                &tokens[..lp_indices[0]],
                            )?;
                            Ok(self.combine(deltas, &head, &tail))
                        }
                    },
                    Some(Associativity::Left) => lp_indices
                        .iter()
                        .try_fold(
                            (None, None),
                            |(head, prev_lp_index), lp_index| {
                                let slice = match prev_lp_index {
                                    Some(i) => &tokens[i..*lp_index],
                                    None => &tokens[..*lp_index],
                                };
                                // Required otherwise self.ast_from_tokens will return Err(ZiaError::EmprtyParentheses)
                                if slice.is_empty() {
                                    return Err(ZiaError::AmbiguousExpression);
                                }
                                let lp_with_the_rest =
                                    self.ast_from_tokens(deltas, slice)?;
                                Ok((
                                    Some(match head {
                                        None => lp_with_the_rest,
                                        Some(h) => self.combine(
                                            deltas,
                                            &h,
                                            &lp_with_the_rest,
                                        ),
                                    }),
                                    Some(*lp_index),
                                ))
                            },
                        )?
                        .0
                        .ok_or(ZiaError::AmbiguousExpression),
                    None => Err(ZiaError::AmbiguousExpression),
                }
            },
        }
    }

    fn ast_from_pair(
        &self,
        deltas: &ContextDelta,
        left: &str,
        right: &str,
    ) -> ZiaResult<Rc<SyntaxTree>> {
        let lefthand = self.ast_from_token(deltas, left)?;
        let righthand = self.ast_from_token(deltas, right)?;
        Ok(self.combine(deltas, &lefthand, &righthand))
    }

    fn ast_from_token(
        &self,
        deltas: &ContextDelta,
        t: &str,
    ) -> ZiaResult<Rc<SyntaxTree>> {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression(deltas, t)
        } else {
            Ok(Rc::new(self.ast_from_symbol(deltas, t)))
        }
    }

    pub fn concept_from_label(
        &self,
        deltas: &ContextDelta,
        s: &str,
    ) -> Option<usize> {
        self.get_string_concept(deltas, s)
            .and_then(|c| self.get_labellee(deltas, c))
    }

    fn ast_from_symbol(&self, deltas: &ContextDelta, s: &str) -> SyntaxTree {
        self.concept_from_label(deltas, s).map_or_else(
            || s.parse().unwrap(),
            |concept| s.parse::<SyntaxTree>().unwrap().bind_concept(concept),
        )
    }

    fn get_string_concept(
        &self,
        delta: &ContextDelta,
        s: &str,
    ) -> Option<usize> {
        delta
            .string
            .get(s)
            .map_or_else(
                || self.string_map.get(s),
                |string_delta| match string_delta {
                    StringDelta::Update {
                        after,
                        ..
                    } => Some(after),
                    StringDelta::Insert(concept) => Some(concept),
                    StringDelta::Remove(_) => None,
                },
            )
            .cloned()
    }

    pub fn contains(
        &self,
        deltas: &ContextDelta,
        outer: usize,
        inner: usize,
    ) -> bool {
        if let Some((left, right)) =
            self.read_concept(deltas, outer).get_definition()
        {
            left == inner
                || right == inner
                || self.contains(deltas, left, inner)
                || self.contains(deltas, right, inner)
        } else {
            false
        }
    }

    pub fn find_definition(
        &self,
        delta: &ContextDelta,
        lefthand: usize,
        righthand: usize,
    ) -> Option<usize> {
        let lc = self.read_concept(delta, lefthand);
        let rc = self.read_concept(delta, righthand);
        let has_lefthand = lc.get_lefthand_of();
        let has_righthand = rc.get_righthand_of();
        let mut candidates = has_lefthand.intersection(has_righthand);
        candidates.next().map(|index| {
            candidates.next().map_or(*index, |_| {
                panic!("Multiple definitions with the same lefthand and righthand pair exist.")
            })
        })
    }

    fn get_labellee(&self, delta: &ContextDelta, c: usize) -> Option<usize> {
        let concept = self.read_concept(delta, c);
        let mut candidates: VecDeque<usize> =
            concept.find_what_reduces_to_it().copied().collect();
        loop {
            if let Some(candidate) = candidates.pop_front() {
                let candidate_concept = self.read_concept(delta, candidate);
                if let Some((r, x)) = candidate_concept.get_definition() {
                    if r == LABEL {
                        return Some(x);
                    }
                }
                let extra_candidates =
                    candidate_concept.find_what_reduces_to_it().copied();
                candidates.extend(extra_candidates);
            } else {
                return None;
            }
        }
    }

    pub fn get_label(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> Option<String> {
        match self.get_concept_of_label(deltas, concept) {
            None => self
                .read_concept(deltas, concept)
                .get_reduction()
                .and_then(|r| self.get_label(deltas, r)),
            Some(d) => self
                .get_normal_form(deltas, d)
                .and_then(|n| self.read_concept(deltas, n).get_string()),
        }
    }

    /// Expands syntax by definition of its associated concept.
    pub fn expand(
        &self,
        deltas: &ContextDelta,
        ast: &Rc<SyntaxTree>,
    ) -> Rc<SyntaxTree> {
        if let Some(con) = ast.get_concept() {
            if let Some((left, right)) =
                self.read_concept(deltas, con).get_definition()
            {
                self.combine(
                    deltas,
                    &self.expand(deltas, &self.to_ast(deltas, left)),
                    &self.expand(deltas, &self.to_ast(deltas, right)),
                )
            } else {
                self.to_ast(deltas, con)
            }
        } else if let Some((ref left, ref right)) = ast.get_expansion() {
            self.combine(
                deltas,
                &self.expand(deltas, left),
                &self.expand(deltas, right),
            )
        } else {
            ast.clone()
        }
    }

    /// Returns the syntax for a concept.
    pub fn to_ast(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> Rc<SyntaxTree> {
        if let Some(s) = self.get_label(deltas, concept) {
            Rc::new(s.parse::<SyntaxTree>().unwrap().bind_concept(concept))
        } else {
            let (left, right) = self
                .read_concept(deltas, concept)
                .get_definition()
                .unwrap_or_else(|| {
                    panic!(
                        "Unlabelled concept ({}) with no definition",
                        concept
                    )
                });
            self.combine(
                deltas,
                &self.to_ast(deltas, left),
                &self.to_ast(deltas, right),
            )
        }
    }

    pub fn combine(
        &self,
        deltas: &ContextDelta,
        ast: &Rc<SyntaxTree>,
        other: &Rc<SyntaxTree>,
    ) -> Rc<SyntaxTree> {
        let syntax = ast
            .get_concept()
            .and_then(|l| {
                other.get_concept().and_then(|r| {
                    self.find_definition(deltas, l, r).map(|concept| {
                        self.join(deltas, ast, other).bind_concept(concept)
                    })
                })
            })
            .unwrap_or_else(|| self.join(deltas, ast, other));
        Rc::new(syntax)
    }

    fn join(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> SyntaxTree {
        self.display_joint(deltas, left, right)
            .parse::<SyntaxTree>()
            .unwrap()
            .bind_pair(left, right)
    }

    fn display_joint(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> String {
        let left_string = left.get_expansion().map_or_else(
            || left.to_string(),
            |(l, r)| match self.get_associativity(deltas, &r).unwrap() {
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
            |(l, r)| match self.get_associativity(deltas, &l).unwrap() {
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

    fn get_associativity(
        &self,
        deltas: &ContextDelta,
        ast: &Rc<SyntaxTree>,
    ) -> Option<Associativity> {
        let assoc_of_ast =
            self.combine(deltas, &self.to_ast(deltas, ASSOC), ast);
        ContextSearch::from((self, deltas)).reduce(&assoc_of_ast).and_then(
            |ast| match ast.get_concept() {
                Some(LEFT) => Some(Associativity::Left),
                Some(RIGHT) => Some(Associativity::Right),
                _ => None,
            },
        )
    }

    /// Returns the abstract syntax from two syntax parts, using the label and concept of the composition of associated concepts if it exists.
    pub fn contract_pair(
        &self,
        deltas: &ContextDelta,
        lefthand: &Rc<SyntaxTree>,
        righthand: &Rc<SyntaxTree>,
    ) -> Rc<SyntaxTree> {
        Rc::new(
            lefthand
                .get_concept()
                .and_then(|lc| {
                    righthand.get_concept().and_then(|rc| {
                        self.find_definition(deltas, lc, rc).and_then(|def| {
                            self.get_label(deltas, def).map(|label| {
                                label
                                    .parse::<SyntaxTree>()
                                    .unwrap()
                                    .bind_concept(def)
                            })
                        })
                    })
                })
                .unwrap_or_else(|| {
                    self.display_joint(deltas, lefthand, righthand)
                        .parse::<SyntaxTree>()
                        .unwrap()
                })
                .bind_pair(lefthand, righthand),
        )
    }

    pub fn check_reductions(
        &self,
        deltas: &ContextDelta,
        outer_concept: usize,
        inner_concept: usize,
    ) -> ZiaResult<()> {
        if let Some(r) =
            self.read_concept(deltas, inner_concept).get_reduction()
        {
            if r == outer_concept || self.contains(deltas, r, outer_concept) {
                Err(ZiaError::InfiniteDefinition)
            } else {
                self.check_reductions(deltas, outer_concept, r)
            }
        } else {
            Ok(())
        }
    }

    pub fn get_reduction_of_composition(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> usize {
        self.read_concept(deltas, concept)
            .get_definition()
            .and_then(|(left, right)| {
                self.find_definition(
                    deltas,
                    self.get_reduction_or_reduction_of_composition(
                        deltas, left,
                    ),
                    self.get_reduction_or_reduction_of_composition(
                        deltas, right,
                    ),
                )
            })
            .unwrap_or(concept)
    }

    fn get_reduction_or_reduction_of_composition(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> usize {
        self.read_concept(deltas, concept).get_reduction().unwrap_or_else(
            || self.get_reduction_of_composition(deltas, concept),
        )
    }
}

impl Apply for SnapShot {
    type Delta = ContextDelta;

    fn apply(&mut self, delta: ContextDelta) {
        delta.string.iter().for_each(|(s, sd)| match sd {
            StringDelta::Update {
                after,
                ..
            } => {
                self.string_map.insert(s.to_string(), *after);
            },
            StringDelta::Insert(id) => self.add_string(*id, s),
            StringDelta::Remove(_) => self.remove_string(s),
        });
        for (id, (cd, v, temporary)) in delta.concept {
            if !temporary {
                match cd {
                    ConceptDelta::Insert(c) => {
                        if self.concepts.len() <= id {
                            self.concepts
                                .extend(vec![None; id - self.concepts.len()]);
                            self.concepts.push(Some(c));
                        } else {
                            self.concepts[id] = Some(c);
                        }
                        if v {
                            self.variables.insert(id);
                        }
                    },
                    ConceptDelta::Remove(_) => {
                        self.blindly_remove_concept(id);
                        if v {
                            self.variables.remove(&id);
                        }
                    },
                    ConceptDelta::Update(d) => self.write_concept(id).apply(d),
                }
            }
        }
    }

    fn diff(&self, _other: Self) -> ContextDelta {
        ContextDelta {
            string: hashmap! {},
            concept: hashmap! {},
        }
    }
}

fn parse_line(buffer: &str) -> ZiaResult<Vec<String>> {
    let mut tokens: Vec<String> = [].to_vec();
    let mut token = String::new();
    let parenthesis_level = buffer.chars().try_fold(0, |p_level, letter| {
        parse_letter(letter, p_level, &mut token, &mut tokens)
    })?;
    if parenthesis_level != 0 {
        return Err(ZiaError::MissingSymbol {
            symbol: ")",
        });
    }
    if token != "" {
        tokens.push(token);
    }
    Ok(tokens)
}

fn parse_letter(
    letter: char,
    mut parenthesis_level: u8,
    token: &mut String,
    tokens: &mut Vec<String>,
) -> ZiaResult<u8> {
    match letter {
        '(' => {
            push_token(letter, parenthesis_level, token, tokens);
            Ok(parenthesis_level + 1)
        },
        ')' => {
            if parenthesis_level > 0 {
                parenthesis_level -= 1;
                push_token(letter, parenthesis_level, token, tokens);
                Ok(parenthesis_level)
            } else {
                Err(ZiaError::MissingSymbol {
                    symbol: "(",
                })
            }
        },
        ' ' => {
            push_token(letter, parenthesis_level, token, tokens);
            Ok(parenthesis_level)
        },
        '\n' | '\r' => Ok(parenthesis_level),
        _ => {
            token.push(letter);
            Ok(parenthesis_level)
        },
    }
}

fn push_token(
    letter: char,
    parenthesis_level: u8,
    token: &mut String,
    tokens: &mut Vec<String>,
) {
    if (token != "") & (parenthesis_level == 0) {
        tokens.push(token.clone());
        *token = String::new();
    }
    if parenthesis_level != 0 {
        token.push(letter);
    }
}
