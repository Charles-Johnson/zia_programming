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
    concepts::{Concept, SpecificPart},
    constants::{IMPLICATION, LABEL, TRUE},
    context_delta::{ConceptDelta, ContextDelta, StringDelta},
    delta::Apply,
    errors::{ZiaError, ZiaResult},
    snap_shot::Reader as SnapShotReader,
};
use maplit::hashmap;
use std::collections::{HashMap, HashSet, VecDeque};

/// A container for adding, reading, writing and removing concepts of generic type `T`.
#[derive(Default, Debug, Clone)]
pub struct ContextSnapShot {
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
pub enum Associativity {
    Left,
    Right,
}

impl ContextSnapShot {
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

    pub fn add_concept_delta(
        &self,
        delta: &ContextDelta,
        concept_type: SpecificPart,
        variable: bool,
    ) -> (ContextDelta, usize) {
        let mut added_gaps = Vec::<usize>::new();
        let mut removed_gaps = HashSet::<usize>::new();
        let mut new_concept_length = self.concepts.len();
        for (id, (cd, _, _)) in delta.concept() {
            if let ConceptDelta::Insert(_) = cd {
                if *id >= new_concept_length {
                    new_concept_length = *id + 1
                }
            }
        }
        for (id, (cd, _, _)) in delta.concept() {
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
            ContextDelta::new(
                hashmap! {},
                hashmap! {index => (ConceptDelta::Insert((concept_type, index).into()), variable, false)},
            ),
            index,
        )
    }

    pub fn add_string_delta(string_id: usize, string: &str) -> ContextDelta {
        ContextDelta::new(
            hashmap! {string.to_string() => StringDelta::Insert(string_id)},
            HashMap::default(),
        )
    }

    fn add_string(&mut self, string_id: usize, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }

    pub fn get_normal_form(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> Option<usize> {
        self.read_concept(delta, concept)
            .get_reduction()
            .map(|n| self.get_normal_form(delta, n).unwrap_or(n))
    }

    pub fn get_concept_of_label(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> Option<usize> {
        self.read_concept(delta, concept)
            .get_righthand_of()
            .iter()
            .find(|candidate| {
                self.read_concept(delta, **candidate)
                    .get_definition()
                    .expect("Candidate should have a definition!")
                    .0
                    == LABEL
            })
            .cloned()
    }

    pub fn is_disconnected(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> bool {
        self.read_concept(delta, concept).get_reduction().is_none()
            && self.read_concept(delta, concept).get_definition().is_none()
            && self.read_concept(delta, concept).get_lefthand_of().is_empty()
            && self.righthand_of_without_label_is_empty(delta, concept)
            && self
                .read_concept(delta, concept)
                .find_what_reduces_to_it()
                .next()
                .is_none()
    }

    fn righthand_of_without_label_is_empty(
        &self,
        delta: &ContextDelta,
        con: usize,
    ) -> bool {
        self.read_concept(delta, con)
            .get_righthand_of()
            .iter()
            .find_map(|concept| {
                self.read_concept(delta, *concept)
                    .get_definition()
                    .filter(|(left, _)| *left != LABEL)
            })
            .is_none()
    }

    fn blindly_remove_concept(&mut self, id: usize) {
        self.concepts[id] = None;
        self.gaps.push(id);
    }

    fn remove_string(&mut self, string: &str) {
        self.string_map.remove(string).expect("No string to remove!");
    }

    pub fn concept_from_label(
        &self,
        delta: &ContextDelta,
        s: &str,
    ) -> Option<usize> {
        self.get_string_concept(delta, s)
            .and_then(|c| self.get_labellee(delta, c))
    }

    fn get_string_concept(
        &self,
        delta: &ContextDelta,
        s: &str,
    ) -> Option<usize> {
        delta
            .string()
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
        delta: &ContextDelta,
        outer: usize,
        inner: usize,
    ) -> bool {
        if let Some((left, right)) =
            self.read_concept(delta, outer).get_definition()
        {
            left == inner
                || right == inner
                || self.contains(delta, left, inner)
                || self.contains(delta, right, inner)
        } else {
            false
        }
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

    pub fn check_reductions(
        &self,
        delta: &ContextDelta,
        outer_concept: usize,
        inner_concept: usize,
    ) -> ZiaResult<()> {
        if let Some(r) = self.read_concept(delta, inner_concept).get_reduction()
        {
            if r == outer_concept || self.contains(delta, r, outer_concept) {
                Err(ZiaError::InfiniteDefinition)
            } else {
                self.check_reductions(delta, outer_concept, r)
            }
        } else {
            Ok(())
        }
    }

    pub fn get_reduction_of_composition(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> usize {
        self.read_concept(delta, concept)
            .get_definition()
            .and_then(|(left, right)| {
                self.find_definition(
                    delta,
                    self.get_reduction_or_reduction_of_composition(delta, left),
                    self.get_reduction_or_reduction_of_composition(
                        delta, right,
                    ),
                )
            })
            .unwrap_or(concept)
    }

    fn get_reduction_or_reduction_of_composition(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> usize {
        self.read_concept(delta, concept).get_reduction().unwrap_or_else(|| {
            self.get_reduction_of_composition(delta, concept)
        })
    }
}

impl SnapShotReader for ContextSnapShot {
    fn true_id() -> usize {
        TRUE
    }
    fn implication_id() -> usize {
        IMPLICATION
    }
    fn concept_len(&self, delta: &ContextDelta) -> usize {
        let mut length = self.concepts.len();
        for (id, (cd, _, _)) in delta.concept() {
            if let ConceptDelta::Insert(_) = cd {
                if length <= *id {
                    length = *id + 1;
                }
            }
        }
        length
    }

    fn get_label(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> Option<String> {
        match self.get_concept_of_label(delta, concept) {
            None => self
                .read_concept(delta, concept)
                .get_reduction()
                .and_then(|r| self.get_label(delta, r)),
            Some(d) => self
                .get_normal_form(delta, d)
                .and_then(|n| self.read_concept(delta, n).get_string()),
        }
    }

    fn find_definition(
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

    fn ast_from_symbol(&self, delta: &ContextDelta, s: &str) -> SyntaxTree {
        self.concept_from_label(delta, s).map_or_else(
            || s.into(),
            |concept| SyntaxTree::from(s).bind_concept(concept),
        )
    }

    fn read_concept(&self, delta: &ContextDelta, id: usize) -> Concept {
        delta
            .concept().get(&id)
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

    fn has_variable(&self, delta: &ContextDelta, concept: usize) -> bool {
        let in_previous_variables = self.variables.contains(&concept);
        delta.concept().get(&concept).map_or(
            in_previous_variables,
            |(cd, v, _)| match cd {
                ConceptDelta::Insert(_) => *v,
                ConceptDelta::Remove(_) => false,
                ConceptDelta::Update(_) => in_previous_variables,
            },
        )
    }
}

impl Apply for ContextSnapShot {
    type Delta = ContextDelta;

    fn apply(&mut self, delta: ContextDelta) {
        delta.string().iter().for_each(|(s, sd)| match sd {
            StringDelta::Update {
                after,
                ..
            } => {
                self.string_map.insert(s.to_string(), *after);
            },
            StringDelta::Insert(id) => self.add_string(*id, s),
            StringDelta::Remove(_) => self.remove_string(s),
        });
        let concept_len = self.concept_len(&delta);
        if concept_len > self.concepts.len() {
            self.concepts.extend(vec![None; concept_len - self.concepts.len()]);
        }
        for (id, (cd, v, temporary)) in delta.concept() {
            if !temporary {
                match cd {
                    ConceptDelta::Insert(c) => {
                        self.concepts[*id] = Some(c.clone());
                        if *v {
                            self.variables.insert(*id);
                        }
                    },
                    ConceptDelta::Remove(_) => {
                        self.blindly_remove_concept(*id);
                        if *v {
                            self.variables.remove(id);
                        }
                    },
                    ConceptDelta::Update(d) => {
                        self.write_concept(*id).apply(d.clone())
                    },
                }
            }
        }
    }

    fn diff(&self, _other: Self) -> ContextDelta {
        ContextDelta::default()
    }
}

pub fn parse_line(buffer: &str) -> ZiaResult<Vec<String>> {
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
