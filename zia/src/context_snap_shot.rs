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
    concepts::Concept,
    constants::{
        ASSOC, DEFAULT, DEFINE, EXISTS_SUCH_THAT, FALSE, GREATER_THAN,
        IMPLICATION, LABEL, LEFT, LET, PRECEDENCE, REDUCTION, RIGHT, TRUE,
    },
    context_delta::{ConceptDelta, ContextDelta, StringDelta},
    delta::Apply,
    snap_shot::Reader as SnapShotReader,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
};

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

impl Associativity {
    pub fn display_joint_left(
        &self,
        leftleft: impl Display,
        leftright: impl Display,
    ) -> String {
        match &self {
            Self::Left => format!("{} {}", leftleft, leftright),
            Self::Right => format!("({} {})", leftleft, leftright),
        }
    }

    pub fn display_joint_right(
        &self,
        rightleft: impl Display,
        rightright: impl Display,
    ) -> String {
        match &self {
            Self::Left => format!("({} {})", rightleft, rightright),
            Self::Right => format!("{} {}", rightleft, rightright),
        }
    }

    pub fn slice_tokens<'a>(
        &self,
        tokens: &'a [String],
        prev_lp_index: Option<usize>,
        lp_index: usize,
    ) -> &'a [String] {
        match &self {
            Self::Left => match prev_lp_index {
                Some(i) => &tokens[i..lp_index],
                None => &tokens[..lp_index],
            },
            Self::Right => match prev_lp_index {
                Some(i) => &tokens[lp_index..i],
                None => &tokens[lp_index..],
            },
        }
    }
}

impl ContextSnapShot {
    fn write_concept(&mut self, id: usize) -> &mut Concept {
        match self.concepts[id] {
            Some(ref mut c) => c,
            None => panic!("No concept with id = {}", id),
        }
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

    fn add_string(&mut self, string_id: usize, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }

    fn blindly_remove_concept(&mut self, id: usize) {
        self.concepts[id] = None;
        self.gaps.push(id);
    }

    fn remove_string(&mut self, string: &str) {
        self.string_map.remove(string).expect("No string to remove!");
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
}

impl SnapShotReader for ContextSnapShot {
    fn true_id() -> usize {
        TRUE
    }

    fn implication_id() -> usize {
        IMPLICATION
    }

    fn precedence_id() -> usize {
        PRECEDENCE
    }

    fn default_id() -> usize {
        DEFAULT
    }

    fn greater_than_id() -> usize {
        GREATER_THAN
    }

    fn reduction_id() -> usize {
        REDUCTION
    }

    fn false_id() -> usize {
        FALSE
    }

    fn assoc_id() -> usize {
        ASSOC
    }

    fn right_id() -> usize {
        RIGHT
    }

    fn left_id() -> usize {
        LEFT
    }

    fn exists_such_that_id() -> usize {
        EXISTS_SUCH_THAT
    }

    fn define_id() -> usize {
        DEFINE
    }

    fn label_id() -> usize {
        LABEL
    }

    fn let_id() -> usize {
        LET
    }

    fn concept_from_label(
        &self,
        delta: &ContextDelta,
        s: &str,
    ) -> Option<usize> {
        self.get_string_concept(delta, s)
            .and_then(|c| self.get_labellee(delta, c))
    }

    fn get_concept(&self, id: usize) -> Option<&Concept> {
        match self.concepts.get(id) {
            Some(Some(c)) => Some(c),
            _ => None,
        }
    }

    fn lowest_unoccupied_concept_id(&self, delta: &ContextDelta) -> usize {
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
        index
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
