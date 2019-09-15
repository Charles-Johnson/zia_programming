/*  Library for the Zia programming language.
    Copyright (C) 2018 to 2019 Charles Johnson

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

use adding::{ConceptAdder, ConceptAdderDelta, StringAdder, StringAdderDelta};
use delta::Delta;
use errors::ZiaResult;
use logging::Logger;
use reading::{ConceptReader, Variable};
use removing::{
    BlindConceptRemover, BlindConceptRemoverDeltas, StringRemover, StringRemoverDeltas,
};
use slog;
use slog::Drain;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};
use translating::StringConcept;
use writing::{
    ConceptWriter, DeleteDefinition, MakeReduceFromDelta, RemoveConceptReduction,
    RemoveDefinitionDelta, RemoveReductionDelta, SetAsDefinitionOfDelta,
    SetConceptDefinitionDeltas, SetConceptReductionDelta, SetDefinitionDelta, SetReductionDelta,
};

/// A container for adding, reading, writing and removing concepts of generic type `T`.
pub struct Context<T> {
    /// Relates a String value to the index where the concept corresponding to the String is stored
    /// in the `concepts` field.
    string_map: HashMap<String, usize>,
    /// Concepts may be stored at an index of this vector as `Some(T)`. If that concept is removed
    /// from the context, `None` will be left at its index.
    concepts: Vec<Option<T>>,
    /// Keeps track of indices of the `concepts` field that have `None`.
    gaps: Vec<usize>,
    logger: slog::Logger,
    variables: HashSet<usize>,
}

impl<T> Context<T> {
    fn get_concept(&self, id: usize) -> Option<&T> {
        match self.concepts.get(id) {
            Some(Some(c)) => Some(c),
            _ => None,
        }
    }
}

impl<T> Variable for Context<T>
where
    T: Delta + Clone + Debug,
    T::Delta: Clone + Debug,
{
    fn has_variable(&self, deltas: &[ContextDelta<T>], concept: usize) -> bool {
        deltas
            .iter()
            .fold(self.variables.contains(&concept), |truth, delta| {
                delta
                    .concept
                    .get(&concept)
                    .map(|(cd, v)| match cd {
                        ConceptDelta::Insert(_) => *v,
                        ConceptDelta::Remove(_) => false,
                        ConceptDelta::Update(_) => truth,
                    })
                    .unwrap_or(truth)
            })
    }
}

impl<T> Logger for Context<T> {
    fn logger(&mut self) -> &mut slog::Logger {
        &mut self.logger
    }
}

#[derive(Clone, Debug)]
pub struct ContextDelta<T>
where
    T: Delta,
    T::Delta: Clone + Debug,
{
    string: HashMap<String, StringDelta>,
    concept: HashMap<usize, (ConceptDelta<T>, bool)>,
}

#[derive(Clone, Debug)]
pub enum StringDelta {
    Insert(usize),
    Remove(usize),
}

#[derive(Clone, Debug)]
pub enum ConceptDelta<T>
where
    T: Delta,
    T::Delta: Clone + Debug,
{
    Insert(T),
    Remove(T),
    Update(T::Delta),
}

impl<T> Delta for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    type Delta = ContextDelta<T>;
    fn apply(&mut self, delta: ContextDelta<T>) {
        delta.string.iter().for_each(|(s, sd)| match sd {
            StringDelta::Insert(id) => {
                info!(self.logger, "add_string({}, {})", id, &s);
                self.add_string(*id, &s);
            }
            StringDelta::Remove(_) => self.remove_string(&s),
        });
        for (id, (cd, v)) in delta.concept {
            match cd {
                ConceptDelta::Insert(c) => {
                    self.add_concept(c);
                    if v {
                        self.variables.insert(id);
                    }
                }
                ConceptDelta::Remove(_) => {
                    self.blindly_remove_concept(id);
                    if v {
                        self.variables.remove(&id);
                    }
                }
                ConceptDelta::Update(d) => self.write_concept(id).apply(d),
            }
        }
    }
}

impl<T> Default for Context<T> {
    fn default() -> Context<T> {
        let plain = slog_term::PlainSyncDecorator::new(slog_term::TestStdoutWriter);
        let logger = slog::Logger::root(slog_term::FullFormat::new(plain).build().fuse(), o!());
        Context::<T> {
            string_map: HashMap::new(),
            concepts: Vec::new(),
            gaps: Vec::new(),
            logger,
            variables: HashSet::new(),
        }
    }
}

impl<T> StringAdder for Context<T> {
    fn add_string(&mut self, string_id: usize, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }
}

impl<T> StringAdderDelta for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    fn add_string_delta(string_id: usize, string: &str) -> ContextDelta<T> {
        ContextDelta {
            string: hashmap! {string.to_string() => StringDelta::Insert(string_id)},
            concept: HashMap::default(),
        }
    }
}

impl<T> ConceptWriter<T> for Context<T> {
    fn write_concept(&mut self, id: usize) -> &mut T {
        match self.concepts[id] {
            Some(ref mut c) => c,
            None => panic!("No concept with id = {}", id),
        }
    }
}

impl<T> SetConceptDefinitionDeltas for Context<T>
where
    Self: ConceptReader<T> + Delta<Delta = ContextDelta<T>>,
    T: SetDefinitionDelta + SetAsDefinitionOfDelta + Clone,
    T::Delta: Clone + Debug,
{
    fn set_concept_definition_deltas(
        &self,
        deltas: &mut Vec<Self::Delta>,
        concept: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<()> {
        let concept_delta1 = try!(self
            .read_concept(deltas, concept)
            .set_definition_delta(lefthand, righthand));
        deltas.push(ContextDelta {
            concept: hashmap! {concept => (
                ConceptDelta::Update(concept_delta1),
                false, // Doesn't affect anything whether true or false
            )},
            string: hashmap! {},
        });
        let concept_delta2 = self
            .read_concept(deltas, lefthand)
            .add_as_lefthand_of_delta(concept);
        deltas.push(ContextDelta {
            concept: hashmap! {lefthand => (
                ConceptDelta::Update(concept_delta2),
                false, // Doesn't affect anything whether true or false
            )},
            string: hashmap! {},
        });
        let concept_delta3 = self
            .read_concept(deltas, righthand)
            .add_as_righthand_of_delta(concept);
        deltas.push(ContextDelta {
            concept: hashmap! {righthand => (
                ConceptDelta::Update(concept_delta3),
                false, // Doesn't affect anything whether true or false
            )},
            string: hashmap! {},
        });
        Ok(())
    }
}

impl<T> SetConceptReductionDelta for Context<T>
where
    T: Delta + SetReductionDelta + MakeReduceFromDelta + Clone,
    Self: ConceptReader<T> + Delta<Delta = ContextDelta<T>>,
    T::Delta: Clone + Debug,
{
    fn concept_reduction_deltas(
        &self,
        deltas: &mut Vec<Self::Delta>,
        concept: usize,
        reduction: usize,
    ) -> ZiaResult<()> {
        let concept_delta1 = self
            .read_concept(deltas, concept)
            .make_reduce_to_delta(reduction)?;
        let concept_delta2 = self
            .read_concept(deltas, reduction)
            .make_reduce_from_delta(concept);
        deltas.extend(vec![
            ContextDelta {
                concept: hashmap! {concept => (ConceptDelta::Update(concept_delta1), false)},
                string: hashmap! {},
            }, // Doesn't affect anything whether true or false
            ContextDelta {
                concept: hashmap! {reduction => (ConceptDelta::Update(concept_delta2), false)},
                string: hashmap! {},
            }, // Doesn't affect anything whether true or false
        ]);
        Ok(())
    }
}

impl<T> ConceptReader<T> for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    fn read_concept(&self, deltas: &[ContextDelta<T>], id: usize) -> T {
        deltas
            .iter()
            .fold(
                self.get_concept(id).cloned(),
                |concept_if_still_exists, delta| {
                    delta
                        .concept
                        .get(&id)
                        .map(|(cd, _)| match cd {
                            ConceptDelta::Insert(c) => Some(c.clone()),
                            ConceptDelta::Remove(_) => None,
                            ConceptDelta::Update(d) => {
                                let mut concept = concept_if_still_exists.clone().expect(
                                "Deltas imply that a concept that doesn't exist will be updated!",
                            );
                                concept.apply(d.clone());
                                Some(concept)
                            }
                        })
                        .unwrap_or(concept_if_still_exists)
                },
            )
            .unwrap_or_else(|| panic!("No concept with id = {}", id))
    }
}

impl<T> BlindConceptRemoverDeltas for Context<T>
where
    T: Delta + Clone + Debug,
    T::Delta: Clone + Debug,
{
    fn blindly_remove_concept_deltas(
        &self,
        deltas: &[ContextDelta<T>],
        id: usize,
    ) -> ContextDelta<T> {
        let concept = deltas
            .iter()
            .fold(self.get_concept(id), |c, delta| {
                delta
                    .concept
                    .get(&id)
                    .map(|(cd, _)| match cd {
                        ConceptDelta::Insert(c) => Some(c),
                        ConceptDelta::Remove(_) => None,
                        ConceptDelta::Update(_) => c,
                    })
                    .unwrap_or(c)
            })
            .expect("Concept will be already removed!");
        ContextDelta {
            concept: hashmap! {id => (
                ConceptDelta::Remove(concept.clone()),
                self.has_variable(deltas, id),
            )},
            string: hashmap! {},
        }
    }
}

impl<T> BlindConceptRemover for Context<T> {
    fn blindly_remove_concept(&mut self, id: usize) {
        self.concepts[id] = None;
        self.gaps.push(id);
    }
}

impl<T> StringRemover for Context<T> {
    fn remove_string(&mut self, string: &str) {
        self.string_map
            .remove(string)
            .expect("No string to remove!");
    }
}

impl<T> StringRemoverDeltas for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    fn remove_string_deltas(&self, deltas: &[ContextDelta<T>], string: &str) -> ContextDelta<T> {
        let index = deltas
            .iter()
            .fold(self.string_map.get(string), |string_index, delta| {
                delta
                    .string
                    .get(string)
                    .map(|sd| match sd {
                        StringDelta::Insert(index) => Some(index),
                        StringDelta::Remove(_) => None,
                    })
                    .unwrap_or(string_index)
            })
            .expect("string already removed or doesn't exist");
        ContextDelta {
            string: hashmap! {string.to_string() => StringDelta::Remove(*index)},
            concept: hashmap! {},
        }
    }
}

impl<T> ConceptAdder<T> for Context<T> {
    fn add_concept(&mut self, concept: T) {
        match self.gaps.pop() {
            Some(index) => self.concepts[index] = Some(concept),
            None => self.concepts.push(Some(concept)),
        };
    }
}

impl<T> ConceptAdderDelta<T> for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    fn add_concept_delta(
        &self,
        deltas: &[ContextDelta<T>],
        concept: T,
        variable: bool,
    ) -> (ContextDelta<T>, usize) {
        let mut added_gaps = Vec::<usize>::new();
        let mut removed_gaps = HashSet::<usize>::new();
        let mut new_concept_length = self.concepts.len();
        for delta in deltas {
            for (id, (cd, _)) in delta.concept.clone() {
                match cd {
                    ConceptDelta::Insert(_) => {
                        if id < new_concept_length {
                            removed_gaps.insert(id);
                        } else if id == new_concept_length {
                            new_concept_length += 1
                        } else {
                            panic!(
                                "Deltas imply that a new concept has been given too large an id."
                            )
                        }
                    }
                    ConceptDelta::Remove(_) => {
                        added_gaps.push(id);
                        removed_gaps.remove(&id);
                    }
                    ConceptDelta::Update(_) => (),
                }
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
                }
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
                }
                (None, None) => {
                    index = new_concept_length;
                    break;
                }
            };
        }
        (
            ContextDelta {
                concept: hashmap! {index => (ConceptDelta::Insert(concept), variable)},
                string: hashmap! {},
            },
            index,
        )
    }
}

impl<T> StringConcept for Context<T>
where
    T: Delta + Clone,
    T::Delta: Debug + Clone,
{
    fn get_string_concept(&self, deltas: &[ContextDelta<T>], s: &str) -> Option<usize> {
        deltas
            .iter()
            .fold(None, |candidate, delta| {
                delta
                    .string
                    .get(s)
                    .map(|string_delta| match string_delta {
                        StringDelta::Insert(concept) => Some(*concept),
                        StringDelta::Remove(_) => None,
                    })
                    .unwrap_or(candidate)
            })
            .or_else(|| self.string_map.get(s).cloned())
    }
}

impl<T> RemoveConceptReduction for Context<T>
where
    T: Delta + Clone + RemoveReductionDelta,
    T::Delta: Clone + Debug,
{
    fn remove_concept_reduction(
        &self,
        deltas: &[ContextDelta<T>],
        concept: usize,
        reduction: usize,
    ) -> [ContextDelta<T>; 2] {
        let mut edited_concept: Option<T> = Some(self.read_concept(deltas, concept));
        let mut edited_reduction: Option<T> = Some(self.read_concept(deltas, reduction));
        let mut concept_deltas = Vec::<T::Delta>::new();
        let mut reduction_deltas = Vec::<T::Delta>::new();
        for delta in deltas {
            delta.concept.get(&concept).map(|(cd, _)| match cd {
                ConceptDelta::Update(d) => concept_deltas.push(d.clone()),
                ConceptDelta::Insert(t) => edited_concept = Some(t.clone()),
                ConceptDelta::Remove(_) => {
                    edited_concept = None;
                    concept_deltas = vec![];
                }
            });
            delta.concept.get(&reduction).map(|(cd, _)| match cd {
                ConceptDelta::Update(d) => reduction_deltas.push(d.clone()),
                ConceptDelta::Insert(t) => edited_reduction = Some(t.clone()),
                ConceptDelta::Remove(_) => {
                    edited_reduction = None;
                    reduction_deltas = vec![];
                }
            });
        }
        [
            ContextDelta {
                concept: hashmap! {concept => (
                    ConceptDelta::Update(
                        edited_concept
                            .expect("Concept previously removed!")
                            .make_reduce_to_none_delta(&concept_deltas),
                    ),
                    false, // Doesn't affect anything whether true or false
                )},
                string: hashmap! {},
            },
            ContextDelta {
                concept: hashmap! {
                    reduction => (
                        ConceptDelta::Update(
                            edited_reduction
                                .expect("Reduction previously removed!")
                                .no_longer_reduces_from_delta(&reduction_deltas, concept),
                        ),
                        false, // Doesn't affect anything whether true or false
                    )
                },
                string: hashmap! {},
            },
        ]
    }
}

impl<T> DeleteDefinition<T> for Context<T>
where
    T: Delta + RemoveDefinitionDelta + Clone,
    T::Delta: Clone + Debug,
{
    fn delete_definition(
        &self,
        deltas: &[Self::Delta],
        concept: usize,
        left: usize,
        right: usize,
    ) -> [Self::Delta; 3] {
        let mut edited_concept: Option<T> = Some(self.read_concept(deltas, concept));
        let mut edited_left: Option<T> = Some(self.read_concept(deltas, left));
        let mut edited_right: Option<T> = Some(self.read_concept(deltas, right));
        let mut concept_deltas = Vec::<T::Delta>::new();
        let mut left_deltas = Vec::<T::Delta>::new();
        let mut right_deltas = Vec::<T::Delta>::new();
        for delta in deltas {
            delta.concept.get(&concept).map(|(cd, _)| match cd {
                ConceptDelta::Update(d) => concept_deltas.push(d.clone()),
                ConceptDelta::Insert(t) => edited_concept = Some(t.clone()),
                ConceptDelta::Remove(_) => {
                    edited_concept = None;
                    concept_deltas = vec![];
                }
            });
            delta.concept.get(&left).map(|(cd, _)| match cd {
                ConceptDelta::Update(d) => left_deltas.push(d.clone()),
                ConceptDelta::Insert(t) => edited_left = Some(t.clone()),
                ConceptDelta::Remove(_) => {
                    edited_left = None;
                    left_deltas = vec![];
                }
            });
            delta.concept.get(&right).map(|(cd, _)| match cd {
                ConceptDelta::Update(d) => right_deltas.push(d.clone()),
                ConceptDelta::Insert(t) => edited_right = Some(t.clone()),
                ConceptDelta::Remove(_) => {
                    edited_right = None;
                    right_deltas = vec![];
                }
            });
        }
        [
            ContextDelta {
                concept: hashmap! {concept => (
                    ConceptDelta::Update(
                        edited_concept
                            .expect("Concept previously removed!")
                            .remove_definition_delta(&concept_deltas),
                    ),
                    false, // Doesn't affect anything whether true or false
                )},
                string: hashmap! {},
            },
            ContextDelta {
                concept: hashmap! {left => (
                    ConceptDelta::Update(
                        edited_left
                            .expect("Left previously removed!")
                            .remove_as_lefthand_of_delta(&left_deltas, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                )},
                string: hashmap! {},
            },
            ContextDelta {
                concept: hashmap! {right => (
                    ConceptDelta::Update(
                        edited_right
                            .expect("Right previously removed!")
                            .remove_as_righthand_of_delta(&right_deltas, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                )},
                string: hashmap! {},
            },
        ]
    }
}
