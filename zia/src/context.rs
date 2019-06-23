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
use reading::ConceptReader;
use removing::{BlindConceptRemover, StringRemover};
use slog;
use slog::Drain;
use std::{fmt::Debug, collections::{HashMap, HashSet}};
use translating::StringConcept;
use writing::{
    ConceptWriter, SetAsDefinitionOfDelta, SetConceptDefinitionDeltas, SetDefinitionDelta,
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
}

impl<T> Logger for Context<T> {
    fn logger(&mut self) -> &mut slog::Logger {
        &mut self.logger
    }
}

#[derive(Clone, Debug)]
pub enum ContextDelta<T>
where
    T: Delta,
    T::Delta: Clone + Debug,
{
    String(String, StringDelta),
    Concept(usize, ConceptDelta<T>),
}

#[derive(Clone, Debug)]
pub enum StringDelta {
    Insert(usize),
    Remove,
}

#[derive(Clone, Debug)]
pub enum ConceptDelta<T>
where
    T: Delta,
    T::Delta: Clone + Debug,
{
    Insert(T),
    Remove,
    Update(T::Delta),
}

impl<T> Delta for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    type Delta = ContextDelta<T>;
    fn apply(&mut self, delta: &ContextDelta<T>) {
        match delta {
            ContextDelta::String(s, sd) => match sd {
                StringDelta::Insert(id) => {
                    info!(self.logger, "add_string({}, {})", id, &s);
                    self.add_string(*id, &s);
                },
                StringDelta::Remove => self.remove_string(&s),
            },
            ContextDelta::Concept(id, cd) => match cd {
                ConceptDelta::Insert(c) => {
                    self.add_concept(c.clone());
                }
                ConceptDelta::Remove => self.blindly_remove_concept(*id),
                ConceptDelta::Update(d) => self.write_concept(*id).apply(d),
            },
        };
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
        ContextDelta::String(string.to_string(), StringDelta::Insert(string_id))
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
        deltas: &Vec<Self::Delta>,
        concept: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<Vec<ContextDelta<T>>> {
        let mut new_deltas = deltas.clone();
        let concept_delta1 = try!(self
            .read_concept(&new_deltas, concept)
            .set_definition_delta(lefthand, righthand));
        new_deltas.push(ContextDelta::Concept(
            concept,
            ConceptDelta::Update(concept_delta1),
        ));
        let concept_delta2 = self
            .read_concept(&new_deltas, lefthand)
            .add_as_lefthand_of_delta(concept);
        new_deltas.push(ContextDelta::Concept(
            lefthand,
            ConceptDelta::Update(concept_delta2),
        ));
        let concept_delta3 = self
            .read_concept(&new_deltas, righthand)
            .add_as_righthand_of_delta(concept);
        new_deltas.push(ContextDelta::Concept(
            righthand,
            ConceptDelta::Update(concept_delta3),
        ));
        Ok(new_deltas)
    }
}

impl<T> ConceptReader<T> for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    fn read_concept(&self, deltas: &[ContextDelta<T>], id: usize) -> T {
        let mut concept_if_still_exists = match &self.concepts.get(id) {
            Some(Some(ref c)) => Some(c.clone()),
            _ => None,
        };
        for delta in deltas {
            if let ContextDelta::Concept(index, cd) = delta {
                if *index == id {
                    match cd {
                        ConceptDelta::Insert(c) => concept_if_still_exists = Some(c.clone()),
                        ConceptDelta::Remove => concept_if_still_exists = None,
                        ConceptDelta::Update(d) => {
                            if let Some(ref mut c) = concept_if_still_exists {
                                c.apply(d)
                            } else {
                                panic!("Deltas imply that a concept that doesn't exist will be updated!")
                            }
                        }
                    }
                }
            };
        }
        match concept_if_still_exists {
            Some(c) => c,
            None => panic!("No concept with id = {}", id),
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
        match self.string_map.remove(string) {
            Some(_) => (),
            None => panic!("No string to remove!"),
        };
    }
}

impl<T> ConceptAdder<T> for Context<T> {
    fn add_concept(&mut self, concept: T) -> usize {
        match self.gaps.pop() {
            None => {
                let index = self.concepts.len();
                self.concepts.push(Some(concept));
                index
            }
            Some(index) => {
                self.concepts[index] = Some(concept);
                index
            }
        }
    }
}

impl<T> ConceptAdderDelta<T> for Context<T>
where
    T: Delta + Clone,
    T::Delta: Clone + Debug,
{
    fn add_concept_deltas(
        &self,
        deltas: &[ContextDelta<T>],
        concept: T,
    ) -> (usize, Vec<ContextDelta<T>>) {
        let mut added_gaps = Vec::<usize>::new();
        let mut removed_gaps = HashSet::<usize>::new();
        let mut new_concept_length = self.concepts.len();
        for delta in deltas {
            match delta {
                ContextDelta::Concept(id, ConceptDelta::Insert(_)) => {
                    if *id < new_concept_length {
                        removed_gaps.insert(*id);
                    } else if *id == new_concept_length {
                        new_concept_length += 1
                    } else {
                        panic!("Deltas imply that a new concept has been given too large an id.")
                    }
                }
                ContextDelta::Concept(id, ConceptDelta::Remove) => {
                    added_gaps.push(*id);
                    removed_gaps.remove(&id);
                }
                _ => (),
            };
        }
        let index: usize;
        let mut gap_index = if self.gaps.is_empty() {
            None
        } else {
            Some(self.gaps.len() - 1)
        };
        loop {
            match added_gaps.pop() {
                Some(id) => {
                    if removed_gaps.contains(&id) {
                        continue;
                    } else {
                        index = id;
                        break;
                    }
                }
                None => match gap_index {
                    Some(gi) => {
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
                    None => {
                        index = new_concept_length;
                        break;
                    }
                },
            };
        }
        let mut new_deltas = deltas.to_vec();
        new_deltas.push(ContextDelta::Concept(index, ConceptDelta::Insert(concept)));
        (
            index,
            new_deltas,
        )
    }
}

impl<T> StringConcept for Context<T> {
    fn get_string_concept(&self, s: &str) -> Option<usize> {
        match self.string_map.get(s) {
            None => None,
            Some(sr) => Some(*sr),
        }
    }
}
