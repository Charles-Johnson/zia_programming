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
use std::collections::{HashMap, HashSet};
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

pub enum ContextDelta<T>
where
    T: Delta,
{
    String(String, StringDelta),
    Concept(usize, ConceptDelta<T>),
}

pub enum StringDelta {
    Insert(usize),
    Remove,
}

pub enum ConceptDelta<T>
where
    T: Delta,
{
    Insert(T),
    Remove,
    Update(T::Delta),
}

impl<T> Delta for Context<T>
where
    T: Delta + Clone,
{
    type Delta = ContextDelta<T>;
    fn apply(&mut self, delta: &ContextDelta<T>) {
        match delta {
            ContextDelta::<T>::String(s, sd) => match sd {
                StringDelta::Insert(id) => self.add_string(*id, &s),
                StringDelta::Remove => self.remove_string(&s),
            },
            ContextDelta::<T>::Concept(id, cd) => match cd {
                ConceptDelta::<T>::Insert(c) => {
                    self.add_concept(c.clone());
                }
                ConceptDelta::<T>::Remove => self.blindly_remove_concept(*id),
                ConceptDelta::<T>::Update(d) => self.write_concept(*id).apply(d),
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
{
    fn add_string_delta(string_id: usize, string: &str) -> ContextDelta<T> {
        ContextDelta::<T>::String(string.to_string(), StringDelta::Insert(string_id))
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
{
    fn set_concept_definition_deltas(
        &self,
        concept: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<Vec<ContextDelta<T>>> {
        let mut deltas = Vec::<ContextDelta<T>>::new();
        let concept_delta1 = try!(self
            .read_concept(&deltas, concept)
            .set_definition_delta(lefthand, righthand));
        deltas.push(ContextDelta::<T>::Concept(
            concept,
            ConceptDelta::<T>::Update(concept_delta1),
        ));
        let concept_delta2 = self
            .read_concept(&deltas, lefthand)
            .add_as_lefthand_of_delta(concept);
        deltas.push(ContextDelta::<T>::Concept(
            lefthand,
            ConceptDelta::<T>::Update(concept_delta2),
        ));
        let concept_delta3 = self
            .read_concept(&deltas, righthand)
            .add_as_righthand_of_delta(concept);
        deltas.push(ContextDelta::<T>::Concept(
            righthand,
            ConceptDelta::<T>::Update(concept_delta3),
        ));
        Ok(deltas)
    }
}

impl<T> ConceptReader<T> for Context<T>
where
    T: Delta + Clone,
{
    fn read_concept(&self, deltas: &[ContextDelta<T>], id: usize) -> T {
        let mut concept_if_still_exists = match &self.concepts[id] {
            Some(c) => Some(c.clone()),
            None => None,
        };
        for delta in deltas {
            if let ContextDelta::<T>::Concept(index, cd) = delta {
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
{
    fn add_concept_delta(
        &self,
        deltas: &[ContextDelta<T>],
        concept: T,
    ) -> (usize, ContextDelta<T>) {
        let mut added_gaps = Vec::<usize>::new();
        let mut removed_gaps = HashSet::<usize>::new();
        let mut new_concept_length = self.concepts.len();
        for delta in deltas {
            match delta {
                ContextDelta::<T>::Concept(id, ConceptDelta::<T>::Insert(_)) => {
                    if *id < new_concept_length {
                        removed_gaps.insert(*id);
                    } else if *id == new_concept_length {
                        new_concept_length += 1
                    } else {
                        panic!("Deltas imply that a new concept has been given too large an id.")
                    }
                }
                ContextDelta::<T>::Concept(id, ConceptDelta::<T>::Remove) => {
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
        (
            index,
            ContextDelta::<T>::Concept(index, ConceptDelta::Insert(concept)),
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
