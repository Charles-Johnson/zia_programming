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
use delta::{ApplyDelta, Delta};
use errors::ZiaResult;
use logging::Logger;
use reading::{ConceptReader, Variable};
use removing::{BlindConceptRemover, BlindConceptRemoverDelta, StringRemover, StringRemoverDelta};
use slog;
use slog::Drain;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    default::Default,
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

fn update_concept_delta<T>(entry: Entry<usize, (ConceptDelta<T>, bool)>, concept_delta: T::Delta)
where
    T: ApplyDelta + Clone,
    T::Delta: Clone + Debug + Delta,
{
    entry
        .and_modify(|(cd, _)| match cd {
            ConceptDelta::Update(d) => {
                d.combine(concept_delta.clone());
                *cd = ConceptDelta::Update(d.clone());
            }
            ConceptDelta::Insert(c) => {
                c.apply(concept_delta.clone());
                *cd = ConceptDelta::Insert(c.clone());
            }
            ConceptDelta::Remove(_) => panic!("Concept will already be removed"),
        })
        .or_insert((ConceptDelta::Update(concept_delta), false));
}

impl<T> Variable for Context<T>
where
    T: ApplyDelta + Clone + Debug,
    T::Delta: Clone + Debug,
{
    fn has_variable(&self, delta: &ContextDelta<T>, concept: usize) -> bool {
        let in_previous_variables = self.variables.contains(&concept);
        delta
            .concept
            .get(&concept)
            .map(|(cd, v)| match cd {
                ConceptDelta::Insert(_) => *v,
                ConceptDelta::Remove(_) => false,
                ConceptDelta::Update(_) => in_previous_variables,
            })
            .unwrap_or(in_previous_variables)
    }
}

impl<T> Logger for Context<T> {
    fn logger(&mut self) -> &mut slog::Logger {
        &mut self.logger
    }
}

#[derive(Clone, Debug, Default)]
pub struct ContextDelta<T>
where
    T: ApplyDelta,
    T::Delta: Clone + Debug,
{
    string: HashMap<String, StringDelta>,
    concept: HashMap<usize, (ConceptDelta<T>, bool)>,
}

#[derive(Clone, Debug)]
pub enum StringDelta {
    Insert(usize),
    Remove(usize),
    Update { before: usize, after: usize },
}

#[derive(Clone, Debug)]
pub enum ConceptDelta<T>
where
    T: ApplyDelta,
    T::Delta: Clone + Debug,
{
    Insert(T),
    Remove(T),
    Update(T::Delta),
}

impl<T> Delta for ContextDelta<T>
where
    T: ApplyDelta + PartialEq + Clone + Debug,
    T::Delta: Clone + Debug + Delta,
{
    fn combine(&mut self, other: ContextDelta<T>) {
        for (other_key, (other_value, v2)) in other.concept {
            let mut remove_key = false;
            let mut update_delta = None;
            self.concept
                .entry(other_key)
                .and_modify(|(cd, v1)| match (cd, &other_value) {
                    (ConceptDelta::Insert(c1), ConceptDelta::Remove(c2))
                        if c1 == c2 && *v1 == v2 =>
                    {
                        remove_key = true;
                    }
                    (ConceptDelta::Remove(c1), ConceptDelta::Insert(c2)) => {
                        if c1 == c2 {
                            remove_key = true;
                        } else {
                            update_delta = Some(c1.diff(c2.clone()));
                        }
                    }
                    (ConceptDelta::Insert(c), ConceptDelta::Update(cd)) => {
                        c.apply(cd.clone());
                        *v1 = v2;
                    }
                    (ConceptDelta::Update(cd1), ConceptDelta::Update(cd2)) => {
                        cd1.combine(cd2.clone());
                        *v1 = v2;
                    }
                    _ => panic!("Something went wrong when combining concept deltas!"),
                })
                .or_insert((other_value, v2));
            if remove_key {
                self.concept.remove(&other_key);
            }
            update_delta.map(|cd| {
                self.concept
                    .insert(other_key, (ConceptDelta::Update(cd), v2))
            });
        }
        for (other_key, other_sd) in other.string {
            let mut remove_string = false;
            let mut sd_to_update = None;
            self.string
                .entry(other_key.clone())
                .and_modify(|sd| match (sd, &other_sd) {
                    (StringDelta::Insert(u1), StringDelta::Remove(u2)) if u1 == u2 => {
                        remove_string = true;
                    }
                    (StringDelta::Remove(u1), StringDelta::Insert(u2)) => {
                        if u1 == u2 {
                            remove_string = true;
                        } else {
                            sd_to_update = Some(StringDelta::Update {
                                before: *u1,
                                after: *u2,
                            });
                        }
                    }
                    (StringDelta::Insert(u), StringDelta::Update { before, after })
                        if u == before =>
                    {
                        *u = *after;
                    }
                    (
                        StringDelta::Update { after: a1, .. },
                        StringDelta::Update {
                            before: b2,
                            after: a2,
                        },
                    ) if a1 == b2 => {
                        *a1 = *a2;
                    }
                    _ => panic!("Something went wrong when combining string deltas!"),
                })
                .or_insert(other_sd);
            if remove_string {
                self.string.remove(&other_key);
            }
            sd_to_update.map(|sd| self.string.insert(other_key, sd));
        }
    }
}

impl<T> ApplyDelta for Context<T>
where
    T: ApplyDelta + Clone,
    T::Delta: Clone + Debug,
{
    type Delta = ContextDelta<T>;
    fn apply(&mut self, delta: ContextDelta<T>) {
        delta.string.iter().for_each(|(s, sd)| match sd {
            StringDelta::Update { after, .. } => {
                self.string_map.insert(s.to_string(), *after);
            }
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
    fn diff(&self, _other: Context<T>) -> ContextDelta<T> {
        ContextDelta {
            string: hashmap! {},
            concept: hashmap! {},
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
    T: ApplyDelta + Clone,
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
    Self: ConceptReader<T> + ApplyDelta<Delta = ContextDelta<T>>,
    T: SetDefinitionDelta + SetAsDefinitionOfDelta + Clone,
    T::Delta: Clone + Debug + Delta,
{
    fn set_concept_definition_deltas(
        &self,
        delta: &mut Self::Delta,
        concept: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<()> {
        let concept_delta = self
            .read_concept(delta, concept)
            .set_definition_delta(lefthand, righthand)?;
        update_concept_delta(delta.concept.entry(concept), concept_delta);
        let lefthand_delta = self
            .read_concept(delta, lefthand)
            .add_as_lefthand_of_delta(concept);
        update_concept_delta(delta.concept.entry(lefthand), lefthand_delta);
        let righthand_delta = self
            .read_concept(delta, righthand)
            .add_as_righthand_of_delta(concept);
        update_concept_delta(delta.concept.entry(righthand), righthand_delta);
        Ok(())
    }
}

impl<T> SetConceptReductionDelta for Context<T>
where
    T: ApplyDelta + SetReductionDelta + MakeReduceFromDelta + Clone,
    Self: ConceptReader<T> + ApplyDelta<Delta = ContextDelta<T>>,
    T::Delta: Clone + Debug + Delta,
{
    fn concept_reduction_deltas(
        &self,
        delta: &mut Self::Delta,
        concept: usize,
        reduction: usize,
    ) -> ZiaResult<()> {
        let concept_delta = self
            .read_concept(delta, concept)
            .make_reduce_to_delta(reduction)?;
        let reduction_delta = self
            .read_concept(delta, reduction)
            .make_reduce_from_delta(concept);
        update_concept_delta(delta.concept.entry(concept), concept_delta);
        update_concept_delta(delta.concept.entry(reduction), reduction_delta);
        Ok(())
    }
}

impl<T> ConceptReader<T> for Context<T>
where
    T: ApplyDelta + Clone,
    T::Delta: Clone + Debug,
{
    fn read_concept(&self, delta: &ContextDelta<T>, id: usize) -> T {
        delta
            .concept
            .get(&id)
            .and_then(|(cd, _)| match cd {
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
                    .expect(&format!("No concept with id = {}", id))
                    .clone()
            })
    }
}

impl<T> BlindConceptRemoverDelta for Context<T>
where
    T: ApplyDelta + Clone + Debug,
    T::Delta: Clone + Debug,
{
    fn blindly_remove_concept_delta(&self, delta: &mut ContextDelta<T>, id: usize) {
        let concept = delta
            .concept
            .get(&id)
            .and_then(|(cd, _)| match cd {
                ConceptDelta::Insert(c) => Some(c),
                ConceptDelta::Remove(_) => None,
                ConceptDelta::Update(_) => self.get_concept(id),
            })
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .expect("Concept will be already removed!")
            });
        delta.concept.insert(
            id,
            (
                ConceptDelta::Remove(concept.clone()),
                self.has_variable(delta, id),
            ),
        );
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

impl<T> StringRemoverDelta for Context<T>
where
    T: ApplyDelta + Clone,
    T::Delta: Clone + Debug,
{
    fn remove_string_delta(&self, delta: &mut ContextDelta<T>, string: &str) {
        let index = delta
            .string
            .get(string)
            .and_then(|sd| match sd {
                StringDelta::Update { after, .. } => Some(after),
                StringDelta::Insert(index) => Some(index),
                StringDelta::Remove(_) => None,
            })
            .expect("string already removed or doesn't exist");
        delta
            .string
            .insert(string.to_string(), StringDelta::Remove(*index));
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
    T: ApplyDelta + Clone,
    T::Delta: Clone + Debug,
{
    fn add_concept_delta(
        &self,
        delta: &ContextDelta<T>,
        concept: T,
        variable: bool,
    ) -> (ContextDelta<T>, usize) {
        let mut added_gaps = Vec::<usize>::new();
        let mut removed_gaps = HashSet::<usize>::new();
        let mut new_concept_length = self.concepts.len();
        for (id, (cd, _)) in &delta.concept {
            match cd {
                ConceptDelta::Insert(_) => {
                    if *id >= new_concept_length {
                        new_concept_length = *id + 1
                    }
                }
                _ => (),
            };
        }
        for (id, (cd, _)) in &delta.concept {
            match cd {
                ConceptDelta::Insert(_) => {
                    removed_gaps.insert(*id);
                }
                ConceptDelta::Remove(_) => {
                    added_gaps.push(*id);
                    removed_gaps.remove(id);
                }
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
    T: ApplyDelta + Clone,
    T::Delta: Debug + Clone,
{
    fn get_string_concept(&self, delta: &ContextDelta<T>, s: &str) -> Option<usize> {
        delta
            .string
            .get(s)
            .map(|string_delta| match string_delta {
                StringDelta::Update { after, .. } => Some(after),
                StringDelta::Insert(concept) => Some(concept),
                StringDelta::Remove(_) => None,
            })
            .unwrap_or_else(|| self.string_map.get(s))
            .cloned()
    }
}

impl<T> RemoveConceptReduction for Context<T>
where
    T: ApplyDelta + Clone + RemoveReductionDelta,
    T::Delta: Clone + Debug + Default + Delta,
{
    fn remove_concept_reduction(
        &self,
        delta: &ContextDelta<T>,
        concept: usize,
        reduction: usize,
    ) -> ContextDelta<T> {
        let mut edited_concept: Option<T> = Some(self.read_concept(delta, concept));
        let mut concept_delta = T::Delta::default();
        delta.concept.get(&concept).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => concept_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_concept = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_concept = None;
                concept_delta = T::Delta::default();
            }
        });
        let mut edited_reduction: Option<T> = Some(self.read_concept(delta, reduction));
        let mut reduction_delta = T::Delta::default();
        delta.concept.get(&reduction).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => reduction_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_reduction = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_reduction = None;
                reduction_delta = T::Delta::default();
            }
        });
        ContextDelta {
            concept: hashmap! {
                concept => (
                    ConceptDelta::Update(
                        edited_concept
                            .expect("Concept previously removed!")
                            .make_reduce_to_none_delta(&concept_delta),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
                reduction => (
                    ConceptDelta::Update(
                        edited_reduction
                            .expect("Reduction previously removed!")
                            .no_longer_reduces_from_delta(&reduction_delta, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
            },
            string: hashmap! {},
        }
    }
}

impl<T> DeleteDefinition<T> for Context<T>
where
    T: ApplyDelta + RemoveDefinitionDelta + Clone,
    T::Delta: Clone + Debug + Default + Delta,
{
    fn delete_definition(
        &self,
        delta: &Self::Delta,
        concept: usize,
        left: usize,
        right: usize,
    ) -> Self::Delta {
        let mut edited_concept: Option<T> = Some(self.read_concept(delta, concept));
        let mut edited_left: Option<T> = Some(self.read_concept(delta, left));
        let mut edited_right: Option<T> = Some(self.read_concept(delta, right));
        let mut concept_delta = T::Delta::default();
        let mut left_delta = T::Delta::default();
        let mut right_delta = T::Delta::default();
        delta.concept.get(&concept).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => concept_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_concept = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_concept = None;
                concept_delta = T::Delta::default();
            }
        });
        delta.concept.get(&left).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => left_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_left = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_left = None;
                left_delta = T::Delta::default();
            }
        });
        delta.concept.get(&right).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => right_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_right = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_right = None;
                right_delta = T::Delta::default();
            }
        });
        ContextDelta {
            concept: hashmap! {
                concept => (
                    ConceptDelta::Update(
                        edited_concept
                            .expect("Concept previously removed!")
                            .remove_definition_delta(&concept_delta),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
                left => (
                    ConceptDelta::Update(
                        edited_left
                            .expect("Left previously removed!")
                            .remove_as_lefthand_of_delta(&left_delta, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
                right => (
                    ConceptDelta::Update(
                        edited_right
                            .expect("Right previously removed!")
                            .remove_as_righthand_of_delta(&right_delta, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
            },
            string: hashmap! {},
        }
    }
}
