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
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta,
    context_delta::{
        Change, Composition, ConceptDelta, ContextDelta, DirectConceptDelta,
        NewConceptDelta, NewDirectConceptDelta,
    },
    delta::Apply,
    snap_shot::Reader as SnapShotReader,
};
use assert_matches::assert_matches;
use bimap::BiMap;
use generic_array::{
    arr, functional::FunctionalSequence, ArrayLength, GenericArray,
};
use maplit::hashset;
use std::collections::{HashMap, HashSet, VecDeque};

type ConceptId = usize;

/// A container for adding, reading, writing and removing concepts of generic type `T`.
#[derive(Default, Debug, Clone)]
pub struct ContextSnapShot {
    /// Relates a String value to the index where the concept corresponding to the String is stored
    /// in the `concepts` field.
    string_map: HashMap<String, ConceptId>,
    /// Concepts may be stored at an index of this vector as `Some(T)`. If that concept is removed
    /// from the context, `None` will be left at its index.
    concepts: Vec<Option<Concept<ConceptId>>>,
    /// Keeps track of indices of the `concepts` field that have `None`.
    gaps: Vec<ConceptId>,
    concrete_concepts: BiMap<ConceptId, ConcreteConceptType>,
}

impl ContextSnapShot {
    fn write_concept(&mut self, id: ConceptId) -> &mut Concept<ConceptId> {
        match self.concepts.get_mut(id) {
            Some(Some(ref mut c)) => c,
            _ => panic!("No concept with id = {}", id),
        }
    }

    fn apply_new_concept(&mut self, ndcd: &NewDirectConceptDelta<ConceptId>) {
        let NewDirectConceptDelta {
            new_concept_id,
            delta,
        } = ndcd;
        match delta {
            NewConceptDelta::FreeVariable => {
                self.concepts[*new_concept_id] =
                    Some(Concept::make_free_variable(*new_concept_id))
            }
            NewConceptDelta::BoundVariable => {
                self.concepts[*new_concept_id] =
                    Some(Concept::make_bound_variable(*new_concept_id))
            }
            NewConceptDelta::String(s) => {
                self.string_map.insert(s.into(), *new_concept_id);
                self.concepts[*new_concept_id] = Some(
                    (SpecificPart::String(s.into()), *new_concept_id).into(),
                );
            }
            NewConceptDelta::Composition(c) => {
                let [left, right]: [&mut Concept<ConceptId>; 2] = self
                    .write_concepts(arr![ConceptId; c.left_id, c.right_id])
                    .into();
                self.concepts[*new_concept_id] =
                    Some(Concept::composition_of(*new_concept_id, left, right));
            }
            NewConceptDelta::Left {
                composition_id,
                right_id,
                concrete_type,
            } => {
                let [right, composition]: [&mut Concept<ConceptId>; 2] = self
                    .write_concepts(arr![ConceptId; *right_id, *composition_id])
                    .into();
                self.concepts[*new_concept_id] = Some(
                    Concept::lefthand_of(
                        *new_concept_id,
                        right,
                        composition,
                        *concrete_type,
                    )
                    .unwrap(),
                );
                if let Some(cct) = concrete_type {
                    self.concrete_concepts.insert(*new_concept_id, *cct);
                }
            }
            NewConceptDelta::Double {
                composition_id,
                concrete_type,
                ..
            } => {
                let composition = self.write_concept(*composition_id);
                debug_assert!(new_concept_id != composition_id);
                self.concepts[*new_concept_id] = Some(Concept::double(
                    *new_concept_id,
                    composition,
                    *concrete_type,
                ));
                if let Some(cct) = concrete_type {
                    self.concrete_concepts.insert(*new_concept_id, *cct);
                }
            }
            NewConceptDelta::Right {
                composition_id,
                left_id,
                concrete_type,
            } => {
                let [left, composition]: [&mut Concept<ConceptId>; 2] = self
                    .write_concepts(arr![ConceptId; *left_id, *composition_id])
                    .into();
                debug_assert!(new_concept_id != left_id);
                debug_assert!(new_concept_id != composition_id);
                self.concepts[*new_concept_id] = Some(
                    Concept::righthand_of(
                        *new_concept_id,
                        left,
                        composition,
                        *concrete_type,
                    )
                    .unwrap(),
                );
                if let Some(cct) = concrete_type {
                    self.concrete_concepts.insert(*new_concept_id, *cct);
                }
            }
            NewConceptDelta::ReducesTo {
                reduction,
            } => {
                debug_assert!(new_concept_id != reduction);
                let reduction_concept = self.write_concept(*reduction);
                self.concepts[*new_concept_id] = Some(Concept::reduction_to(
                    *new_concept_id,
                    reduction_concept,
                ));
            }
        }
    }

    fn write_concepts<
        'a,
        N: ArrayLength<ConceptId> + ArrayLength<&'a mut Concept<ConceptId>>,
    >(
        &'a mut self,
        ids: GenericArray<ConceptId, N>,
    ) -> GenericArray<&'a mut Concept<ConceptId>, N> {
        assert_matches!(
            ids.iter().try_fold(hashset! {}, |mut acc, id| {
                if acc.contains(id) {
                    Err("Duplicate concept ID") // Prevent multiple &mut to same element to avoid UB
                } else if *id < self.concepts.len() {
                    acc.insert(*id);
                    Ok(acc)
                } else {
                    Err("Concept ID greater than largest stored ID") // Prevent buffer over-read for memory safety
                }
            }),
            Ok(_)
        );
        let ptr = self.concepts.as_mut_ptr();
        ids.map(|x| unsafe {
            ptr.add(x)
                .as_mut()
                .expect("Null pointer")
                .as_mut()
                .expect("some concepts are empty")
        })
    }

    fn concept_len(&self, delta: &ContextDelta<ConceptId>) -> ConceptId {
        let mut length = self.concepts.len();
        for (id, cdv) in delta.concept() {
            for dcd in cdv.iter().filter_map(ConceptDelta::try_direct) {
                if let DirectConceptDelta::New(_) = dcd.as_ref() {
                    if length <= *id {
                        length = *id + 1;
                    }
                }
            }
        }
        length
    }

    fn add_string(&mut self, string_id: ConceptId, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }

    fn remove_string(&mut self, string: &str) {
        self.string_map.remove(string).expect("No string to remove!");
    }

    fn get_string_concept(
        &self,
        delta: &ContextDelta<ConceptId>,
        s: &str,
    ) -> Option<ConceptId> {
        delta
            .string()
            .get(s)
            .map_or_else(
                || self.string_map.get(s),
                |string_delta| match string_delta {
                    context_delta::Change::Update {
                        after,
                        before,
                    } => {
                        debug_assert_eq!(self.string_map.get(s), Some(before));
                        Some(after)
                    }
                    context_delta::Change::Create(concept) => Some(concept),
                    context_delta::Change::Remove(before) => {
                        debug_assert_eq!(self.string_map.get(s), Some(before));
                        None
                    }
                },
            )
            .cloned()
    }

    fn get_labellee(
        &self,
        delta: &ContextDelta<ConceptId>,
        c: ConceptId,
    ) -> Option<ConceptId> {
        let concept = self.read_concept(delta, c);
        let mut candidates: VecDeque<ConceptId> =
            concept.find_what_reduces_to_it().copied().collect();
        loop {
            if let Some(candidate) = candidates.pop_front() {
                let candidate_concept = self.read_concept(delta, candidate);
                if let Some((r, x)) = candidate_concept.get_composition() {
                    if self.concrete_concept_type(delta, r)
                        == Some(ConcreteConceptType::Label)
                    {
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
    type ConceptId = ConceptId;
    fn concept_from_label(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        s: &str,
    ) -> Option<Self::ConceptId> {
        self.get_string_concept(delta, s)
            .and_then(|c| self.get_labellee(delta, c))
    }

    fn get_concept(
        &self,
        id: Self::ConceptId,
    ) -> Option<&Concept<Self::ConceptId>> {
        match self.concepts.get(id) {
            Some(Some(c)) => Some(c),
            _ => None,
        }
    }

    fn lowest_unoccupied_concept_id(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
    ) -> Self::ConceptId {
        let mut added_gaps = Vec::<Self::ConceptId>::new();
        let mut removed_gaps = HashSet::<Self::ConceptId>::new();
        let mut new_concept_length = self.concepts.len();
        for (id, cdv) in delta.concept() {
            for dcd in cdv.iter().filter_map(ConceptDelta::try_direct) {
                if let DirectConceptDelta::New(_) = dcd.as_ref() {
                    if *id >= new_concept_length {
                        new_concept_length = *id + 1
                    }
                }
            }
        }
        for (id, cdv) in delta.concept() {
            for dcd in cdv.iter().filter_map(ConceptDelta::try_direct) {
                match dcd.as_ref() {
                    DirectConceptDelta::New {
                        ..
                    } => {
                        removed_gaps.insert(*id);
                    }
                    DirectConceptDelta::Remove(_) => {
                        added_gaps.push(*id);
                        removed_gaps.remove(id);
                    }
                    _ => (),
                }
            }
        }
        let index: Self::ConceptId;
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
                    }
                }
                (None, Some(gi)) => {
                    if removed_gaps.contains(&self.gaps[gi]) {
                        if gi == 0 {
                            index = new_concept_length;
                            break;
                        }
                        gap_index = Some(gi - 1);
                        continue;
                    }
                }
                (None, None) => {
                    index = new_concept_length;
                    break;
                }
            };
        }
        index
    }

    fn get_label(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept: Self::ConceptId,
    ) -> Option<String> {
        self.get_concept_of_label(delta, concept).map_or_else(
            || {
                self.read_concept(delta, concept)
                    .get_reduction()
                    .and_then(|r| self.get_label(delta, r))
            },
            |d| {
                self.get_normal_form(delta, d)
                    .and_then(|n| self.read_concept(delta, n).get_string())
            },
        )
    }

    fn concrete_concept_id(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        cc: ConcreteConceptType,
    ) -> Option<Self::ConceptId> {
        let mut id = None;
        for (concept_id, cdv) in
            delta.concept().iter().filter(|(concept_id, _)| {
                self.concrete_concept_type(delta, **concept_id) == Some(cc)
            })
        {
            for dcd in cdv.iter().filter_map(ConceptDelta::try_direct) {
                match dcd.as_ref() {
                    DirectConceptDelta::New(_) => id = Some(Some(*concept_id)),
                    DirectConceptDelta::Remove(concept_id) => {
                        debug_assert_eq!(Some(Some(*concept_id)), id);
                        id = Some(None)
                    }
                    _ => (),
                }
            }
        }
        id.unwrap_or_else(|| self.concrete_concepts.get_by_right(&cc).cloned())
    }

    fn concrete_concept_type(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.read_concept(delta, concept_id).get_concrete_concept_type()
    }

    #[cfg(test)]
    fn new_test_case(
        _: &[Concept<Self::ConceptId>],
        _: &HashMap<Self::ConceptId, &'static str>,
    ) -> Self {
        unimplemented!()
    }
}

impl Apply for ContextSnapShot {
    type Delta = ContextDelta<ConceptId>;

    #[allow(clippy::clippy::too_many_lines)]
    fn apply(&mut self, delta: ContextDelta<ConceptId>) {
        delta.string().iter().for_each(|(s, sd)| match sd {
            context_delta::Change::Update {
                after,
                before,
            } => {
                debug_assert_eq!(self.string_map.get(s), Some(before));
                self.string_map.insert(s.to_string(), *after);
            }
            context_delta::Change::Create(id) => self.add_string(*id, s),
            context_delta::Change::Remove(before) => {
                debug_assert_eq!(self.string_map.get(s), Some(before));
                self.remove_string(s)
            }
        });
        let concept_len = self.concept_len(&delta);
        if concept_len > self.concepts.len() {
            self.concepts.extend(vec![None; concept_len - self.concepts.len()]);
        }
        for (concept_id, concept_delta) in delta.concepts_to_apply_in_order() {
            match concept_delta.as_ref() {
                DirectConceptDelta::New(delta) => {
                    self.apply_new_concept(&NewDirectConceptDelta {
                        delta: delta.clone(),
                        new_concept_id: *concept_id,
                    })
                }
                DirectConceptDelta::Compose {
                    change,
                    composition_id,
                } => {
                    debug_assert_eq!(concept_id, composition_id);
                    match change {
                        Change::Create(Composition {
                            left_id,
                            right_id,
                        }) => {
                            let [composition, left, right]: [&mut Concept<ConceptId>; 3] = self.write_concepts(arr![ConceptId; *composition_id, *left_id, *right_id]).into();
                            composition
                                .change_composition(Change::Create([
                                    left, right,
                                ]))
                                .unwrap();
                        }
                        Change::Update {
                            before:
                                Composition {
                                    left_id,
                                    right_id,
                                },
                            after:
                                Composition {
                                    left_id: after_left_id,
                                    right_id: after_right_id,
                                },
                        } => {
                            let [composition, before_left, before_right, after_left, after_right]: [&mut Concept<ConceptId>; 5] = self.write_concepts(arr![ConceptId; *composition_id, *left_id, *right_id, *after_left_id, *after_right_id]).into();
                            composition
                                .change_composition(Change::Update {
                                    before: [before_left, before_right],
                                    after: [after_left, after_right],
                                })
                                .unwrap();
                        }
                        Change::Remove(Composition {
                            left_id,
                            right_id,
                        }) => {
                            let [composition, left, right]: [&mut Concept<ConceptId>; 3] = self.write_concepts(arr![ConceptId; *composition_id, *left_id, *right_id]).into();
                            composition
                                .change_composition(Change::Remove([
                                    left, right,
                                ]))
                                .unwrap();
                        }
                    }
                }
                DirectConceptDelta::Reduce {
                    change,
                    unreduced_id,
                } => {
                    debug_assert_eq!(concept_id, unreduced_id);
                    match change {
                        Change::Create(reduced_id) => {
                            let [unreduced_concept, reduced_concept]: [&mut Concept<ConceptId>; 2] = self.write_concepts(arr![ConceptId; *unreduced_id, *reduced_id]).into();
                            unreduced_concept.make_reduce_to(reduced_concept);
                        }
                        Change::Update {
                            before: before_reduced_id,
                            after: after_reduced_id,
                        } => {
                            let [unreduced_concept, before_reduced_concept, after_reduced_concept]: [&mut Concept<ConceptId>; 3] = self.write_concepts(arr![ConceptId; *unreduced_id, *before_reduced_id, *after_reduced_id]).into();
                            unreduced_concept.make_no_longer_reduce_to(
                                before_reduced_concept,
                            );
                            unreduced_concept
                                .make_reduce_to(after_reduced_concept);
                        }
                        Change::Remove(reduced_id) => {
                            let [unreduced_concept, reduced_concept]: [&mut Concept<ConceptId>; 2] = self.write_concepts(arr![ConceptId; *unreduced_id, *reduced_id]).into();
                            unreduced_concept
                                .make_no_longer_reduce_to(reduced_concept);
                        }
                    }
                }
                DirectConceptDelta::Remove(_) => todo!(),
            }
        }
    }
}
