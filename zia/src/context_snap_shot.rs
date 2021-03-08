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
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta,
    context_delta::{
        Change, Composition, ConceptDelta, ContextDelta, DirectConceptDelta,
        NewConceptDelta, NewDirectConceptDelta,
    },
    delta::Apply,
    snap_shot::Reader as SnapShotReader,
};
use bimap::BiMap;
use generic_array::{
    arr, functional::FunctionalSequence, ArrayLength, GenericArray,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    convert::TryInto,
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
    concrete_concepts: BiMap<usize, ConcreteConceptType>,
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

    // TODO document why this is a safe abstraction or refactor `self.concepts` as a slotmap
    fn write_concept_pair(
        &mut self,
        left: usize,
        right: usize,
    ) -> [&mut Concept; 2] {
        let ptr = self.concepts.as_mut_ptr();
        let (opt_l, opt_r) = unsafe {
            let l = ptr.add(left).as_mut().expect("Null pointer");
            let r = ptr.add(right).as_mut().expect("Null pointer");
            (l, r)
        };
        let (l, r) =
            opt_l.and_also_mut(opt_r).expect("some concepts are empty");
        [l, r]
    }

    // TODO document why this is a safe abstraction or refactor `self.concepts` as a slotmap
    fn write_concepts<
        'a,
        N: ArrayLength<usize> + ArrayLength<&'a mut Concept>,
    >(
        &'a mut self,
        ids: GenericArray<usize, N>,
    ) -> GenericArray<&'a mut Concept, N> {
        let ptr = self.concepts.as_mut_ptr();
        ids.map(|x| unsafe {
            ptr.add(x)
                .as_mut()
                .expect("Null pointer")
                .as_mut()
                .expect("some concepts are empty")
        })
    }

    fn concept_len(&self, delta: &ContextDelta) -> usize {
        let mut length = self.concepts.len();
        for (id, cdv) in delta.concept() {
            for dcd in cdv.iter().filter_map(|(cd, _)| {
                if let ConceptDelta::Direct(dcd) = cd {
                    Some(dcd)
                } else {
                    None
                }
            }) {
                // Is new_concept_id the same as id?
                if let DirectConceptDelta::New(NewDirectConceptDelta {
                    new_concept_id,
                    delta,
                }) = dcd.as_ref()
                {
                    if length <= *id {
                        length = *id + 1;
                    }
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
                    context_delta::Change::Update {
                        after,
                        ..
                    } => Some(after),
                    context_delta::Change::Create(concept) => Some(concept),
                    context_delta::Change::Remove(_) => None,
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
        for (id, cdv) in delta.concept() {
            for dcd in cdv.iter().filter_map(|(cd, _)| {
                if let ConceptDelta::Direct(dcd) = cd {
                    Some(dcd)
                } else {
                    None
                }
            }) {
                if let DirectConceptDelta::New(NewDirectConceptDelta {
                    new_concept_id,
                    delta,
                }) = dcd.as_ref()
                {
                    if *id >= new_concept_length {
                        new_concept_length = *id + 1
                    }
                }
            }
        }
        for (id, cdv) in delta.concept() {
            for dcd in cdv.iter().filter_map(|(cd, _)| {
                if let ConceptDelta::Direct(dcd) = cd {
                    Some(dcd)
                } else {
                    None
                }
            }) {
                match dcd.as_ref() {
                    DirectConceptDelta::New {
                        ..
                    } => {
                        removed_gaps.insert(*id);
                    },
                    DirectConceptDelta::Remove(_) => {
                        added_gaps.push(*id);
                        removed_gaps.remove(id);
                    },
                    _ => (),
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

    fn concrete_concept_id(
        &self,
        delta: &ContextDelta,
        cc: ConcreteConceptType,
    ) -> Option<usize> {
        let mut id = None;
        for (concept_id, cdv) in
            delta.concept().iter().filter(|(concept_id, _)| {
                self.concrete_concept_type(delta, **concept_id) == Some(cc)
            })
        {
            for dcd in cdv.iter().filter_map(|(cd, _)| {
                if let ConceptDelta::Direct(dcd) = cd {
                    Some(dcd)
                } else {
                    None
                }
            }) {
                match dcd.as_ref() {
                    DirectConceptDelta::New(NewDirectConceptDelta {
                        new_concept_id,
                        ..
                    }) => id = Some(Some(*concept_id)),
                    DirectConceptDelta::Remove(concept_id) => id = Some(None),
                    _ => (),
                }
            }
        }
        id.unwrap_or_else(|| self.concrete_concepts.get_by_right(&cc).cloned())
    }

    fn concrete_concept_type(
        &self,
        delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<ConcreteConceptType> {
        self.read_concept(delta, concept_id).get_concrete_concept_type()
    }

    #[cfg(test)]
    fn new_test_case(_: &[Concept], _: &HashMap<usize, &'static str>) -> Self {
        unimplemented!()
    }
}

impl Apply for ContextSnapShot {
    type Delta = ContextDelta;

    fn apply(&mut self, delta: ContextDelta) {
        delta.string().iter().for_each(|(s, sd)| match sd {
            context_delta::Change::Update {
                after,
                ..
            } => {
                self.string_map.insert(s.to_string(), *after);
            },
            context_delta::Change::Create(id) => self.add_string(*id, s),
            context_delta::Change::Remove(_) => self.remove_string(s),
        });
        let concept_len = self.concept_len(&delta);
        if concept_len > self.concepts.len() {
            self.concepts.extend(vec![None; concept_len - self.concepts.len()]);
        }
        for concept_delta in delta.concepts_to_apply_in_order() {
            match concept_delta.as_ref() {
                DirectConceptDelta::New(NewDirectConceptDelta {
                    new_concept_id,
                    delta,
                }) => match delta {
                    NewConceptDelta::String(s) => {
                        self.string_map.insert(s.into(), *new_concept_id);
                        self.concepts[*new_concept_id] = Some(
                            (SpecificPart::String(s.into()), *new_concept_id)
                                .into(),
                        );
                    },
                    NewConceptDelta::Composition(c) => {
                        let [left, right] =
                            self.write_concept_pair(c.left_id, c.right_id);
                        debug_assert!(*new_concept_id != c.left_id);
                        debug_assert!(*new_concept_id != c.right_id);
                        self.concepts[*new_concept_id] =
                            Some(Concept::composition_of(
                                *new_concept_id,
                                left,
                                right,
                            ));
                    },
                    NewConceptDelta::Left {
                        composition_id,
                        right_id,
                        concrete_type,
                        variable,
                    } => {
                        todo!();
                    },
                    NewConceptDelta::Double {
                        composition_id,
                        concrete_type,
                        variable,
                    } => {
                        let composition = self.write_concept(*composition_id);
                        debug_assert!(new_concept_id != composition_id);
                        self.concepts[*new_concept_id] = Some(Concept::double(
                            *new_concept_id,
                            composition,
                            *variable,
                            *concrete_type,
                        ));
                    },
                    NewConceptDelta::Right {
                        composition_id,
                        left_id,
                        concrete_type,
                        variable,
                    } => {
                        let [left, composition] =
                            self.write_concept_pair(*left_id, *composition_id);
                        debug_assert!(new_concept_id != left_id);
                        debug_assert!(new_concept_id != composition_id);
                        self.concepts[*new_concept_id] = Some(
                            Concept::righthand_of(
                                *new_concept_id,
                                left,
                                composition,
                                *concrete_type,
                                *variable,
                            )
                            .unwrap(),
                        );
                    },
                    NewConceptDelta::ReducesTo {
                        reduction,
                        variable,
                    } => {
                        debug_assert!(new_concept_id != reduction);
                        let reduction_concept = self.write_concept(*reduction);
                        self.concepts[*new_concept_id] =
                            Some(Concept::reduction_to(
                                *new_concept_id,
                                reduction_concept,
                                *variable,
                            ));
                    },
                },
                DirectConceptDelta::Compose {
                    change,
                    composition_id,
                } => match change {
                    Change::Create(Composition {
                        left_id,
                        right_id,
                    }) => {
                        let [composition, left, right]: [&mut Concept; 3] = self.write_concepts(arr![usize; *composition_id, *left_id, *right_id]).into();
                        composition
                            .change_composition(Change::Create([left, right]))
                            .unwrap();
                    },
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
                        let [composition, before_left, before_right, after_left, after_right]: [&mut Concept; 5] = self.write_concepts(arr![usize; *composition_id, *left_id, *right_id, *after_left_id, *after_right_id]).into();
                        composition
                            .change_composition(Change::Update {
                                before: [before_left, before_right],
                                after: [after_left, after_right],
                            })
                            .unwrap();
                    },
                    Change::Remove(Composition {
                        left_id,
                        right_id,
                    }) => {
                        let [composition, left, right]: [&mut Concept; 3] = self.write_concepts(arr![usize; *composition_id, *left_id, *right_id]).into();
                        composition
                            .change_composition(Change::Remove([left, right]))
                            .unwrap();
                    },
                },
                DirectConceptDelta::Reduce {
                    change,
                    unreduced_id,
                } => {
                    match change {
                        Change::Create(reduced_id) => {
                            let [unreduced_concept, reduced_concept]: [&mut Concept; 2] = self.write_concepts(arr![usize; *unreduced_id, *reduced_id]).into();
                            unreduced_concept.make_reduce_to(reduced_concept);
                        },
                        Change::Update {
                            before: before_reduced_id,
                            after: after_reduced_id,
                        } => {
                            let [unreduced_concept, before_reduced_concept, after_reduced_concept]: [&mut Concept; 3] = self.write_concepts(arr![usize; *unreduced_id, *before_reduced_id, *after_reduced_id]).into();
                            unreduced_concept.make_no_longer_reduce_to(
                                before_reduced_concept,
                            );
                            unreduced_concept
                                .make_reduce_to(after_reduced_concept);
                        },
                        Change::Remove(reduced_id) => {
                            let [unreduced_concept, reduced_concept]: [&mut Concept; 2] = self.write_concepts(arr![usize; *unreduced_id, *reduced_id]).into();
                            unreduced_concept
                                .make_no_longer_reduce_to(reduced_concept);
                        },
                    }
                },
                DirectConceptDelta::Remove(_) => todo!(),
            }
        }
    }

    fn diff(&self, _other: Self) -> ContextDelta {
        ContextDelta::default()
    }
}
