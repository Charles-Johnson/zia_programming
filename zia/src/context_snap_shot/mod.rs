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

mod concept;

mod concept_id;

use concept::Mixed;
use concept_id::Committed;
pub use concept_id::{ConceptId, Uncommitted};

use crate::{
    concepts::{Concept, ConceptTrait, ConcreteConceptType, SpecificPart},
    context_delta,
    context_delta::{
        Composition, ConceptDelta, DirectConceptDelta, NestedDelta,
        NewConceptDelta, NewDirectConceptDelta, SharedDelta, ValueChange,
    },
    delta::Apply,
    nester::SharedReference,
    snap_shot::Reader as SnapShotReader,
};
use bimap::BiMap;
use slotmap::SlotMap;
use std::{
    collections::{HashMap, VecDeque},
    convert::{TryFrom, TryInto},
    fmt::Debug,
    marker::PhantomData,
};

/// A container for adding, reading, writing and removing concepts of generic type `T`.
#[derive(Default, Debug, Clone)]
pub struct ContextSnapShot<SR: SharedReference> {
    /// Relates a String value to the index where the concept corresponding to the String is stored
    /// in the `concepts` field.
    string_map: HashMap<String, Committed>,
    concepts: SlotMap<Committed, Concept<Committed>>,
    concrete_concepts: BiMap<Committed, ConcreteConceptType>,
    previously_uncommitted_concepts: HashMap<Uncommitted, Committed>,
    phantom: PhantomData<SR>,
}

impl<SR: SharedReference> ContextSnapShot<SR> {
    fn commit_new_concept(
        &mut self,
        id: Uncommitted,
        f: impl FnOnce(Committed) -> Concept<Committed>,
    ) -> Committed {
        let committed_id = self.concepts.insert_with_key(f);
        self.previously_uncommitted_concepts.insert(id, committed_id);
        committed_id
    }

    fn commit_new_concept_with_dependent_concepts<const N: usize>(
        &mut self,
        id: Uncommitted,
        dependencies: [Committed; N],
        f: impl Fn([&mut Concept<Committed>; N], Committed) -> Concept<Committed>,
    ) -> Committed {
        // initialise concept as free variable to get the concept id
        let committed_id =
            self.concepts.insert_with_key(Concept::make_free_variable);
        // construct actual concept referencing dependencies
        let concept = {
            let dependencies =
                self.concepts.get_disjoint_mut(dependencies).unwrap();
            f(dependencies, committed_id)
        };
        // update the concept
        self.concepts[committed_id] = concept;
        self.previously_uncommitted_concepts.insert(id, committed_id);
        committed_id
    }

    fn convert_to_committed_concept_id(&self, id: ConceptId) -> Committed {
        match id {
            ConceptId::Committed(cci) => cci,
            ConceptId::Uncommitted(uci) => *self
                .previously_uncommitted_concepts
                .get(&uci)
                .expect("Concept ID not committed yet!"),
        }
    }

    fn convert_to_committed_concept_ids<const N: usize>(
        &self,
        ids: [ConceptId; N],
    ) -> [Committed; N] {
        ids.map(|id| self.convert_to_committed_concept_id(id))
    }

    #[allow(clippy::too_many_lines)]
    fn apply_new_concept(
        &mut self,
        ndcd: &NewDirectConceptDelta<ConceptId, Uncommitted>,
    ) {
        let NewDirectConceptDelta {
            new_concept_id,
            delta,
        } = ndcd;
        match delta {
            NewConceptDelta::FreeVariable => {
                self.commit_new_concept(
                    *new_concept_id,
                    Concept::make_free_variable,
                );
            },
            NewConceptDelta::BoundVariable => {
                self.commit_new_concept(
                    *new_concept_id,
                    Concept::make_bound_variable,
                );
            },
            NewConceptDelta::String(s) => {
                let new_concept_id = self.commit_new_concept(
                    *new_concept_id,
                    |new_concept_id| {
                        (SpecificPart::String(s.into()), new_concept_id).into()
                    },
                );
                self.string_map.insert(s.into(), new_concept_id);
            },
            NewConceptDelta::Composition(c) => {
                self.commit_new_concept_with_dependent_concepts(
                    *new_concept_id,
                    self.convert_to_committed_concept_ids([
                        c.left_id, c.right_id,
                    ]),
                    |[left, right], new_concept_id| {
                        Concept::composition_of(new_concept_id, left, right)
                    },
                );
            },
            NewConceptDelta::Left {
                composition_id,
                right_id,
                concrete_type,
            } => {
                let new_concept_id = self
                    .commit_new_concept_with_dependent_concepts(
                        *new_concept_id,
                        self.convert_to_committed_concept_ids([
                            *right_id,
                            *composition_id,
                        ]),
                        |[right, composition], new_concept_id| {
                            Concept::lefthand_of(
                                new_concept_id,
                                right,
                                composition,
                                *concrete_type,
                            )
                            .unwrap()
                        },
                    );
                if let Some(cct) = concrete_type {
                    self.concrete_concepts.insert(new_concept_id, *cct);
                }
            },
            NewConceptDelta::Double {
                composition_id,
                concrete_type,
                ..
            } => {
                let new_concept_id = self
                    .commit_new_concept_with_dependent_concepts(
                        *new_concept_id,
                        [self.convert_to_committed_concept_id(*composition_id)],
                        |[composition], new_concept_id| {
                            Concept::double(
                                new_concept_id,
                                composition,
                                *concrete_type,
                            )
                        },
                    );
                if let Some(cct) = concrete_type {
                    self.concrete_concepts.insert(new_concept_id, *cct);
                }
            },
            NewConceptDelta::Right {
                composition_id,
                left_id,
                concrete_type,
            } => {
                let new_concept_id = self
                    .commit_new_concept_with_dependent_concepts(
                        *new_concept_id,
                        self.convert_to_committed_concept_ids([
                            *left_id,
                            *composition_id,
                        ]),
                        |[left, composition], new_concept_id| {
                            Concept::righthand_of(
                                new_concept_id,
                                left,
                                composition,
                                *concrete_type,
                            )
                            .unwrap()
                        },
                    );
                if let Some(cct) = concrete_type {
                    self.concrete_concepts.insert(new_concept_id, *cct);
                }
            },
            NewConceptDelta::ReducesTo {
                reduction,
            } => {
                self.commit_new_concept_with_dependent_concepts(
                    *new_concept_id,
                    [self.convert_to_committed_concept_id(*reduction)],
                    |[reduction_concept], new_concept_id| {
                        Concept::reduction_to(new_concept_id, reduction_concept)
                    },
                );
            },
        }
    }

    fn write_concepts<const N: usize>(
        &mut self,
        ids: [ConceptId; N],
    ) -> Option<[&mut Concept<Committed>; N]> {
        self.concepts
            .get_disjoint_mut(self.convert_to_committed_concept_ids(ids))
    }

    fn add_string(&mut self, string_id: Committed, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }

    fn remove_string(&mut self, string: &str) {
        self.string_map.remove(string).expect("No string to remove!");
    }

    fn get_string_concept<
        SDCD: Clone
            + AsRef<DirectConceptDelta<ConceptId>>
            + From<DirectConceptDelta<ConceptId>>
            + Debug,
        D: SharedDelta<
            NestedDelta = NestedDelta<concept_id::ConceptId, SDCD, D, SR>,
        >,
    >(
        &self,
        delta: &NestedDelta<ConceptId, SDCD, D, SR>,
        s: &str,
    ) -> Option<ConceptId> {
        delta.get_string(s).map_or_else(
            || self.string_map.get(s).copied().map(ConceptId::Committed),
            |string_delta| match string_delta {
                context_delta::ValueChange::Update {
                    after,
                    before,
                } => {
                    debug_assert_eq!(
                        self.string_map
                            .get(s)
                            .copied()
                            .map(ConceptId::Committed),
                        Some(before)
                    );
                    Some(after)
                },
                context_delta::ValueChange::Create(concept) => Some(concept),
                context_delta::ValueChange::Remove(before) => {
                    debug_assert_eq!(
                        self.string_map
                            .get(s)
                            .copied()
                            .map(ConceptId::Committed),
                        Some(before)
                    );
                    None
                },
            },
        )
    }

    fn get_labellee<
        SDCD: Clone
            + AsRef<DirectConceptDelta<ConceptId>>
            + From<DirectConceptDelta<ConceptId>>
            + Debug,
        D: SharedDelta<NestedDelta = NestedDelta<ConceptId, SDCD, D, SR>>,
    >(
        &self,
        delta: &NestedDelta<ConceptId, SDCD, D, SR>,
        c: ConceptId,
    ) -> Option<ConceptId> {
        let concept = self.read_concept(delta, c);
        let mut candidates: VecDeque<ConceptId> =
            concept.find_what_reduces_to_it().collect();
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
                    candidate_concept.find_what_reduces_to_it();
                candidates.extend(extra_candidates);
            } else {
                return None;
            }
        }
    }
}

impl<SDCD, SR: SharedReference> SnapShotReader<SDCD, SR> for ContextSnapShot<SR>
where
    SDCD: Clone
        + AsRef<DirectConceptDelta<ConceptId>>
        + From<DirectConceptDelta<ConceptId>>
        + Debug,
{
    type CommittedConceptId = Committed;
    type ConceptId = ConceptId;
    type MixedConcept<'a>
        = Mixed<'a>
    where
        SR: 'a;

    fn concept_from_label<
        D: SharedDelta<NestedDelta = NestedDelta<Self::ConceptId, SDCD, D, SR>>,
    >(
        &self,
        delta: &NestedDelta<Self::ConceptId, SDCD, D, SR>,
        s: &str,
    ) -> Option<Self::ConceptId> {
        self.get_string_concept(delta, s)
            .and_then(|c| self.get_labellee(delta, c))
    }

    fn get_concept(&self, id: Self::ConceptId) -> Option<Mixed> {
        if let ConceptId::Committed(id) = id {
            self.concepts.get(id).map(Mixed::from)
        } else {
            None
        }
    }

    fn get_label<
        D: SharedDelta<NestedDelta = NestedDelta<Self::ConceptId, SDCD, D, SR>>,
    >(
        &self,
        delta: &NestedDelta<Self::ConceptId, SDCD, D, SR>,
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

    fn concrete_concept_id<
        D: SharedDelta<
            NestedDelta = NestedDelta<concept_id::ConceptId, SDCD, D, SR>,
        >,
    >(
        &self,
        delta: &NestedDelta<Self::ConceptId, SDCD, D, SR>,
        cc: ConcreteConceptType,
    ) -> Option<Self::ConceptId> {
        let mut id = None;
        for (concept_id, cdv) in
            delta.iter_concepts().filter(|(concept_id, _)| {
                self.concrete_concept_type(delta, **concept_id) == Some(cc)
            })
        {
            for dcd in cdv.filter_map(ConceptDelta::try_direct) {
                match dcd.as_ref() {
                    DirectConceptDelta::New(_) => id = Some(Some(*concept_id)),
                    DirectConceptDelta::Remove(concept_id) => {
                        debug_assert_eq!(Some(Some(*concept_id)), id);
                        id = Some(None);
                    },
                    _ => (),
                }
            }
        }
        id.unwrap_or_else(|| {
            self.concrete_concepts
                .get_by_right(&cc)
                .copied()
                .map(Self::ConceptId::from)
        })
    }

    fn concrete_concept_type<
        D: SharedDelta<
            NestedDelta = NestedDelta<concept_id::ConceptId, SDCD, D, SR>,
        >,
    >(
        &self,
        delta: &NestedDelta<Self::ConceptId, SDCD, D, SR>,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.read_concept(delta, concept_id).get_concrete_concept_type()
    }
}

impl<SDCD, SR: SharedReference> Apply<SDCD, SR> for ContextSnapShot<SR>
where
    SDCD: Clone
        + AsRef<DirectConceptDelta<ConceptId>>
        + From<DirectConceptDelta<ConceptId>>
        + Debug,
    Uncommitted: TryFrom<Self::ConceptId, Error = ()>,
{
    #[allow(clippy::too_many_lines)]
    fn apply<
        D: SharedDelta<
            NestedDelta = NestedDelta<concept_id::ConceptId, SDCD, D, SR>,
        >,
    >(
        &mut self,
        delta: NestedDelta<Self::ConceptId, SDCD, D, SR>,
    ) {
        debug_assert!(self.previously_uncommitted_concepts.is_empty());
        for (concept_id, concept_delta) in delta.concepts_to_apply_in_order() {
            match concept_delta.as_ref() {
                DirectConceptDelta::New(delta) => {
                    self.apply_new_concept(&NewDirectConceptDelta {
                        delta: delta.clone(),
                        new_concept_id: concept_id.try_into().unwrap(),
                    });
                },
                DirectConceptDelta::Compose {
                    change,
                    composition_id,
                } => {
                    debug_assert_eq!(&concept_id, composition_id);
                    match change {
                        ValueChange::Create(Composition {
                            left_id,
                            right_id,
                        }) => {
                            let [composition, left, right] = self
                                .write_concepts([
                                    *composition_id,
                                    *left_id,
                                    *right_id,
                                ])
                                .unwrap();
                            composition
                                .change_composition(ValueChange::Create([
                                    left, right,
                                ]))
                                .unwrap();
                        },
                        ValueChange::Update {
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
                            let [composition, before_left, before_right, after_left, after_right] =
                                self.write_concepts([
                                    *composition_id,
                                    *left_id,
                                    *right_id,
                                    *after_left_id,
                                    *after_right_id,
                                ])
                                .unwrap();
                            composition
                                .change_composition(ValueChange::Update {
                                    before: [before_left, before_right],
                                    after: [after_left, after_right],
                                })
                                .unwrap();
                        },
                        ValueChange::Remove(Composition {
                            left_id,
                            right_id,
                        }) => {
                            let [composition, left, right] = self
                                .write_concepts([
                                    *composition_id,
                                    *left_id,
                                    *right_id,
                                ])
                                .unwrap();
                            composition
                                .change_composition(ValueChange::Remove([
                                    left, right,
                                ]))
                                .unwrap();
                        },
                    }
                },
                DirectConceptDelta::Reduce {
                    change,
                    unreduced_id,
                } => {
                    debug_assert_eq!(&concept_id, unreduced_id);
                    match change {
                        ValueChange::Create(reduced_id) => {
                            let [unreduced_concept, reduced_concept] = self
                                .write_concepts([*unreduced_id, *reduced_id])
                                .unwrap();
                            unreduced_concept.make_reduce_to(reduced_concept);
                        },
                        ValueChange::Update {
                            before: before_reduced_id,
                            after: after_reduced_id,
                        } => {
                            let [unreduced_concept, before_reduced_concept, after_reduced_concept] =
                                self.write_concepts([
                                    *unreduced_id,
                                    *before_reduced_id,
                                    *after_reduced_id,
                                ])
                                .unwrap();
                            unreduced_concept.make_no_longer_reduce_to(
                                before_reduced_concept,
                            );
                            unreduced_concept
                                .make_reduce_to(after_reduced_concept);
                        },
                        ValueChange::Remove(reduced_id) => {
                            let [unreduced_concept, reduced_concept] = self
                                .write_concepts([*unreduced_id, *reduced_id])
                                .unwrap();
                            unreduced_concept
                                .make_no_longer_reduce_to(reduced_concept);
                        },
                    }
                },
                DirectConceptDelta::Remove(_) => todo!(),
            }
        }
        delta.iter_string().for_each(|(s, sd)| match sd {
            context_delta::ValueChange::Update {
                after,
                before,
            } => {
                let [after, before] =
                    self.convert_to_committed_concept_ids([after, before]);
                debug_assert_eq!(self.string_map.get(s).copied(), Some(before));
                self.string_map.insert(s.to_string(), after);
            },
            context_delta::ValueChange::Create(id) => {
                self.add_string(self.convert_to_committed_concept_id(id), s);
            },
            context_delta::ValueChange::Remove(before) => {
                debug_assert_eq!(
                    self.string_map.get(s).copied(),
                    Some(self.convert_to_committed_concept_id(before))
                );
                self.remove_string(s);
            },
        });
        self.previously_uncommitted_concepts.clear();
    }
}
