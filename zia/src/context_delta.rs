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
    concepts::{ConcreteConceptType, LefthandOf, RighthandOf},
    context_cache::ContextCache,
    snap_shot::Reader,
};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{self, Debug, Display, Formatter},
    sync::Arc,
};

#[derive(Clone, Default)]
pub struct ContextDelta {
    string: HashMap<String, Change<usize>>,
    concepts_to_apply_in_order: Vec<(usize, Arc<DirectConceptDelta>)>,
    concept: HashMap<usize, Vec<ConceptDelta>>,
}

impl ContextDelta {
    fn update_new_concept_delta(
        &mut self,
        delta: &NewConceptDelta,
        snapshot: &impl Reader,
    ) -> usize {
        let new_concept_id = snapshot.lowest_unoccupied_concept_id(self);
        self.insert_delta_for_new_concept(new_concept_id, delta.clone());
        match delta {
            NewConceptDelta::FreeVariable | NewConceptDelta::BoundVariable => {
                // Nothing extra needed because nothing else refers to the variable concept yet
            },
            NewConceptDelta::String(s) => {
                self.string
                    .entry(s.into())
                    .and_modify(|v| match v {
                        Change::Create(_)
                        | Change::Update {
                            ..
                        } => panic!("String \"{}\" already exists", s),
                        Change::Remove(id) => {
                            *v = Change::Update {
                                before: *id,
                                after: new_concept_id,
                            }
                        },
                    })
                    .or_insert_with(|| Change::Create(new_concept_id));
            },
            NewConceptDelta::Composition(Composition {
                left_id,
                right_id,
            }) => {
                let cd = IndirectConceptDelta::LefthandOf(LefthandOf {
                    composition: new_concept_id,
                    righthand: *right_id,
                })
                .into();
                self.insert_delta_for_existing_concept(*left_id, cd);
                let cd = IndirectConceptDelta::RighthandOf(RighthandOf {
                    composition: new_concept_id,
                    lefthand: *left_id,
                })
                .into();
                self.insert_delta_for_existing_concept(*right_id, cd);
            },
            NewConceptDelta::Left {
                composition_id,
                right_id,
                ..
            } => {
                let cd = IndirectConceptDelta::RighthandOf(RighthandOf {
                    composition: *composition_id,
                    lefthand: new_concept_id,
                })
                .into();
                self.insert_delta_for_existing_concept(*right_id, cd);
                let cd = IndirectConceptDelta::ComposedOf(Composition {
                    left_id: new_concept_id,
                    right_id: *right_id,
                })
                .into();
                self.insert_delta_for_existing_concept(*composition_id, cd);
            },
            NewConceptDelta::Right {
                composition_id,
                left_id,
                ..
            } => {
                let cd = IndirectConceptDelta::LefthandOf(LefthandOf {
                    composition: *composition_id,
                    righthand: new_concept_id,
                })
                .into();
                self.insert_delta_for_existing_concept(*left_id, cd);
                let cd = IndirectConceptDelta::ComposedOf(Composition {
                    left_id: *left_id,
                    right_id: new_concept_id,
                })
                .into();
                self.insert_delta_for_existing_concept(*composition_id, cd);
            },
            NewConceptDelta::Double {
                composition_id,
                ..
            } => {
                let cd = IndirectConceptDelta::ComposedOf(Composition {
                    left_id: new_concept_id,
                    right_id: new_concept_id,
                })
                .into();
                self.insert_delta_for_existing_concept(*composition_id, cd);
            },
            NewConceptDelta::ReducesTo {
                reduction,
            } => {
                let cd =
                    IndirectConceptDelta::ReducesFrom(new_concept_id).into();
                self.insert_delta_for_existing_concept(*reduction, cd);
            },
        };
        new_concept_id
    }

    pub fn update_concept_delta(
        &mut self,
        concept_delta: &Arc<DirectConceptDelta>,
        cache_to_invalidate: &mut ContextCache,
        snapshot: &impl Reader,
    ) -> usize {
        let dcd = ConceptDelta::Direct(concept_delta.clone());
        let concept_id = match concept_delta.as_ref() {
            DirectConceptDelta::New(delta) => {
                self.update_new_concept_delta(delta, snapshot)
            },
            DirectConceptDelta::Compose {
                composition_id,
                change,
            } => {
                self.insert_delta_for_existing_concept(*composition_id, dcd);
                if let Change::Remove(before)
                | Change::Update {
                    before,
                    ..
                } = change
                {
                    let Composition {
                        left_id,
                        right_id,
                    } = before;
                    let cd = IndirectConceptDelta::NoLongerLefthandOf(
                        *composition_id,
                    )
                    .into();
                    self.insert_delta_for_existing_concept(*left_id, cd);
                    let cd = IndirectConceptDelta::NoLongerRighthandOf(
                        *composition_id,
                    )
                    .into();
                    self.insert_delta_for_existing_concept(*right_id, cd);
                }
                if let Change::Create(after)
                | Change::Update {
                    after,
                    ..
                } = change
                {
                    let Composition {
                        left_id,
                        right_id,
                    } = after;
                    let cd = IndirectConceptDelta::LefthandOf(LefthandOf {
                        composition: *composition_id,
                        righthand: *right_id,
                    })
                    .into();
                    self.insert_delta_for_existing_concept(*left_id, cd);
                    let cd = IndirectConceptDelta::RighthandOf(RighthandOf {
                        composition: *composition_id,
                        lefthand: *left_id,
                    })
                    .into();
                    self.insert_delta_for_existing_concept(*right_id, cd);
                }
                *composition_id
            },
            DirectConceptDelta::Reduce {
                unreduced_id,
                change,
            } => {
                self.insert_delta_for_existing_concept(*unreduced_id, dcd);
                if let Change::Remove(before)
                | Change::Update {
                    before,
                    ..
                } = change
                {
                    let cd = IndirectConceptDelta::NoLongerReducesFrom(
                        *unreduced_id,
                    )
                    .into();
                    self.insert_delta_for_existing_concept(*before, cd);
                }
                if let Change::Create(after)
                | Change::Update {
                    after,
                    ..
                } = change
                {
                    let cd =
                        IndirectConceptDelta::ReducesFrom(*unreduced_id).into();
                    self.insert_delta_for_existing_concept(*after, cd);
                }
                *unreduced_id
            },
            DirectConceptDelta::Remove(concept_id_to_remove) => {
                self.insert_delta_for_existing_concept(
                    *concept_id_to_remove,
                    dcd,
                );
                *concept_id_to_remove
            },
        };
        self.concepts_to_apply_in_order
            .push((concept_id, concept_delta.clone()));
        cache_to_invalidate.invalidate();
        concept_id
    }

    pub const fn concepts_to_apply_in_order(
        &self,
    ) -> &Vec<(usize, Arc<DirectConceptDelta>)> {
        &self.concepts_to_apply_in_order
    }

    fn insert_delta_for_existing_concept(
        &mut self,
        concept_id: usize,
        cd: ConceptDelta,
    ) {
        self.insert_delta_for_concept(
            concept_id,
            cd,
            |last_delta, concept_id: usize| {
                match last_delta {
                    ConceptDelta::Direct(dcd)
                        if matches!(
                            dcd.as_ref(),
                            &DirectConceptDelta::Remove(_)
                        ) =>
                    {
                        panic!("Concept {} already removed", concept_id)
                    }
                    _ => (),
                };
            },
        );
    }

    fn insert_delta_for_new_concept(
        &mut self,
        concept_id: usize,
        cd: NewConceptDelta,
    ) {
        self.insert_delta_for_concept(
            concept_id,
            ConceptDelta::Direct(Arc::new(DirectConceptDelta::New(cd))),
            |last_delta, concept_id: usize| {
                match last_delta {
                    ConceptDelta::Direct(dcd)
                        if matches!(
                            dcd.as_ref(),
                            &DirectConceptDelta::Remove(_)
                        ) => {},
                    _ => panic!("Concept {} already exists", concept_id),
                };
            },
        );
    }

    fn insert_delta_for_concept(
        &mut self,
        concept_id: usize,
        cd: ConceptDelta,
        sanity_check: impl Fn(&ConceptDelta, usize),
    ) {
        match self.concept.entry(concept_id) {
            Entry::Occupied(mut e) => {
                let last_delta = &e
                    .get()
                    .last()
                    .expect("Vector must include at least one element");
                sanity_check(last_delta, concept_id);
                e.get_mut().push(cd);
            },
            Entry::Vacant(e) => {
                e.insert(vec![cd]);
            },
        };
    }

    pub const fn string(&self) -> &HashMap<String, Change<usize>> {
        &self.string
    }

    pub const fn concept(&self) -> &HashMap<usize, Vec<ConceptDelta>> {
        &self.concept
    }
}

impl Debug for ContextDelta {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let mut string = "{\n".to_string();
        if !self.string.is_empty() {
            string += "    string: {\n";
            let mut unsorted_keys: Vec<&String> = self.string.keys().collect();
            unsorted_keys.sort();
            for key in unsorted_keys {
                let sd = self.string.get(key).unwrap();
                string += &format!("\t{}: {:#?},\n", key, sd);
            }
            string += "    },\n";
        }
        if !self.concept.is_empty() {
            string += "    concept: {\n";
            let mut unsorted_keys: Vec<&usize> = self.concept.keys().collect();
            unsorted_keys.sort();
            for key in unsorted_keys {
                for cd in self.concept.get(key).unwrap() {
                    string += &format!("\t{}: {:#?},\n", key, cd);
                }
            }
            string += "    },\n";
        }
        string += "}";
        formatter.write_str(&string)
    }
}

impl<T: Clone + Display> Debug for Change<T> {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str(&match self {
            Self::Create(n) => format!("+ {}", n),
            Self::Remove(n) => format!("- {}", n),
            Self::Update {
                before,
                after,
            } => format!("{} -> {}", before, after),
        })
    }
}

#[derive(Clone)]
pub enum ConceptDelta {
    Direct(Arc<DirectConceptDelta>),
    Indirect(IndirectConceptDelta),
}

impl ConceptDelta {
    pub const fn try_direct(&self) -> Option<&Arc<DirectConceptDelta>> {
        if let Self::Direct(dcd) = &self {
            Some(dcd)
        } else {
            None
        }
    }
}

impl From<IndirectConceptDelta> for ConceptDelta {
    fn from(icd: IndirectConceptDelta) -> Self {
        Self::Indirect(icd)
    }
}

// Only used by snapshot to calculate the Concept given the deltas
// and not to decide the mutations to apply to Concepts
#[derive(Clone, Debug)]
pub enum IndirectConceptDelta {
    LefthandOf(LefthandOf),
    RighthandOf(RighthandOf),
    NoLongerLefthandOf(usize),
    NoLongerRighthandOf(usize),
    ReducesFrom(usize),
    NoLongerReducesFrom(usize),
    ComposedOf(Composition),
}

#[derive(Clone, Debug)]
pub enum DirectConceptDelta {
    New(NewConceptDelta),
    Compose {
        composition_id: usize,
        change: Change<Composition>,
    },
    Reduce {
        unreduced_id: usize,
        change: Change<usize>,
    },
    Remove(usize),
}

#[derive(Clone, Debug)]
pub struct NewDirectConceptDelta {
    pub delta: NewConceptDelta,
    pub new_concept_id: usize,
}

#[derive(Clone, Debug)]
pub enum NewConceptDelta {
    String(String),
    Composition(Composition),
    FreeVariable,
    BoundVariable,
    Left {
        composition_id: usize,
        right_id: usize,
        concrete_type: Option<ConcreteConceptType>,
    },
    Right {
        composition_id: usize,
        left_id: usize,
        concrete_type: Option<ConcreteConceptType>,
    },
    /// Used for e.g. `let label_of label_of -> 'label_of'`
    Double {
        composition_id: usize,
        concrete_type: Option<ConcreteConceptType>,
    },
    ReducesTo {
        reduction: usize,
    },
}

#[derive(Copy)]
pub enum Change<T> {
    Create(T),
    Update {
        before: T,
        after: T,
    },
    Remove(T),
}

impl<T: Clone> Clone for Change<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Create(x) => Self::Create(x.clone()),
            Self::Update {
                before,
                after,
            } => Self::Update {
                before: before.clone(),
                after: after.clone(),
            },
            Self::Remove(x) => Self::Remove(x.clone()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Composition {
    pub left_id: usize,
    pub right_id: usize,
}

impl Display for Composition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("({}, {})", self.left_id, self.right_id))
    }
}

impl Debug for ConceptDelta {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str(&match *self {
            Self::Direct(ref c) => format!("+ {:#?}", c) + " (direct) ",
            Self::Indirect(ref c) => format!("- {:#?}", c) + " (indirect) ",
        })
    }
}
