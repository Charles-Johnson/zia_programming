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
    concepts::{ConcreteConceptType, LefthandOf, RighthandOf},
    context_cache::ContextCache,
    snap_shot::Reader,
};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
};

#[derive(Clone)]
pub struct ContextDelta<ConceptId, SharedDirectConceptDelta> {
    string: HashMap<String, Change<ConceptId>>,
    concepts_to_apply_in_order: Vec<(ConceptId, SharedDirectConceptDelta)>,
    concept: HashMap<
        ConceptId,
        Vec<ConceptDelta<ConceptId, SharedDirectConceptDelta>>,
    >,
}

impl<ConceptId, SharedDirectConceptDelta> Default
    for ContextDelta<ConceptId, SharedDirectConceptDelta>
{
    fn default() -> Self {
        Self {
            string: HashMap::new(),
            concepts_to_apply_in_order: vec![],
            concept: HashMap::new(),
        }
    }
}

impl<ConceptId, SharedDirectConceptDelta>
    ContextDelta<ConceptId, SharedDirectConceptDelta>
where
    ConceptId: Clone + Copy + Display + Eq + Hash,
    SharedDirectConceptDelta: Clone
        + AsRef<DirectConceptDelta<ConceptId>>
        + From<DirectConceptDelta<ConceptId>>,
{
    pub fn update_concept_delta<Syntax, R>(
        &mut self,
        concept_delta: DirectConceptDelta<R::ConceptId>,
        cache_to_invalidate: &mut impl ContextCache<Syntax = Syntax>,
        snapshot: &R,
    ) -> ConceptId
    where
        Syntax: SyntaxTree<ConceptId = R::ConceptId>,
        R: Reader<SharedDirectConceptDelta, ConceptId = ConceptId>,
    {
        let concept_delta: SharedDirectConceptDelta = concept_delta.into();
        let dcd = ConceptDelta::Direct(concept_delta.clone());
        let concept_id = match concept_delta.as_ref() {
            DirectConceptDelta::New(delta) => {
                self.update_new_concept_delta::<Syntax, R>(delta, snapshot)
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
        self.concepts_to_apply_in_order.push((concept_id, concept_delta));
        cache_to_invalidate.invalidate();
        concept_id
    }

    pub fn concepts_to_apply_in_order(
        &self,
    ) -> &Vec<(ConceptId, SharedDirectConceptDelta)> {
        &self.concepts_to_apply_in_order
    }

    pub fn string(&self) -> &HashMap<String, Change<ConceptId>> {
        &self.string
    }

    pub fn concept(
        &self,
    ) -> &HashMap<
        ConceptId,
        Vec<ConceptDelta<ConceptId, SharedDirectConceptDelta>>,
    > {
        &self.concept
    }

    fn update_new_concept_delta<Syntax, R>(
        &mut self,
        delta: &NewConceptDelta<ConceptId>,
        snapshot: &R,
    ) -> ConceptId
    where
        Syntax: SyntaxTree<ConceptId = R::ConceptId>,
        R: Reader<SharedDirectConceptDelta, ConceptId = ConceptId>,
    {
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

    fn insert_delta_for_existing_concept(
        &mut self,
        concept_id: ConceptId,
        cd: ConceptDelta<ConceptId, SharedDirectConceptDelta>,
    ) {
        self.insert_delta_for_concept(
            concept_id,
            cd,
            |last_delta, concept_id: ConceptId| {
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
        concept_id: ConceptId,
        cd: NewConceptDelta<ConceptId>,
    ) {
        self.insert_delta_for_concept(
            concept_id,
            ConceptDelta::Direct(DirectConceptDelta::New(cd).into()),
            |last_delta, concept_id: ConceptId| {
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
        concept_id: ConceptId,
        cd: ConceptDelta<ConceptId, SharedDirectConceptDelta>,
        sanity_check: impl Fn(
            &ConceptDelta<ConceptId, SharedDirectConceptDelta>,
            ConceptId,
        ),
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
}

impl<ConceptId, SharedDirectConceptDelta> Debug
    for ContextDelta<ConceptId, SharedDirectConceptDelta>
where
    ConceptId: Clone + Debug + Display + Eq + Hash,
    SharedDirectConceptDelta: Debug,
{
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
            for key in self.concept.keys() {
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

#[derive(Clone)]
pub enum ConceptDelta<Id, SharedDirectConceptDelta> {
    Direct(SharedDirectConceptDelta),
    Indirect(IndirectConceptDelta<Id>),
}

impl<Id: Clone + Display, SharedDirectConceptDelta>
    ConceptDelta<Id, SharedDirectConceptDelta>
{
    pub fn try_direct(&self) -> Option<&SharedDirectConceptDelta> {
        if let Self::Direct(dcd) = &self {
            Some(dcd)
        } else {
            None
        }
    }
}

impl<Id: Clone + Display, SharedDirectConceptDelta>
    From<IndirectConceptDelta<Id>>
    for ConceptDelta<Id, SharedDirectConceptDelta>
{
    fn from(icd: IndirectConceptDelta<Id>) -> Self {
        Self::Indirect(icd)
    }
}

impl<Id, SharedDirectConceptDelta> Debug
    for ConceptDelta<Id, SharedDirectConceptDelta>
where
    SharedDirectConceptDelta: Debug,
    Id: Debug,
{
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

// Only used by snapshot to calculate the Concept given the deltas
// and not to decide the mutations to apply to Concepts
#[derive(Clone, Debug)]
pub enum IndirectConceptDelta<Id> {
    LefthandOf(LefthandOf<Id>),
    RighthandOf(RighthandOf<Id>),
    NoLongerLefthandOf(Id),
    NoLongerRighthandOf(Id),
    ReducesFrom(Id),
    NoLongerReducesFrom(Id),
    ComposedOf(Composition<Id>),
}

#[derive(Clone, Debug)]
pub enum DirectConceptDelta<Id: Clone + Display> {
    New(NewConceptDelta<Id>),
    Compose {
        composition_id: Id,
        change: Change<Composition<Id>>,
    },
    Reduce {
        unreduced_id: Id,
        change: Change<Id>,
    },
    Remove(Id),
}

#[derive(Clone, Debug)]
pub struct NewDirectConceptDelta<Id> {
    pub delta: NewConceptDelta<Id>,
    pub new_concept_id: Id,
}

#[derive(Clone, Debug)]
pub enum NewConceptDelta<Id> {
    String(String),
    Composition(Composition<Id>),
    FreeVariable,
    BoundVariable,
    Left {
        composition_id: Id,
        right_id: Id,
        concrete_type: Option<ConcreteConceptType>,
    },
    Right {
        composition_id: Id,
        left_id: Id,
        concrete_type: Option<ConcreteConceptType>,
    },
    /// Used for e.g. `let label_of label_of -> 'label_of'`
    Double {
        composition_id: Id,
        concrete_type: Option<ConcreteConceptType>,
    },
    ReducesTo {
        reduction: Id,
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
pub struct Composition<Id> {
    pub left_id: Id,
    pub right_id: Id,
}

impl<Id: Display> Display for Composition<Id> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("({}, {})", self.left_id, self.right_id))
    }
}
