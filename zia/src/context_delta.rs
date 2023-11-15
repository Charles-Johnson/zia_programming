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
    mixed_concept::MixedConcept,
    reduction_reason::{ReductionReason, Syntax},
};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{self, Debug, Display, Formatter},
    hash::Hash, slice::Iter,
};

#[derive(Clone)]
pub struct ContextDelta<ConceptId, SharedDirectConceptDelta> {
    string: HashMap<String, ValueChange<ConceptId>>,
    number_of_uncommitted_concepts: usize,
    concepts_to_apply_in_order: Vec<(ConceptId, SharedDirectConceptDelta)>,
    concept: HashMap<
        ConceptId,
        Vec<ConceptDelta<ConceptId, SharedDirectConceptDelta>>,
    >,
}

pub trait SharedDelta: Clone + Default + Debug {
    type NestedDelta;
    fn get_mut(&mut self) -> Option<&mut Self::NestedDelta>;
}

impl<ConceptId, SharedDirectConceptDelta> Default
    for ContextDelta<ConceptId, SharedDirectConceptDelta>
{
    fn default() -> Self {
        Self {
            string: HashMap::new(),
            concepts_to_apply_in_order: vec![],
            concept: HashMap::new(),
            number_of_uncommitted_concepts: 0,
        }
    }
}

impl<ConceptId, SharedDirectConceptDelta>
    ContextDelta<ConceptId, SharedDirectConceptDelta>
where
    ConceptId: MixedConcept,
    SharedDirectConceptDelta: Clone
        + AsRef<DirectConceptDelta<ConceptId>>
        + From<DirectConceptDelta<ConceptId>>,
{
    pub fn update_concept_delta<C>(
        &mut self,
        concept_delta: DirectConceptDelta<ConceptId>,
        cache_to_invalidate: &mut C,
    ) -> ConceptId
    where
        C: ContextCache,
        <C::RR as ReductionReason>::Syntax: SyntaxTree<ConceptId = ConceptId>,
    {
        let concept_delta: SharedDirectConceptDelta = concept_delta.into();
        let dcd = ConceptDelta::Direct(concept_delta.clone());
        let concept_id = match concept_delta.as_ref() {
            DirectConceptDelta::New(delta) => {
                self.update_new_concept_delta::<Syntax<C>>(delta)
            },
            DirectConceptDelta::Compose {
                composition_id,
                change,
            } => {
                self.insert_delta_for_existing_concept(*composition_id, dcd);
                if let ValueChange::Remove(before)
                | ValueChange::Update {
                    before,
                    ..
                } = change
                {
                    let Composition {
                        left_id,
                        right_id,
                    } = before;
                    let cd =
                        IndirectConceptDelta::NoLongerLefthandOf(*right_id)
                            .into();
                    self.insert_delta_for_existing_concept(*left_id, cd);
                    let cd =
                        IndirectConceptDelta::NoLongerRighthandOf(*left_id)
                            .into();
                    self.insert_delta_for_existing_concept(*right_id, cd);
                }
                if let ValueChange::Create(after)
                | ValueChange::Update {
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
                if let ValueChange::Remove(before)
                | ValueChange::Update {
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
                if let ValueChange::Create(after)
                | ValueChange::Update {
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

    pub const fn concepts_to_apply_in_order(
        &self,
    ) -> &Vec<(ConceptId, SharedDirectConceptDelta)> {
        &self.concepts_to_apply_in_order
    }

    pub fn get_string(&self, key: &str) -> Option<&ValueChange<ConceptId>> {
        self.string.get(key)
    }

    pub fn get_concept(&self, key: &ConceptId) -> Option<impl Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>> {
        self.concept.get(key).map(|v| v.iter())
    }

    pub fn iter_concepts(&self) -> impl Iterator<Item=(&ConceptId, Iter<ConceptDelta<ConceptId, SharedDirectConceptDelta>>)> {
        self.concept.iter().map(|(c, v)| (c, v.iter())) 
    }

    fn update_new_concept_delta<Syntax>(
        &mut self,
        delta: &NewConceptDelta<ConceptId>,
    ) -> ConceptId
    where
        Syntax: SyntaxTree<ConceptId = ConceptId>,
    {
        let new_concept_id = self.insert_delta_for_new_concept(delta.clone());
        match delta {
            NewConceptDelta::FreeVariable | NewConceptDelta::BoundVariable => {
                // Nothing extra needed because nothing else refers to the variable concept yet
            },
            NewConceptDelta::String(s) => {
                self.string
                    .entry(s.into())
                    .and_modify(|v| match v {
                        ValueChange::Create(_)
                        | ValueChange::Update {
                            ..
                        } => panic!("String \"{}\" already exists", s),
                        ValueChange::Remove(id) => {
                            *v = ValueChange::Update {
                                before: *id,
                                after: new_concept_id,
                            }
                        },
                    })
                    .or_insert_with(|| ValueChange::Create(new_concept_id));
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
                        panic!("Concept {} already removed", concept_id);
                    },
                    _ => (),
                };
            },
        );
    }

    pub fn insert_delta_for_new_concept(
        &mut self,
        cd: NewConceptDelta<ConceptId>,
    ) -> ConceptId {
        // TODO: Explicitly convert to uncommitted concept
        let concept_id: ConceptId = ConceptId::uncommitted(self.number_of_uncommitted_concepts);
        self.number_of_uncommitted_concepts += 1;
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
        concept_id
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

#[derive(Debug)]
pub struct NestedContextDelta<ConceptId, SharedDirectConceptDelta, D>
where
    ConceptId: MixedConcept,
    SharedDirectConceptDelta: Debug,
    D: Debug + AsRef<NestedContextDelta<ConceptId, SharedDirectConceptDelta, D>>,
{
    inner_delta: Option<D>,
    overlay_delta: ContextDelta<ConceptId, SharedDirectConceptDelta>,
}

impl<ConceptId: Clone, SharedDirectConceptDelta, D: Debug + AsRef<NestedContextDelta<ConceptId, SharedDirectConceptDelta, D>>> Default
    for NestedContextDelta<ConceptId, SharedDirectConceptDelta, D>
where
    ConceptId: MixedConcept,
    SharedDirectConceptDelta: Debug,
{
    fn default() -> Self {
        Self {
            inner_delta: None,
            overlay_delta: ContextDelta::default(),
        }
    }
}

impl<ConceptId, SharedDirectConceptDelta, D>
    NestedContextDelta<ConceptId, SharedDirectConceptDelta, D>
where
    ConceptId: MixedConcept,
    SharedDirectConceptDelta: Clone
        + AsRef<DirectConceptDelta<ConceptId>>
        + From<DirectConceptDelta<ConceptId>>
        + Debug,
    D: AsRef<NestedContextDelta<ConceptId, SharedDirectConceptDelta, D>> + Debug,
{
    pub fn spawn(inner_delta: D) -> Self {
        Self {
            inner_delta: Some(inner_delta),
            overlay_delta: ContextDelta::default(),
        }
    }
    pub fn get_string(&self, key: &str) -> Option<ValueChange<ConceptId>> {
        match (self.inner_delta.as_ref().and_then(|d| d.as_ref().get_string(key)), self.overlay_delta.get_string(key)) {
            (None, None) => None,
            (x @ Some(_), None) => x,
            (None, Some(x))
                | (Some(ValueChange::Remove(_)), Some(x @ ValueChange::Create(_)))
                => Some(*x),
            (Some(ValueChange::Create(before_remove)|ValueChange::Update { after:before_remove, .. }), Some(x @ ValueChange::Remove(removed)))
                => {
                    assert_eq!(&before_remove, removed);
                    Some(*x)
                },
            (Some(ValueChange::Create(created)), Some(ValueChange::Update{after, before}))
                => {
                    debug_assert_eq!(&created, before);
                    Some(ValueChange::Create(*after))
                },
            (Some(ValueChange::Update { before, after: between1 }), Some(ValueChange::Update { before: between2, after })) => {
                assert_eq!(&between1, between2);
                Some(ValueChange::Update { before, after: *after })
            }
            (x @ Some(ValueChange::Remove(_)), y @ Some(ValueChange::Remove(_)))
            | (x @ Some(ValueChange::Remove(_)), y @ Some(ValueChange::Update{..}))
            | (x @ Some(ValueChange::Create(_)), y @ Some(ValueChange::Create(_)))
            | (x @ Some(ValueChange::Update{..}), y @ Some(ValueChange::Create(_))) => {
                panic!("Unexpected string delta combination - before: {:?}, after: {:?}", x, y)
            } 
        }
    }

    pub fn iter_string(&self) -> impl Iterator<Item=(&String, ValueChange<ConceptId>)> {
        // need to combine values for same key
        let maybe_inner_strings = self.inner_delta.as_ref().map(|d| d.as_ref().iter_string());
        let overlay_strings = self.overlay_delta.string.keys();
        match maybe_inner_strings {
            None => {
                let new_box: Box<dyn Iterator<Item=(&String, ValueChange<ConceptId>)>> = Box::new(std::iter::empty());
                new_box
            },
            Some(inner_strings) => {
                let new_box: Box<dyn Iterator<Item=(&String, ValueChange<ConceptId>)>> = Box::new(inner_strings.filter(move |(s, _)| !self.overlay_delta.string.contains_key(*s)));
                new_box
            }
        }.chain(overlay_strings.flat_map(move |s| {
            self.get_string(s).map(|vc| (s, vc))
        }))
    }

    pub fn get_concept<'a>(&'a self, key: &'a ConceptId) -> Option<impl Iterator<Item=&'a ConceptDelta<ConceptId, SharedDirectConceptDelta>>> {
        let maybe_inner_concept_deltas = self.inner_delta.as_ref().and_then(|d| d.as_ref().get_concept(key));
        let maybe_over_concept_deltas = self.overlay_delta.get_concept(key);
        match (maybe_inner_concept_deltas, maybe_over_concept_deltas) {
            (None, None) => None,
            (Some(cd), None) => {
                let new_box: Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>> = Box::new(cd);
                Some(new_box)
            },
            (None, Some(cd)) => {
                let new_box: Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>> = Box::new(cd);
                Some(new_box)
            },
            (Some(icd), Some(ocd)) => {
                let new_box: Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>> = Box::new(icd.chain(ocd));
                Some(new_box)
            }
        }
    }

    pub fn iter_concepts<'a>(&'a self) -> impl Iterator<Item=(&'a ConceptId, Box<dyn Iterator<Item=&'a ConceptDelta<ConceptId, SharedDirectConceptDelta>> + 'a>)> {
        let inner_delta = self.inner_delta.as_ref();
        let new_box: Box<dyn Iterator<Item=(&ConceptId, Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>>)>> = Box::new(self.overlay_delta.iter_concepts().map(move |(k, v)| {
            (k, {
                let inner_concept_iter = inner_delta.and_then(|d| d.as_ref().get_concept(k));
                match inner_concept_iter {
                    None => {
                        let new_box: Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>> = Box::new(
                            v
                        );
                        new_box
                    },
                    Some(inner_concept_iter) => {
                        let new_box: Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>> = Box::new(
                            inner_concept_iter.chain(v)
                        );
                        new_box
                    }
                }
            })
        }));
        let concepts = &self.overlay_delta.concept;
        match &self.inner_delta {
            Some(d) => {
                let new_box: Box<dyn Iterator<Item=(&ConceptId, Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>>)>> = Box::new(d.as_ref().iter_concepts().filter(move |(k, _)| !concepts.contains_key(*k)));
                new_box
            }
            None => {
                let new_box: Box<dyn Iterator<Item=(&ConceptId, Box<dyn Iterator<Item=&ConceptDelta<ConceptId, SharedDirectConceptDelta>>>)>> = Box::new(std::iter::empty());
                new_box
            }
        }.chain(new_box)
    }
    pub fn update_concept_delta<C>(
        &mut self,
        concept_delta: DirectConceptDelta<ConceptId>,
        cache_to_invalidate: &mut C,
    ) -> ConceptId
    where
        C: ContextCache,
        <C::RR as ReductionReason>::Syntax: SyntaxTree<ConceptId = ConceptId>,
    {
        self.overlay_delta
            .update_concept_delta(concept_delta, cache_to_invalidate)
    }

    pub fn concepts_to_apply_in_order(
        &self,
    ) -> Vec<(ConceptId, SharedDirectConceptDelta)> {
        let mut concepts = self.inner_delta.as_ref().map_or_else(Vec::new, |d| {
            d.as_ref().concepts_to_apply_in_order().to_vec()
        });
        concepts
            .extend(self.overlay_delta.concepts_to_apply_in_order().to_vec());
        concepts
    }

    pub fn insert_delta_for_new_concept(
        &mut self,
        cd: NewConceptDelta<ConceptId>,
    ) -> ConceptId {
        self.overlay_delta.insert_delta_for_new_concept(cd)
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
    pub const fn try_direct(&self) -> Option<&SharedDirectConceptDelta> {
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

impl<T: Clone + Display, U: Clone + Display> Debug
    for ValueAndTypeChange<T, U>
{
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
        change: ValueChange<Composition<Id>>,
    },
    Reduce {
        unreduced_id: Id,
        change: ValueChange<Id>,
    },
    Remove(Id),
}

#[derive(Clone, Debug)]
pub struct NewDirectConceptDelta<Id, UncommittedId> {
    pub delta: NewConceptDelta<Id>,
    pub new_concept_id: UncommittedId,
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

pub type ValueChange<T> = ValueAndTypeChange<T, T>;

#[derive(Copy, Clone, PartialEq)]
pub enum ValueAndTypeChange<T, U> {
    Create(U),
    Update {
        before: T,
        after: U,
    },
    Remove(T),
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
