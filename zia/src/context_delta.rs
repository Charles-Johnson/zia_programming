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
    concepts::{
        Concept, ConcreteConceptType, LefthandOf, RighthandOf, SpecificPart,
    },
    context_cache::GenericCache,
    context_snap_shot::{
        ConceptId as ContextConceptId, ContextSnapShot, Uncommitted,
    },
    mixed_concept::{self, ConceptId, MixedConcept},
    nester::SharedReference,
};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{self, Debug, Display, Formatter},
    marker::PhantomData,
    slice::Iter,
};

#[derive(Clone)]
pub struct ContextDelta<
    ConceptId: mixed_concept::ConceptId,
    SR: SharedReference,
> {
    string: HashMap<String, ValueChange<ConceptId>>,
    number_of_uncommitted_concepts: usize,
    concepts_to_apply_in_order:
        Vec<(ConceptId, SR::Share<DirectContextConceptDelta<ConceptId, SR>>)>,
    concept: HashMap<ConceptId, Vec<ContextConceptDelta<ConceptId, SR>>>,
    phantom: PhantomData<SR>,
}

impl<ConceptId: mixed_concept::ConceptId, SR: SharedReference>
    ContextDelta<ConceptId, SR>
{
    fn new(number_of_uncommitted_concepts: usize) -> Self {
        Self {
            string: HashMap::new(),
            concepts_to_apply_in_order: vec![],
            concept: HashMap::new(),
            number_of_uncommitted_concepts,
            phantom: PhantomData,
        }
    }
}

impl<CI, SR: SharedReference> ContextDelta<CI, SR>
where
    CI: MixedConcept,
{
    pub fn update_concept_delta(
        &mut self,
        concept_delta: DirectConceptDelta<CI, ND>,
        cache_to_invalidate: &mut GenericCache<CI, SR>,
    ) -> CI {
        let concept_delta = SR::share(concept_delta);
        let dcd = ConceptDelta::Direct(concept_delta.clone());
        let concept_id = match concept_delta.as_ref() {
            DirectConceptDelta::New(delta) => {
                self.update_new_concept_delta(delta.clone())
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
    ) -> &Vec<(CI, SR::Share<DirectConceptDelta<CI, SR, ND>>)> {
        &self.concepts_to_apply_in_order
    }

    pub fn get_string(&self, key: &str) -> Option<&ValueChange<CI>> {
        self.string.get(key)
    }

    pub fn get_concept(
        &self,
        key: &CI,
    ) -> Option<impl Iterator<Item = &ConceptDelta<CI, SR>>> {
        self.concept.get(key).map(|v| v.iter())
    }

    pub fn iter_concepts(
        &self,
    ) -> impl Iterator<Item = (&CI, Iter<'_, ConceptDelta<CI, SR>>)> {
        self.concept.iter().map(|(c, v)| (c, v.iter()))
    }

    fn update_new_concept_delta(
        &mut self,
        delta: SR::Share<dyn NewConceptDelta<Id = CI, SR = SR>>,
    ) -> CI {
        let new_concept_id = self.insert_delta_for_new_concept(delta.clone());
        delta.update_context_delta(self, new_concept_id);
        new_concept_id
    }

    fn insert_delta_for_existing_concept(
        &mut self,
        concept_id: CI,
        cd: ConceptDelta<CI, SR>,
    ) {
        self.insert_delta_for_concept(
            concept_id,
            cd,
            |last_delta, concept_id: CI| match last_delta {
                ConceptDelta::Direct(dcd)
                    if matches!(
                        dcd.as_ref(),
                        &DirectConceptDelta::Remove(_)
                    ) =>
                {
                    panic!("Concept {concept_id} already removed");
                },
                _ => (),
            },
        );
    }

    pub fn insert_delta_for_new_concept(
        &mut self,
        cd: SR::Share<dyn NewConceptDelta<Id = CI, SR = SR>>,
    ) -> CI {
        let concept_id: CI =
            CI::uncommitted(self.number_of_uncommitted_concepts);
        self.number_of_uncommitted_concepts += 1;
        self.insert_delta_for_concept(
            concept_id,
            ConceptDelta::Direct(SR::share(DirectConceptDelta::New(cd))),
            |last_delta, concept_id: CI| match last_delta {
                ConceptDelta::Direct(dcd)
                    if matches!(
                        dcd.as_ref(),
                        &DirectConceptDelta::Remove(_)
                    ) => {},
                _ => panic!("Concept {concept_id} already exists"),
            },
        );
        concept_id
    }

    fn insert_delta_for_concept(
        &mut self,
        concept_id: CI,
        cd: ConceptDelta<CI, SR>,
        sanity_check: impl Fn(&ConceptDelta<CI, SR>, CI),
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
        }
    }
}

impl<CI, SR: SharedReference> Debug for ContextDelta<CI, SR>
where
    CI: ConceptId,
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
                string += &format!("\t{key}: {sd:#?},\n");
            }
            string += "    },\n";
        }
        if !self.concept.is_empty() {
            string += "    concept: {\n";
            for key in self.concept.keys() {
                for cd in self.concept.get(key).unwrap() {
                    string += &format!("\t{key}: {cd:#?},\n");
                }
            }
            string += "    },\n";
        }
        string += "}";
        formatter.write_str(&string)
    }
}

#[derive(Clone)]
pub struct NestedDelta<CI, SR: SharedReference>
where
    CI: ConceptId,
{
    inner_delta: Option<SR::Share<NestedDelta<CI, SR>>>,
    overlay_delta: ContextDelta<CI, SR>,
}

impl<CI: ConceptId, SR: SharedReference> Debug for NestedDelta<CI, SR> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("NestedDelta")
            .field(
                "inner_delta",
                &self.inner_delta.as_ref().map(std::convert::AsRef::as_ref),
            )
            .field("overlay_delta", &self.overlay_delta)
            .finish()
    }
}
impl<CI, SR: SharedReference> Default for NestedDelta<CI, SR>
where
    CI: ConceptId + Clone,
{
    fn default() -> Self {
        Self {
            inner_delta: None,
            overlay_delta: ContextDelta::new(0),
        }
    }
}
type BoxedTupleIterator<'a, ConceptId, SR> = Box<
    dyn Iterator<
            Item = (
                &'a ConceptId,
                Box<dyn Iterator<Item = &'a ConceptDelta<ConceptId, SR>> + 'a>,
            ),
        > + 'a,
>;
impl<CI, SR: SharedReference> NestedDelta<CI, SR>
where
    CI: MixedConcept,
{
    pub fn spawn(inner_delta: SR::Share<Self>) -> Self {
        Self {
            overlay_delta: ContextDelta::new(
                inner_delta
                    .as_ref()
                    .overlay_delta
                    .number_of_uncommitted_concepts,
            ),
            inner_delta: Some(inner_delta),
        }
    }

    pub fn get_string(&self, key: &str) -> Option<ValueChange<CI>> {
        match (
            self.inner_delta.as_ref().and_then(|d| d.as_ref().get_string(key)),
            self.overlay_delta.get_string(key),
        ) {
            (None, None) => None,
            (x @ Some(_), None) => x,
            (None, Some(x))
            | (
                Some(ValueChange::Remove(_)),
                Some(x @ ValueChange::Create(_)),
            ) => Some(*x),
            (
                Some(
                    ValueChange::Create(before_remove)
                    | ValueChange::Update {
                        after: before_remove,
                        ..
                    },
                ),
                Some(x @ ValueChange::Remove(removed)),
            ) => {
                assert_eq!(&before_remove, removed);
                Some(*x)
            },
            (
                Some(ValueChange::Create(created)),
                Some(ValueChange::Update {
                    after,
                    before,
                }),
            ) => {
                debug_assert_eq!(&created, before);
                Some(ValueChange::Create(*after))
            },
            (
                Some(ValueChange::Update {
                    before,
                    after: between1,
                }),
                Some(ValueChange::Update {
                    before: between2,
                    after,
                }),
            ) => {
                assert_eq!(&between1, between2);
                Some(ValueChange::Update {
                    before,
                    after: *after,
                })
            },
            (
                x @ Some(ValueChange::Remove(_)),
                y @ Some(
                    ValueChange::Remove(_)
                    | ValueChange::Update {
                        ..
                    },
                ),
            )
            | (
                x @ Some(
                    ValueChange::Create(_)
                    | ValueChange::Update {
                        ..
                    },
                ),
                y @ Some(ValueChange::Create(_)),
            ) => {
                panic!("Unexpected string delta combination - before: {x:?}, after: {y:?}")
            },
        }
    }

    pub fn iter_string(
        &self,
    ) -> impl Iterator<Item = (&String, ValueChange<CI>)> {
        // need to combine values for same key
        let maybe_inner_strings =
            self.inner_delta.as_ref().map(|d| d.as_ref().iter_string());
        let overlay_strings = self.overlay_delta.string.keys();
        maybe_inner_strings
            .map_or_else(
                || {
                    let new_box: Box<
                        dyn Iterator<Item = (&String, ValueChange<CI>)>,
                    > = Box::new(std::iter::empty());
                    new_box
                },
                |inner_strings| {
                    let new_box: Box<
                        dyn Iterator<Item = (&String, ValueChange<CI>)>,
                    > = Box::new(inner_strings.filter(move |(s, _)| {
                        !self.overlay_delta.string.contains_key(*s)
                    }));
                    new_box
                },
            )
            .chain(
                overlay_strings
                    .filter_map(move |s| self.get_string(s).map(|vc| (s, vc))),
            )
    }

    pub fn get_concept<'a>(
        &'a self,
        key: &'a CI,
    ) -> Option<impl Iterator<Item = &'a ConceptDelta<CI, SR>>> {
        let maybe_inner_concept_deltas =
            self.inner_delta.as_ref().and_then(|d| d.as_ref().get_concept(key));
        let maybe_over_concept_deltas = self.overlay_delta.get_concept(key);
        match (maybe_inner_concept_deltas, maybe_over_concept_deltas) {
            (None, None) => None,
            (Some(cd), None) => {
                let new_box: Box<dyn Iterator<Item = &ConceptDelta<CI, SR>>> =
                    Box::new(cd);
                Some(new_box)
            },
            (None, Some(cd)) => {
                let new_box: Box<dyn Iterator<Item = &ConceptDelta<CI, SR>>> =
                    Box::new(cd);
                Some(new_box)
            },
            (Some(inner_cd), Some(outer_cd)) => {
                let new_box: Box<
                    dyn Iterator<
                        Item = &ConceptDelta<
                            CI,
                            SR,
                            SR::Share<dyn NewConceptDelta<Id = CI, SR = SR>>,
                        >,
                    >,
                > = Box::new(inner_cd.chain(outer_cd));
                Some(new_box)
            },
        }
    }

    pub fn iter_concepts<'a>(
        &'a self,
    ) -> impl Iterator<
        Item = (
            &'a CI,
            Box<
                dyn Iterator<
                        Item = &'a ConceptDelta<
                            CI,
                            SR,
                            SR::Share<dyn NewConceptDelta<Id = CI, SR = SR>>,
                        >,
                    > + 'a,
            >,
        ),
    > + 'a {
        let inner_delta = self.inner_delta.as_ref();
        let new_box: BoxedTupleIterator<'a, CI, SR> =
            Box::new(self.overlay_delta.iter_concepts().map(move |(k, v)| {
                (k, {
                    let inner_concept_iter =
                        inner_delta.and_then(|d| d.as_ref().get_concept(k));
                    match inner_concept_iter {
                        None => {
                            let new_box: Box<
                                dyn Iterator<Item = &ConceptDelta<CI, SR>>,
                            > = Box::new(v);
                            new_box
                        },
                        Some(inner_concept_iter) => {
                            let new_box: Box<
                                dyn Iterator<Item = &ConceptDelta<CI, SR>>,
                            > = Box::new(inner_concept_iter.chain(v));
                            new_box
                        },
                    }
                })
            }));
        let concepts = &self.overlay_delta.concept;
        self.inner_delta
            .as_ref()
            .map_or_else(
                || {
                    let new_box: BoxedTupleIterator<'a, CI, SR> =
                        Box::new(std::iter::empty());
                    new_box
                },
                |d| {
                    let new_box: BoxedTupleIterator<'a, CI, SR> = Box::new(
                        d.as_ref()
                            .iter_concepts()
                            .filter(move |(k, _)| !concepts.contains_key(*k)),
                    );
                    new_box
                },
            )
            .chain(new_box)
    }

    pub fn update_concept_delta(
        &mut self,
        concept_delta: DirectConceptDelta<CI, ND>,
        cache_to_invalidate: &mut GenericCache<CI, SR>,
    ) -> CI {
        self.overlay_delta
            .update_concept_delta(concept_delta, cache_to_invalidate)
    }

    pub fn concepts_to_apply_in_order(
        &self,
    ) -> Vec<(CI, SR::Share<DirectContextConceptDelta<CI, SR>>)> {
        let mut concepts = self
            .inner_delta
            .as_ref()
            .map_or_else(Vec::new, |d| d.as_ref().concepts_to_apply_in_order());
        concepts
            .extend(self.overlay_delta.concepts_to_apply_in_order().clone());
        concepts
    }

    pub fn insert_delta_for_new_concept(
        &mut self,
        cd: SR::Share<dyn NewConceptDelta<Id = CI, SR = SR>>,
    ) -> CI {
        self.overlay_delta.insert_delta_for_new_concept(cd)
    }
}

#[derive(Clone)]
pub enum ConceptDelta<Id: ConceptId, SR: SharedReference, ND> {
    Direct(SR::Share<DirectConceptDelta<Id, ND>>),
    Indirect(IndirectConceptDelta<Id>),
}

impl<Id: ConceptId, SR: SharedReference, ND> ConceptDelta<Id, SR, ND> {
    pub const fn try_direct(
        &self,
    ) -> Option<&SR::Share<DirectConceptDelta<Id, ND>>> {
        if let Self::Direct(dcd) = &self {
            Some(dcd)
        } else {
            None
        }
    }
}

impl<Id: ConceptId, SR: SharedReference, ND> From<IndirectConceptDelta<Id>>
    for ConceptDelta<Id, SR, ND>
{
    fn from(icd: IndirectConceptDelta<Id>) -> Self {
        Self::Indirect(icd)
    }
}

impl<Id, SR, ND> Debug for ConceptDelta<Id, SR, ND>
where
    Id: ConceptId,
    SR: SharedReference,
{
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str(&match *self {
            Self::Direct(ref c) => {
                format!("+ {:#?}", c.as_ref()) + " (direct) "
            },
            Self::Indirect(ref c) => format!("- {c:#?}") + " (indirect) ",
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
            Self::Create(n) => format!("+ {n}"),
            Self::Remove(n) => format!("- {n}"),
            Self::Update {
                before,
                after,
            } => format!("{before} -> {after}"),
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

#[derive(Clone)]
pub enum DirectConceptDelta<Id: Clone + Display, ND> {
    New(ND),
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

impl<Id: Clone + Debug + Display, ND: AsRef<dyn Debug>> Debug
    for DirectConceptDelta<Id, ND>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::New(arg0) => {
                let arg0: Box<dyn Debug> = Box::new(arg0.as_ref());
                f.debug_tuple("New").field(&arg0).finish()
            },
            Self::Compose {
                composition_id,
                change,
            } => f
                .debug_struct("Compose")
                .field("composition_id", composition_id)
                .field("change", change)
                .finish(),
            Self::Reduce {
                unreduced_id,
                change,
            } => f
                .debug_struct("Reduce")
                .field("unreduced_id", unreduced_id)
                .field("change", change)
                .finish(),
            Self::Remove(arg0) => f.debug_tuple("Remove").field(arg0).finish(),
        }
    }
}

#[derive(Debug)]
pub struct NewDirectConceptDelta<D, UncommittedId> {
    pub delta: D,
    pub new_concept_id: UncommittedId,
}

pub trait NewConceptDelta: Debug {
    type Id: ConceptId;
    type SR: SharedReference;
    fn update_context_delta(
        &self,
        context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        new_concept_id: Self::Id,
    );
}

pub trait NewSnapshotConceptDelta {
    type SR: SharedReference;
    fn apply(
        &self,
        snap_shot: &mut ContextSnapShot<Self::SR>,
        new_concept_id: Uncommitted,
    );
}
type ContextConceptDelta<CI, SR: SharedReference> =
    ConceptDelta<CI, SR, SR::Share<dyn NewConceptDelta<Id = CI, SR = SR>>>;
type DirectContextConceptDelta<CI, SR: SharedReference> =
    DirectConceptDelta<CI, SR::Share<dyn NewConceptDelta<Id = CI, SR = SR>>>;
#[derive(Debug)]
pub struct NewStringConceptDelta<Id, SR>(String, PhantomData<(Id, SR)>);
impl<T: Into<String>, Id, SR> From<T> for NewStringConceptDelta<Id, SR> {
    fn from(value: T) -> Self {
        Self(value.into(), PhantomData)
    }
}
impl<SR: SharedReference> NewSnapshotConceptDelta
    for NewStringConceptDelta<ContextConceptId, SR>
{
    type SR = SR;

    fn apply(
        &self,
        snap_shot: &mut ContextSnapShot<Self::SR>,
        new_concept_id: Uncommitted,
    ) {
        let s = &self.0;
        let new_concept_id =
            snap_shot.commit_new_concept(new_concept_id, |new_concept_id| {
                (SpecificPart::String(s.into()), new_concept_id).into()
            });
        snap_shot.insert_string(s, new_concept_id);
    }
}
impl<Id: Debug + ConceptId, SR: SharedReference> NewConceptDelta
    for NewStringConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;

    fn update_context_delta(
        &self,
        context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        new_concept_id: Self::Id,
    ) {
        let s = &self.0;
        context_delta
            .string
            .entry(s.into())
            .and_modify(|v| match v {
                ValueChange::Create(_)
                | ValueChange::Update {
                    ..
                } => panic!("String \"{s}\" already exists"),
                ValueChange::Remove(id) => {
                    *v = ValueChange::Update {
                        before: *id,
                        after: new_concept_id,
                    }
                },
            })
            .or_insert_with(|| ValueChange::Create(new_concept_id));
    }
}
#[derive(Debug)]
pub struct NewCompositionConceptDelta<Id, SR>(Composition<Id>, PhantomData<SR>);
impl<Id: Debug + MixedConcept, SR: SharedReference> NewConceptDelta
    for NewCompositionConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;

    fn update_context_delta(
        &self,
        context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        new_concept_id: Self::Id,
    ) {
        let Composition {
            left_id,
            right_id,
        } = self.0;
        let cd = IndirectConceptDelta::LefthandOf(LefthandOf {
            composition: new_concept_id,
            righthand: right_id,
        })
        .into();
        context_delta.insert_delta_for_existing_concept(left_id, cd);
        let cd = IndirectConceptDelta::RighthandOf(RighthandOf {
            composition: new_concept_id,
            lefthand: left_id,
        })
        .into();
        context_delta.insert_delta_for_existing_concept(right_id, cd);
    }
}
#[derive(Debug)]
pub struct NewFreeVariableConceptDelta<Id, SR>(PhantomData<(Id, SR)>);
impl<Id: Debug, SR> Default for NewFreeVariableConceptDelta<Id, SR> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<Id: Debug + ConceptId, SR: SharedReference> NewConceptDelta
    for NewFreeVariableConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;

    fn update_context_delta(
        &self,
        _context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        _new_concept_id: Self::Id,
    ) {
        // Nothing extra needed because nothing else refers to the variable concept yet
    }
}
impl<SR: SharedReference> NewSnapshotConceptDelta
    for NewFreeVariableConceptDelta<ContextConceptId, SR>
{
    type SR = SR;

    fn apply(
        &self,
        snap_shot: &mut ContextSnapShot<Self::SR>,
        new_concept_id: Uncommitted,
    ) {
        snap_shot
            .commit_new_concept(new_concept_id, Concept::make_free_variable);
    }
}
#[derive(Debug)]
pub struct NewBoundVariableConceptDelta<Id, SR>(PhantomData<(Id, SR)>);
impl<Id, SR> Default for NewBoundVariableConceptDelta<Id, SR> {
    fn default() -> Self {
        Self(Default::default())
    }
}
impl<SR: SharedReference> NewSnapshotConceptDelta
    for NewBoundVariableConceptDelta<ContextConceptId, SR>
{
    type SR = SR;

    fn apply(
        &self,
        snap_shot: &mut ContextSnapShot<Self::SR>,
        new_concept_id: Uncommitted,
    ) {
        snap_shot
            .commit_new_concept(new_concept_id, Concept::make_bound_variable);
    }
}

impl<Id: Debug + ConceptId, SR: SharedReference> NewConceptDelta
    for NewBoundVariableConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;
    fn update_context_delta(
        &self,
        _context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        _new_concept_id: Self::Id,
    ) {
        // Nothing extra needed because nothing else refers to the variable concept yet
    }
}
#[derive(Debug)]
pub struct NewLeftConceptDelta<Id, SR> {
    composition_id: Id,
    right_id: Id,
    concrete_type: Option<ConcreteConceptType>,
    phantom: PhantomData<SR>,
}
impl<Id: Debug + MixedConcept, SR: SharedReference> NewConceptDelta
    for NewLeftConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;

    fn update_context_delta(
        &self,
        context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        new_concept_id: Self::Id,
    ) {
        let Self {
            composition_id,
            right_id,
            ..
        } = self;
        let cd = IndirectConceptDelta::RighthandOf(RighthandOf {
            composition: *composition_id,
            lefthand: new_concept_id,
        })
        .into();
        context_delta.insert_delta_for_existing_concept(*right_id, cd);
        let cd = IndirectConceptDelta::ComposedOf(Composition {
            left_id: new_concept_id,
            right_id: *right_id,
        })
        .into();
        context_delta.insert_delta_for_existing_concept(*composition_id, cd);
    }
}
#[derive(Debug)]
pub struct NewRightConceptDelta<Id, SR> {
    pub composition_id: Id,
    pub left_id: Id,
    pub concrete_type: Option<ConcreteConceptType>,
    pub phantom: PhantomData<SR>,
}
impl<Id: Debug + MixedConcept, SR: SharedReference> NewConceptDelta
    for NewRightConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;

    fn update_context_delta(
        &self,
        context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        new_concept_id: Self::Id,
    ) {
        let Self {
            composition_id,
            left_id,
            ..
        } = self;
        let cd = IndirectConceptDelta::LefthandOf(LefthandOf {
            composition: *composition_id,
            righthand: new_concept_id,
        })
        .into();
        context_delta.insert_delta_for_existing_concept(*left_id, cd);
        let cd = IndirectConceptDelta::ComposedOf(Composition {
            left_id: *left_id,
            right_id: new_concept_id,
        })
        .into();
        context_delta.insert_delta_for_existing_concept(*composition_id, cd);
    }
}
#[derive(Debug)]
/// Used for e.g. `let label_of label_of -> 'label_of'`
pub struct NewDoubleConceptDelta<Id, SR> {
    pub composition_id: Id,
    pub concrete_type: Option<ConcreteConceptType>,
    pub phantom: PhantomData<SR>,
}
impl<Id: MixedConcept, SR: SharedReference> NewConceptDelta
    for NewDoubleConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;

    fn update_context_delta(
        &self,
        context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        new_concept_id: Self::Id,
    ) {
        let Self {
            composition_id,
            ..
        } = self;
        let cd = IndirectConceptDelta::ComposedOf(Composition {
            left_id: new_concept_id,
            right_id: new_concept_id,
        })
        .into();
        context_delta.insert_delta_for_existing_concept(*composition_id, cd);
    }
}

impl<SR: SharedReference> NewSnapshotConceptDelta
    for NewDoubleConceptDelta<ContextConceptId, SR>
{
    type SR = SR;
    fn apply(
        &self,
        snap_shot: &mut ContextSnapShot<SR>,
        new_concept_id: Uncommitted,
    ) {
        let new_concept_id = snap_shot
            .commit_new_concept_with_dependent_concepts(
                new_concept_id,
                [snap_shot
                    .convert_to_committed_concept_id(self.composition_id)],
                |[composition], new_concept_id| {
                    Concept::double(
                        new_concept_id,
                        composition,
                        self.concrete_type,
                    )
                },
            );
        if let Some(cct) = self.concrete_type {
            snap_shot.insert_concrete_concept(new_concept_id, cct);
        }
    }
}
#[derive(Debug)]
pub struct NewReducesToConceptDelta<Id, SR> {
    reduction: Id,
    phantom: PhantomData<SR>,
}
impl<Id, SR> From<Id> for NewReducesToConceptDelta<Id, SR> {
    fn from(reduction: Id) -> Self {
        Self {
            reduction,
            phantom: PhantomData,
        }
    }
}
impl<Id: MixedConcept, SR: SharedReference> NewConceptDelta
    for NewReducesToConceptDelta<Id, SR>
{
    type Id = Id;
    type SR = SR;

    fn update_context_delta(
        &self,
        context_delta: &mut ContextDelta<Self::Id, Self::SR>,
        new_concept_id: Self::Id,
    ) {
        let cd = IndirectConceptDelta::ReducesFrom(new_concept_id).into();
        context_delta.insert_delta_for_existing_concept(self.reduction, cd);
    }
}

impl<SR: SharedReference> NewSnapshotConceptDelta
    for NewReducesToConceptDelta<ContextConceptId, SR>
{
    type SR = SR;
    fn apply(
        &self,
        snap_shot: &mut ContextSnapShot<SR>,
        new_concept_id: Uncommitted,
    ) {
        snap_shot.commit_new_concept_with_dependent_concepts(
            new_concept_id,
            [snap_shot.convert_to_committed_concept_id(self.reduction)],
            |[reduction_concept], new_concept_id| {
                Concept::reduction_to(new_concept_id, reduction_concept)
            },
        );
    }
}

pub type ValueChange<T> = ValueAndTypeChange<T, T>;

#[derive(Copy, Clone, PartialEq, Eq)]
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

impl<Id, SR: SharedReference> From<Composition<Id>>
    for NewCompositionConceptDelta<Id, SR>
{
    fn from(value: Composition<Id>) -> Self {
        Self(value, PhantomData)
    }
}

impl<Id: Display> Display for Composition<Id> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("({}, {})", self.left_id, self.right_id))
    }
}
