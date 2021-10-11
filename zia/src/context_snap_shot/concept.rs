use std::{collections::HashMap, convert::TryInto, fmt::Debug};

use super::concept_id::{Committed, ConceptId};
use crate::{
    concepts::{
        Concept, ConceptTrait, Hand, LefthandOf, MaybeComposition, RighthandOf,
    },
    context_delta::{
        Composition, IndirectConceptDelta, NewDirectConceptDelta, ValueChange,
    },
};

#[derive(Debug, Clone)]
pub enum Mixed<'a> {
    PreviouslyCommitted {
        original_concept: &'a Concept<Committed>,
        changes: Changes,
        // the new value of the composition
        uncommitted_composition: Option<(ConceptId, ConceptId)>,
        // the new value of the reduction
        uncommitted_reduction: Option<ConceptId>,
    },
    Uncommitted(Box<Concept<ConceptId>>),
}

#[derive(Clone, Debug, Default)]
pub struct Changes {
    // if the value is None, then this concept is no longer a lefthand of a concept with righthand ID equal to the key,
    // if the value is Some(c), then this concept is a lefthand of the concept with ID equal to the c and whose
    // righthand is now the concept with ID equal to the key
    lefthand_of: HashMap<ConceptId, Option<ConceptId>>,
    // if the value is None, then this concept is no longer a righthand of the concept with lefthand ID equal to the key,
    // if the value is Some(c), then this concept is a righthand of the concept with ID equal to the c and whose
    // leftthand is now the concept with ID equal to the key
    righthand_of: HashMap<ConceptId, Option<ConceptId>>,
    // if the value is false then this concept no longer reduces from the concept with ID equal to the key
    // if the value is true then this concept now reduces from the concept with ID equal to the key
    reduction_from: HashMap<ConceptId, bool>,
}

impl<'a> From<&'a Concept<Committed>> for Mixed<'a> {
    fn from(c: &'a Concept<Committed>) -> Self {
        Self::PreviouslyCommitted {
            original_concept: c,
            changes: Changes::default(),
            uncommitted_composition: c
                .get_composition()
                .map(|(l, r)| (l.into(), r.into())),
            uncommitted_reduction: c.get_reduction().map(|r| r.into()),
        }
    }
}

impl<'a, 'b> From<&'a NewDirectConceptDelta<ConceptId, ConceptId>>
    for Mixed<'b>
{
    fn from(delta: &'a NewDirectConceptDelta<ConceptId, ConceptId>) -> Self {
        Mixed::Uncommitted(Box::new(delta.into()))
    }
}

impl<'a> ConceptTrait for Mixed<'a> {
    type Id = ConceptId;
    type IdIterator<'b> = Box<dyn Iterator<Item = Self::Id> + 'b>;
    type IdPairIterator<'b> =
        Box<dyn Iterator<Item = (Self::Id, Self::Id)> + 'b>;

    fn id(&self) -> Self::Id {
        match self {
            Self::PreviouslyCommitted {
                original_concept,
                ..
            } => original_concept.id().into(),
            Self::Uncommitted(c) => c.id(),
        }
    }

    fn maybe_composition(&self) -> Option<MaybeComposition<Self::Id>> {
        match self {
            Self::PreviouslyCommitted {
                original_concept,
                uncommitted_composition,
                ..
            } => original_concept.maybe_composition().map(|mc| {
                mc.convert_ids_and_delete_variables::<Self::Id>()
                    .apply_composition_change(uncommitted_composition)
            }),
            Self::Uncommitted(c) => c
                .maybe_composition()
                .map(|mc| mc.convert_ids_and_delete_variables::<Self::Id>()),
        }
    }

    fn change_reduction(
        &mut self,
        change: crate::context_delta::ValueChange<Self::Id>,
    ) {
        match self {
            Self::PreviouslyCommitted {
                uncommitted_reduction,
                ..
            } => match change {
                ValueChange::Create(id)
                | ValueChange::Update {
                    after: id,
                    before: _,
                } => *uncommitted_reduction = Some(id),
                ValueChange::Remove(_) => *uncommitted_reduction = None,
            },
            Self::Uncommitted(c) => c.change_reduction(change),
        }
    }

    fn apply_indirect(&mut self, delta: &IndirectConceptDelta<Self::Id>) {
        match self {
            Self::Uncommitted(c) => c.apply_indirect(delta),
            Self::PreviouslyCommitted {
                original_concept: _,
                changes,
                uncommitted_composition,
                uncommitted_reduction: _,
            } => match delta {
                IndirectConceptDelta::ComposedOf(Composition {
                    left_id,
                    right_id,
                }) => *uncommitted_composition = Some((*left_id, *right_id)),
                IndirectConceptDelta::LefthandOf(LefthandOf {
                    composition,
                    righthand,
                }) => {
                    changes.lefthand_of.insert(*righthand, Some(*composition));
                },
                IndirectConceptDelta::NoLongerLefthandOf(righthand) => {
                    changes.lefthand_of.insert(*righthand, None);
                },
                IndirectConceptDelta::RighthandOf(RighthandOf {
                    composition,
                    lefthand,
                }) => {
                    changes.righthand_of.insert(*lefthand, Some(*composition));
                },
                IndirectConceptDelta::NoLongerRighthandOf(lefthand) => {
                    changes.righthand_of.insert(*lefthand, None);
                },
                IndirectConceptDelta::ReducesFrom(unreduced_id) => {
                    changes.reduction_from.insert(*unreduced_id, true);
                },
                IndirectConceptDelta::NoLongerReducesFrom(unreduced_id) => {
                    changes.reduction_from.insert(*unreduced_id, false);
                },
            },
        }
    }

    fn free_variable(&self) -> bool {
        match self {
            Mixed::PreviouslyCommitted {
                original_concept,
                ..
            } => original_concept.free_variable(),
            Mixed::Uncommitted(c) => c.free_variable(),
        }
    }

    fn bounded_variable(&self) -> bool {
        match self {
            Mixed::PreviouslyCommitted {
                original_concept,
                ..
            } => original_concept.bounded_variable(),
            Mixed::Uncommitted(c) => c.bounded_variable(),
        }
    }

    fn anonymous_variable(&self) -> bool {
        match self {
            Mixed::PreviouslyCommitted {
                original_concept,
                ..
            } => original_concept.anonymous_variable(),
            Mixed::Uncommitted(c) => c.anonymous_variable(),
        }
    }

    fn find_what_reduces_to_it(&self) -> Self::IdIterator<'_> {
        match self {
            Self::PreviouslyCommitted {
                original_concept,
                changes,
                ..
            } => Box::new(
                original_concept
                    .find_what_reduces_to_it()
                    .filter_map(move |unreduced_id| {
                        let unreduced_id = unreduced_id.into();
                        if changes.reduction_from.get(&unreduced_id)
                            == Some(&false)
                        {
                            None
                        } else {
                            Some(unreduced_id)
                        }
                    })
                    .chain(changes.reduction_from.iter().filter_map(
                        |(id, exists)| {
                            if *exists {
                                Some(*id)
                            } else {
                                None
                            }
                        },
                    )),
            ),
            Self::Uncommitted(c) => Box::new(c.find_what_reduces_to_it()),
        }
    }

    fn get_string(&self) -> Option<String> {
        match self {
            Mixed::PreviouslyCommitted {
                original_concept,
                ..
            } => original_concept.get_string(),
            Mixed::Uncommitted(c) => c.get_string(),
        }
    }

    fn iter_hand_of(&self, hand: Hand) -> Self::IdPairIterator<'_> {
        match &self {
            Self::PreviouslyCommitted {
                original_concept,
                changes,
                ..
            } => Box::new(original_concept.iter_hand_of(hand).filter_map(
                move |(o, c)| {
                    let o = o.into();
                    match match hand {
                        Hand::Left => &changes.lefthand_of,
                        Hand::Right => &changes.righthand_of,
                    }
                    .get(&o)
                    {
                        None => Some((o, c.into())),
                        Some(None) => None,
                        Some(Some(c)) => Some((o, *c)),
                    }
                },
            )),
            Self::Uncommitted(c) => Box::new(c.iter_hand_of(hand)),
        }
    }

    fn get_reduction(&self) -> Option<Self::Id> {
        match self {
            Mixed::PreviouslyCommitted {
                uncommitted_reduction,
                ..
            } => *uncommitted_reduction,
            Mixed::Uncommitted(c) => c.get_reduction(),
        }
    }

    fn get_composition(&self) -> Option<(Self::Id, Self::Id)> {
        match self {
            Self::PreviouslyCommitted {
                uncommitted_composition,
                ..
            } => *uncommitted_composition,
            Self::Uncommitted(c) => c.get_composition(),
        }
    }

    fn change_composition(
        &mut self,
        _change: crate::context_delta::ValueChange<[&mut Self; 2]>,
    ) -> crate::errors::ZiaResult<()> {
        unimplemented!()
    }

    fn find_as_hand_in_composition_with(
        &self,
        other_id: Self::Id,
        hand: crate::concepts::Hand,
    ) -> Option<Self::Id> {
        match self {
            Self::PreviouslyCommitted {
                original_concept,
                changes,
                ..
            } => {
                match match hand {
                    Hand::Left => &changes.lefthand_of,
                    Hand::Right => &changes.righthand_of,
                }
                .get(&other_id)
                {
                    Some(None) => return None,
                    Some(Some(c)) => return Some(*c),
                    None => (),
                };
                if let Ok(other_id) = other_id.try_into() {
                    original_concept
                        .find_as_hand_in_composition_with(other_id, hand)
                        .map(|id| id.into())
                } else {
                    None
                }
            },
            Self::Uncommitted(c) => {
                c.find_as_hand_in_composition_with(other_id, hand)
            },
        }
    }

    fn get_concrete_concept_type(
        &self,
    ) -> Option<crate::concepts::ConcreteConceptType> {
        match self {
            Self::PreviouslyCommitted {
                original_concept,
                // concept cannot change it's concrete concept type
                ..
            } => original_concept.get_concrete_concept_type(),
            Self::Uncommitted(c) => c.get_concrete_concept_type(),
        }
    }
}
