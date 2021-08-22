//  Library for the Zia programming language.
// Copyright (C) 2018 to 2020 Charles Johnson
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
    context_delta::{
        Change, Composition, DirectConceptDelta, IndirectConceptDelta,
        NewConceptDelta, NewDirectConceptDelta,
    },
    errors::{ZiaError, ZiaResult},
};
use maplit::{hashmap, hashset};
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
};

/// Data type for any type of concept.
#[derive(Clone, PartialEq)]
pub struct Concept<Id: Eq + Hash> {
    id: Id,
    concrete_part: ConcreteConcept<Id>,
    specific_part: SpecificPart<Id>,
}

impl<Id: Copy + Display + Hash + Eq> Debug for Concept<Id> {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let mut string = "{".to_string();
        if !self.concrete_part.lefthand_of.is_empty() {
            string += " lefthand_of: {";
            for key in self.concrete_part.lefthand_of.values() {
                string += &format!("{},", key);
            }
            string += "},";
        }
        if !self.concrete_part.righthand_of.is_empty() {
            string += " righthand_of: {";
            for key in self.concrete_part.righthand_of.values() {
                string += &format!("{},", key);
            }
            string += "},";
        }
        if !self.concrete_part.reduces_from.is_empty() {
            string += " reduces_from: {";
            for key in &self.concrete_part.reduces_from {
                string += &format!("{},", key);
            }
            string += "},";
        }
        string += &format!(" specific_part: {:#?}", self.specific_part);
        formatter.write_str(&(string + "}"))
    }
}

impl<Id: Copy + Debug + Eq + Hash> From<&NewDirectConceptDelta<Id>>
    for Concept<Id>
{
    fn from(ndcd: &NewDirectConceptDelta<Id>) -> Self {
        Self {
            id: ndcd.new_concept_id,
            concrete_part: ndcd.into(),
            specific_part: (&ndcd.delta).into(),
        }
    }
}

impl<Id: Copy + Display + Eq + Hash + Debug> Concept<Id> {
    pub fn id(&self) -> Id {
        self.id
    }

    pub fn compose_delta(
        &self,
        left_id: Id,
        right_id: Id,
    ) -> ZiaResult<DirectConceptDelta<Id>> {
        let after = Composition {
            left_id,
            right_id,
        };
        if let SpecificPart::Abstract(ap) = &self.specific_part {
            Ok(DirectConceptDelta::Compose {
                change: match ap.composition {
                    MaybeComposition::Composition(CompositePart {
                        lefthand,
                        righthand,
                        ..
                    }) => Change::Update {
                        before: Composition {
                            left_id: lefthand,
                            right_id: righthand,
                        },
                        after,
                    },
                    MaybeComposition::Leaf(LeafCharacter::Constant) => {
                        Change::Create(after)
                    },
                    MaybeComposition::Leaf(_) => {
                        panic!("Not sure what you are trying to do here ...")
                    },
                },
                composition_id: self.id,
            })
        } else {
            Err(ZiaError::SettingCompositionOfConcrete)
        }
    }

    pub fn make_free_variable(id: Id) -> Self {
        Self {
            id,
            concrete_part: ConcreteConcept::default(),
            specific_part: SpecificPart::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(
                    LeafCharacter::FreeVariable,
                ),
                ..AbstractPart::default()
            }),
        }
    }

    pub fn make_bound_variable(id: Id) -> Self {
        Self {
            id,
            concrete_part: ConcreteConcept::default(),
            specific_part: SpecificPart::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(
                    LeafCharacter::BoundVariable,
                ),
                ..AbstractPart::default()
            }),
        }
    }

    pub fn change_reduction(&mut self, change: Change<Id>)
    where
        Id: PartialEq + Debug,
    {
        if let SpecificPart::Abstract(ap) = &mut self.specific_part {
            match change {
                Change::Create(reduced_concept_id)
                | Change::Update {
                    after: reduced_concept_id,
                    ..
                } => {
                    ap.reduces_to = Some(reduced_concept_id);
                },
                Change::Remove(reduced_concept_id) => {
                    debug_assert_eq!(Some(reduced_concept_id), ap.reduces_to);
                    ap.reduces_to = None;
                },
            }
        }
    }

    pub fn apply_indirect(&mut self, delta: &IndirectConceptDelta<Id>) {
        match delta {
            IndirectConceptDelta::ComposedOf(Composition {
                left_id,
                right_id,
            }) => {
                if let SpecificPart::Abstract(ref mut ap) = self.specific_part {
                    ap.composition =
                        MaybeComposition::Composition(CompositePart {
                            lefthand: *left_id,
                            righthand: *right_id,
                            free_variables: hashset! {},
                            binding_variables: hashset! {},
                        });
                } else {
                    panic!("Concept isn't abstract");
                }
            },
            IndirectConceptDelta::LefthandOf(lefthand_of) => {
                lefthand_of.insert_into(&mut self.concrete_part.lefthand_of);
            },
            IndirectConceptDelta::RighthandOf(righthand_of) => {
                righthand_of.insert_into(&mut self.concrete_part.righthand_of);
            },
            IndirectConceptDelta::ReducesFrom(unreduced_id) => {
                self.concrete_part.reduces_from.insert(*unreduced_id);
            },
            IndirectConceptDelta::NoLongerLefthandOf(composition_id) => {
                self.concrete_part.lefthand_of.remove(composition_id);
            },
            IndirectConceptDelta::NoLongerRighthandOf(composition_id) => {
                self.concrete_part.righthand_of.remove(composition_id);
            },
            IndirectConceptDelta::NoLongerReducesFrom(unreduced_id) => {
                self.concrete_part.reduces_from.remove(unreduced_id);
            },
        }
    }

    /// Either a free variable or a concept that is composed of at least one free variable
    /// without a quantifying operator like `exists_such_that`
    pub fn free_variable(&self) -> bool {
        if let SpecificPart::Abstract(AbstractPart {
            composition,
            ..
        }) = &self.specific_part
        {
            match composition {
                MaybeComposition::Composition(CompositePart {
                    free_variables,
                    ..
                }) => !free_variables.is_empty(),
                MaybeComposition::Leaf(LeafCharacter::FreeVariable) => true,
                MaybeComposition::Leaf(
                    LeafCharacter::Constant | LeafCharacter::BoundVariable,
                ) => false,
            }
        } else {
            false
        }
    }

    /// Either a bounded variable or a concept that is composed of at least one bounded variable
    /// without a quantifying operator like `exists_such_that`
    pub fn bounded_variable(&self) -> bool {
        if let SpecificPart::Abstract(AbstractPart {
            composition,
            ..
        }) = &self.specific_part
        {
            match composition {
                MaybeComposition::Composition(CompositePart {
                    binding_variables,
                    ..
                }) => !binding_variables.is_empty(),
                MaybeComposition::Leaf(LeafCharacter::BoundVariable) => true,
                MaybeComposition::Leaf(
                    LeafCharacter::Constant | LeafCharacter::FreeVariable,
                ) => false,
            }
        } else {
            false
        }
    }

    /// Either a free or bounded variable or a concept that is composed of at least one bounded or free variable
    /// without a quantifying operator like `exists_such_that`
    pub fn anonymous_variable(&self) -> bool {
        if let SpecificPart::Abstract(AbstractPart {
            composition,
            ..
        }) = &self.specific_part
        {
            match composition {
                MaybeComposition::Composition(CompositePart {
                    binding_variables,
                    free_variables,
                    ..
                }) => {
                    !binding_variables.is_empty() | !free_variables.is_empty()
                },
                MaybeComposition::Leaf(
                    LeafCharacter::BoundVariable | LeafCharacter::FreeVariable,
                ) => true,
                MaybeComposition::Leaf(LeafCharacter::Constant) => false,
            }
        } else {
            false
        }
    }

    pub fn remove_reduction(&self) -> ZiaResult<Id> {
        self.get_reduction().ok_or(ZiaError::RedundantReduction)
    }

    pub fn find_what_reduces_to_it(
        &self,
    ) -> std::collections::hash_set::Iter<Id> {
        self.concrete_part.reduces_from.iter()
    }

    /// Gets the `String` value associated with `self` if it is a string concept. Otherwise returns `None`.
    pub fn get_string(&self) -> Option<String> {
        match self.specific_part {
            SpecificPart::String(ref s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn get_lefthand_of(&self) -> &HashMap<Id, Id> {
        &self.concrete_part.lefthand_of
    }

    pub fn get_righthand_of(&self) -> &HashMap<Id, Id> {
        &self.concrete_part.righthand_of
    }

    pub fn get_hand_of(&self, hand: Hand) -> &HashMap<Id, Id> {
        match hand {
            Hand::Left => self.get_lefthand_of(),
            Hand::Right => self.get_righthand_of(),
        }
    }

    /// Gets the index of the concept that `self` may reduce to.
    pub fn get_reduction(&self) -> Option<Id> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => c.reduces_to,
            _ => None,
        }
    }

    /// If concept is abstract and has a composition returns the indices of the left and right concepts that compose it as `Some((left, right))`. Otherwise returns `None`.
    pub fn get_composition(&self) -> Option<(Id, Id)> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => {
                if let MaybeComposition::Composition(CompositePart {
                    lefthand,
                    righthand,
                    ..
                }) = c.composition
                {
                    Some((lefthand, righthand))
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    pub fn change_composition(
        &mut self,
        change: Change<[&mut Self; 2]>,
    ) -> ZiaResult<()> {
        match &mut self.specific_part {
            SpecificPart::Abstract(c) => {
                match change {
                    Change::Create([left, right]) => {
                        c.composition = MaybeComposition::composition_of(
                            self.id, left, right,
                        );
                    },
                    Change::Update {
                        after: [left, right],
                        before: [before_left, before_right],
                    } => {
                        before_left.concrete_part.lefthand_of.remove(&self.id);
                        before_right
                            .concrete_part
                            .righthand_of
                            .remove(&self.id);
                        c.composition = MaybeComposition::composition_of(
                            self.id, left, right,
                        );
                    },
                    Change::Remove([before_left, before_right]) => {
                        before_left.concrete_part.lefthand_of.remove(&self.id);
                        before_right
                            .concrete_part
                            .righthand_of
                            .remove(&self.id);
                        c.composition =
                            MaybeComposition::Leaf(LeafCharacter::Constant);
                    },
                }
                Ok(())
            },
            _ => Err(ZiaError::SettingCompositionOfConcrete),
        }
    }

    pub fn find_as_lefthand_in_composition_with_righthand(
        &self,
        right_id: Id,
    ) -> Option<Id> {
        self.concrete_part.lefthand_of.get(&right_id).copied()
    }

    pub fn find_as_righthand_in_composition_with_lefthand(
        &self,
        left_id: Id,
    ) -> Option<Id> {
        self.concrete_part.righthand_of.get(&left_id).copied()
    }

    pub fn get_concrete_concept_type(&self) -> Option<ConcreteConceptType> {
        match &self.specific_part {
            SpecificPart::Concrete(cc) => Some(*cc),
            _ => None,
        }
    }

    pub fn make_reduce_to(&mut self, other: &mut Self) {
        if let SpecificPart::Abstract(ref mut ap) = &mut self.specific_part {
            ap.reduces_to = Some(other.id);
            other.concrete_part.reduces_from.insert(self.id);
        } else {
            panic!("Cannot reduce a concrete concept");
        }
    }

    pub fn make_no_longer_reduce_to(
        &mut self,
        before_reduced_concept: &mut Self,
    ) {
        if let SpecificPart::Abstract(ref mut ap) = &mut self.specific_part {
            before_reduced_concept.concrete_part.reduces_from.remove(&self.id);
            ap.reduces_to = None;
        } else {
            panic!("Concrete concept did not reduce to anything");
        }
    }

    pub fn composition_of(id: Id, left: &mut Self, right: &mut Self) -> Self {
        Self {
            id,
            specific_part: SpecificPart::composition_of(id, left, right),
            concrete_part: ConcreteConcept::default(),
        }
    }

    pub fn lefthand_of(
        id: Id,
        right: &mut Self,
        composition: &mut Self,
        concrete_concept_type: Option<ConcreteConceptType>,
    ) -> ZiaResult<Self> {
        if let SpecificPart::Abstract(ap) = &mut composition.specific_part {
            if let MaybeComposition::Leaf(LeafCharacter::Constant) =
                ap.composition
            {
                RighthandOf {
                    composition: composition.id,
                    lefthand: id,
                }
                .insert_into(&mut right.concrete_part.righthand_of);
                let mut binding_variables = hashset! {};
                let mut free_variables = hashset! {};
                if right.get_concrete_concept_type()
                    == Some(ConcreteConceptType::ExistsSuchThat)
                {
                    binding_variables.insert(id);
                } else if let SpecificPart::Abstract(AbstractPart {
                    composition: MaybeComposition::Composition(cp),
                    ..
                }) = &right.specific_part
                {
                    binding_variables = cp.binding_variables.clone();
                    free_variables = cp.free_variables.clone();
                };
                ap.composition = MaybeComposition::Composition(CompositePart {
                    binding_variables,
                    free_variables,
                    lefthand: id,
                    righthand: right.id,
                });
                Ok(Self {
                    concrete_part: ConcreteConcept {
                        lefthand_of: LefthandOf {
                            composition: composition.id,
                            righthand: right.id,
                        }
                        .start_mapping(),
                        ..ConcreteConcept::default()
                    },
                    id,
                    specific_part: concrete_concept_type.map_or_else(
                        || {
                            SpecificPart::Abstract(AbstractPart {
                                composition: MaybeComposition::Leaf(
                                    LeafCharacter::Constant,
                                ),
                                ..AbstractPart::default()
                            })
                        },
                        SpecificPart::Concrete,
                    ),
                })
            } else {
                Err(ZiaError::BadComposition)
            }
        } else {
            Err(ZiaError::SettingCompositionOfConcrete)
        }
    }

    pub fn righthand_of(
        id: Id,
        left: &mut Self,
        composition: &mut Self,
        concrete_concept_type: Option<ConcreteConceptType>,
    ) -> ZiaResult<Self> {
        if let SpecificPart::Abstract(ap) = &mut composition.specific_part {
            if let MaybeComposition::Leaf(LeafCharacter::Constant) =
                ap.composition
            {
                LefthandOf {
                    composition: composition.id,
                    righthand: id,
                }
                .insert_into(&mut left.concrete_part.lefthand_of);
                let mut binding_variables = hashset! {};
                let mut free_variables = hashset! {};
                if concrete_concept_type
                    == Some(ConcreteConceptType::ExistsSuchThat)
                {
                    binding_variables.insert(left.id);
                } else if let SpecificPart::Abstract(AbstractPart {
                    composition: MaybeComposition::Composition(cp),
                    ..
                }) = &left.specific_part
                {
                    binding_variables = cp.binding_variables.clone();
                    free_variables = cp.free_variables.clone();
                };
                ap.composition = MaybeComposition::Composition(CompositePart {
                    binding_variables,
                    free_variables,
                    lefthand: left.id,
                    righthand: id,
                });
                Ok(Self {
                    concrete_part: ConcreteConcept {
                        righthand_of: RighthandOf {
                            composition: composition.id,
                            lefthand: left.id,
                        }
                        .start_mapping(),
                        ..ConcreteConcept::default()
                    },
                    id,
                    specific_part: concrete_concept_type.map_or_else(
                        || {
                            SpecificPart::Abstract(AbstractPart {
                                composition: MaybeComposition::Leaf(
                                    LeafCharacter::Constant,
                                ),
                                ..AbstractPart::default()
                            })
                        },
                        SpecificPart::Concrete,
                    ),
                })
            } else {
                Err(ZiaError::BadComposition)
            }
        } else {
            Err(ZiaError::SettingCompositionOfConcrete)
        }
    }

    pub fn double(
        id: Id,
        composition: &mut Self,
        concrete_concept_type: Option<ConcreteConceptType>,
    ) -> Self {
        let concept = Self {
            concrete_part: ConcreteConcept {
                lefthand_of: LefthandOf {
                    composition: composition.id,
                    righthand: id,
                }
                .start_mapping(),
                righthand_of: RighthandOf {
                    composition: composition.id,
                    lefthand: id,
                }
                .start_mapping(),
                reduces_from: hashset! {},
            },
            id,
            specific_part: concrete_concept_type.map_or_else(
                || {
                    SpecificPart::Abstract(AbstractPart {
                        composition: MaybeComposition::Leaf(
                            LeafCharacter::Constant,
                        ),
                        reduces_to: None,
                    })
                },
                SpecificPart::Concrete,
            ),
        };
        if let SpecificPart::Abstract(AbstractPart {
            composition,
            ..
        }) = &mut composition.specific_part
        {
            *composition = MaybeComposition::Composition(CompositePart {
                lefthand: id,
                righthand: id,
                free_variables: hashset! {},
                binding_variables: hashset! {},
            });
        } else {
            panic!("tried to compose a conctrete concept");
        }
        concept
    }

    pub fn reduction_to(id: Id, reduction: &mut Self) -> Self {
        let new_concept = Self {
            concrete_part: ConcreteConcept::default(),
            id,
            specific_part: SpecificPart::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(LeafCharacter::Constant),
                reduces_to: Some(reduction.id),
            }),
        };
        reduction.concrete_part.reduces_from.insert(id);
        new_concept
    }
}

#[derive(Clone, PartialEq)]
pub enum SpecificPart<Id: Eq + Hash> {
    /// A concrete concept cannot be further reduced or defined as a composition.
    Concrete(ConcreteConceptType),
    /// An abstract concept can reduce to any other concept (whose normal form isn't the former
    /// concept) and can be defined as the composition of any two concepts.
    Abstract(AbstractPart<Id>),
    /// A string concept is associated with a `String` value by the `MaybeString` trait.
    String(String),
}

#[derive(Clone, Copy)]
pub enum Hand {
    Left,
    Right,
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConcreteConceptType {
    Label,
    Define,
    Reduction,
    Let,
    True,
    False,
    Associativity,
    Right,
    Left,
    Precedence,
    Default,
    GreaterThan,
    Implication,
    ExistsSuchThat,
}

impl<Id: Eq + Hash> From<ConcreteConceptType> for SpecificPart<Id> {
    fn from(cc: ConcreteConceptType) -> Self {
        Self::Concrete(cc)
    }
}

impl<Id: Copy + Eq + Hash> From<&NewConceptDelta<Id>> for SpecificPart<Id> {
    fn from(delta: &NewConceptDelta<Id>) -> Self {
        match delta {
            NewConceptDelta::BoundVariable => Self::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(
                    LeafCharacter::BoundVariable,
                ),
                reduces_to: None,
            }),
            NewConceptDelta::FreeVariable => Self::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(
                    LeafCharacter::FreeVariable,
                ),
                reduces_to: None,
            }),
            NewConceptDelta::Composition(Composition {
                left_id,
                right_id,
            }) => Self::Abstract(AbstractPart {
                composition: MaybeComposition::Composition(CompositePart {
                    binding_variables: hashset! {},
                    free_variables: hashset! {},
                    lefthand: *left_id,
                    righthand: *right_id,
                }),
                reduces_to: None,
            }),
            NewConceptDelta::Left {
                concrete_type,
                ..
            }
            | NewConceptDelta::Right {
                concrete_type,
                ..
            }
            | NewConceptDelta::Double {
                concrete_type,
                ..
            } => concrete_type.map_or_else(
                || {
                    Self::Abstract(AbstractPart {
                        composition: MaybeComposition::Leaf(
                            LeafCharacter::Constant,
                        ),
                        reduces_to: None,
                    })
                },
                Self::Concrete,
            ),
            NewConceptDelta::String(s) => Self::String(s.clone()),
            NewConceptDelta::ReducesTo {
                reduction,
            } => Self::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(LeafCharacter::Constant),
                reduces_to: Some(*reduction),
            }),
        }
    }
}

impl<Id: Copy + Debug + Eq + Hash> SpecificPart<Id> {
    pub fn free_variable() -> Self {
        Self::Abstract(AbstractPart {
            composition: MaybeComposition::Leaf(LeafCharacter::FreeVariable),
            reduces_to: None,
        })
    }

    pub fn bound_variable() -> Self {
        Self::Abstract(AbstractPart {
            composition: MaybeComposition::Leaf(LeafCharacter::BoundVariable),
            reduces_to: None,
        })
    }

    fn composition_of(
        composition_id: Id,
        left: &mut Concept<Id>,
        right: &mut Concept<Id>,
    ) -> Self {
        Self::Abstract(AbstractPart {
            composition: MaybeComposition::composition_of(
                composition_id,
                left,
                right,
            ),
            ..AbstractPart::default()
        })
    }
}

impl<Id: Eq + Hash> Default for SpecificPart<Id> {
    fn default() -> Self {
        Self::Abstract(AbstractPart::default())
    }
}

impl<Id: Copy + Display + Eq + Hash> Debug for SpecificPart<Id> {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str(&match *self {
            Self::Concrete(ref cc) => format!("{:#?}", cc),
            Self::Abstract(ref ap) => format!("{:#?}", ap),
            Self::String(ref s) => format_string(s),
        })
    }
}

pub fn format_string(s: &str) -> String {
    format!("\"{}\"", s)
}

impl<Id: Eq + Hash> From<(AbstractPart<Id>, Id)> for Concept<Id> {
    fn from((ap, id): (AbstractPart<Id>, Id)) -> Self {
        (SpecificPart::Abstract(ap), id).into()
    }
}

impl<Id: Eq + Hash, T: Into<SpecificPart<Id>>> From<(T, Id)> for Concept<Id> {
    fn from((sp, id): (T, Id)) -> Self {
        Self {
            id,
            concrete_part: ConcreteConcept::default(),
            specific_part: sp.into(),
        }
    }
}

impl<Id: Eq + Hash> From<(String, Id)> for Concept<Id> {
    fn from((string, id): (String, Id)) -> Self {
        (SpecificPart::String(string), id).into()
    }
}

/// An abstract concept can reduce to other concepts and be defined as a composition of two other concepts.
#[derive(Clone, PartialEq)]
pub struct AbstractPart<Id: Eq + Hash> {
    /// The concept may be defined as a composition of two other concepts.
    composition: MaybeComposition<Id>,
    /// The concept may reduce to another concept.
    reduces_to: Option<Id>,
}

#[derive(Clone, PartialEq)]
pub struct CompositePart<Id: Eq + Hash> {
    /// concept id for lefthand part of the composition
    lefthand: Id,
    /// concept id for righthand part of the composition
    righthand: Id,
    /// The concept's composition might contain free variables
    free_variables: HashSet<Id>,
    /// The concept's composition might contain binding variables
    binding_variables: HashSet<Id>,
}

#[derive(Clone, PartialEq)]
pub enum MaybeComposition<Id: Eq + Hash> {
    Composition(CompositePart<Id>),
    // true if concept is variable
    Leaf(LeafCharacter),
}

#[derive(Clone, PartialEq)]
pub enum LeafCharacter {
    Constant,      // Has a constant meaning regardless of the context search
    FreeVariable, /* Can substitute consistently with any other concept within a context search when finding generalisations */
    BoundVariable, /* Cannot be substituted for when finding generalisations, instead used to find examples that satisfy a property */
}

impl<Id: Copy + Debug + Eq + Hash> MaybeComposition<Id> {
    fn composition_of(
        composition_id: Id,
        left: &mut Concept<Id>,
        right: &mut Concept<Id>,
    ) -> Self {
        LefthandOf {
            composition: composition_id,
            righthand: right.id,
        }
        .insert_into(&mut left.concrete_part.lefthand_of);
        RighthandOf {
            composition: composition_id,
            lefthand: left.id,
        }
        .insert_into(&mut right.concrete_part.righthand_of);
        let mut free_variables = hashset! {};
        let mut binding_variables = hashset! {};
        if let SpecificPart::Abstract(ap) = &right.specific_part {
            match &ap.composition {
                Self::Composition(cp) => {
                    free_variables.extend(&cp.free_variables);
                    binding_variables.extend(&cp.binding_variables);
                },
                Self::Leaf(LeafCharacter::FreeVariable) => {
                    free_variables.insert(right.id);
                },
                Self::Leaf(LeafCharacter::BoundVariable) => {
                    binding_variables.insert(right.id);
                },
                Self::Leaf(LeafCharacter::Constant) => {},
            }
        };
        if let SpecificPart::Abstract(ap) = &left.specific_part {
            match &ap.composition {
                Self::Composition(cp) => {
                    free_variables
                        .retain(|v| !cp.binding_variables.contains(v));
                    free_variables.extend(&cp.free_variables);
                    binding_variables
                        .retain(|v| !cp.free_variables.contains(v));
                    binding_variables.extend(&cp.binding_variables);
                },
                Self::Leaf(LeafCharacter::FreeVariable) => {
                    free_variables.insert(right.id);
                },
                Self::Leaf(LeafCharacter::BoundVariable) => {
                    binding_variables.insert(right.id);
                },
                Self::Leaf(LeafCharacter::Constant) => {},
            }
        }
        Self::Composition(CompositePart {
            lefthand: left.id,
            righthand: right.id,
            free_variables,
            binding_variables,
        })
    }
}

impl<Id: Copy + Display + Eq + Hash> Debug for AbstractPart<Id> {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str("{")?;
        if let MaybeComposition::Composition(CompositePart {
            lefthand,
            righthand,
            ..
        }) = self.composition
        {
            formatter.write_str(&format!(
                "composition: {}, {},",
                lefthand, righthand
            ))?;
        }
        self.reduces_to.iter().try_for_each(|r| {
            formatter.write_str(&format!("reduces_to: {},", r))
        })?;
        formatter.write_str("}")
    }
}

impl<Id: Eq + Hash> Default for AbstractPart<Id> {
    /// The default concept doesn't have a composition and doesn't further reduce.
    fn default() -> Self {
        Self {
            composition: MaybeComposition::Leaf(LeafCharacter::Constant),
            reduces_to: None,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct ConcreteConcept<Id: Eq + Hash> {
    /// Maps each concept that is the righthand of a composition with the current concept being the lefthand to that composition's concept
    lefthand_of: HashMap<Id, Id>,
    /// Maps each concept that is the lefthand of a composition with the current concept being the righthand to that composition's concept
    righthand_of: HashMap<Id, Id>,
    /// Set of all indices of the concepts which reduce to this concept.
    reduces_from: HashSet<Id>,
}

impl<Id: Eq + Hash> Default for ConcreteConcept<Id> {
    fn default() -> Self {
        Self {
            lefthand_of: hashmap! {},
            righthand_of: hashmap! {},
            reduces_from: hashset! {},
        }
    }
}

impl<Id: Copy + Debug + Eq + Hash> From<&NewDirectConceptDelta<Id>>
    for ConcreteConcept<Id>
{
    fn from(delta: &NewDirectConceptDelta<Id>) -> Self {
        match delta.delta {
            NewConceptDelta::FreeVariable
            | NewConceptDelta::BoundVariable
            | NewConceptDelta::Composition(_)
            | NewConceptDelta::ReducesTo {
                ..
            }
            | NewConceptDelta::String(_) => Self::default(),
            NewConceptDelta::Double {
                composition_id,
                ..
            } => Self {
                lefthand_of: LefthandOf {
                    composition: composition_id,
                    righthand: delta.new_concept_id,
                }
                .start_mapping(),
                righthand_of: RighthandOf {
                    composition: composition_id,
                    lefthand: delta.new_concept_id,
                }
                .start_mapping(),
                ..Self::default()
            },
            NewConceptDelta::Left {
                composition_id,
                right_id,
                ..
            } => Self {
                lefthand_of: LefthandOf {
                    composition: composition_id,
                    righthand: right_id,
                }
                .start_mapping(),
                ..Self::default()
            },
            NewConceptDelta::Right {
                composition_id,
                left_id,
                ..
            } => Self {
                righthand_of: RighthandOf {
                    composition: composition_id,
                    lefthand: left_id,
                }
                .start_mapping(),
                ..Self::default()
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct LefthandOf<Id> {
    pub composition: Id,
    pub righthand: Id,
}

impl<Id: Copy + Eq + Hash + Debug> LefthandOf<Id> {
    fn start_mapping(&self) -> HashMap<Id, Id> {
        hashmap! {self.righthand => self.composition}
    }

    fn insert_into(&self, map: &mut HashMap<Id, Id>) {
        if let Some(prev_comp) = map.insert(self.righthand, self.composition) {
            debug_assert_eq!(self.composition, prev_comp, "at most one concept can be the composition of a given pair of concepts");
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RighthandOf<Id> {
    pub composition: Id,
    pub lefthand: Id,
}

impl<Id: Copy + Eq + Hash> RighthandOf<Id> {
    fn start_mapping(&self) -> HashMap<Id, Id> {
        hashmap! {self.lefthand => self.composition}
    }

    fn insert_into(&self, map: &mut HashMap<Id, Id>)
    where
        Id: Debug,
    {
        if let Some(prev_comp) = map.insert(self.lefthand, self.composition) {
            debug_assert_eq!(self.composition, prev_comp, "at most one concept can be the composition of a given pair of concepts");
        }
    }
}
