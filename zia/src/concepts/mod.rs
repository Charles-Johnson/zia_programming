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
    fmt::Debug,
};

/// Data type for any type of concept.
#[derive(Clone, PartialEq)]
pub struct Concept {
    id: usize,
    concrete_part: ConcreteConcept,
    specific_part: SpecificPart,
}

impl Debug for Concept {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let mut string = "{".to_string();
        if !self.concrete_part.lefthand_of.is_empty() {
            string += " lefthand_of: {";
            let mut unorder_keys: Vec<&usize> =
                self.concrete_part.lefthand_of.values().collect();
            unorder_keys.sort();
            for key in unorder_keys {
                string += &format!("{},", key);
            }
            string += "},";
        }
        if !self.concrete_part.righthand_of.is_empty() {
            string += " righthand_of: {";
            let mut unorder_keys: Vec<&usize> =
                self.concrete_part.righthand_of.values().collect();
            unorder_keys.sort();
            for key in unorder_keys {
                string += &format!("{},", key);
            }
            string += "},";
        }
        if !self.concrete_part.reduces_from.is_empty() {
            string += " reduces_from: {";
            let mut unorder_keys: Vec<&usize> =
                self.concrete_part.reduces_from.iter().collect();
            unorder_keys.sort();
            for key in unorder_keys {
                string += &format!("{},", key);
            }
            string += "},";
        }
        string += &format!(" specific_part: {:#?}", self.specific_part);
        formatter.write_str(&(string + "}"))
    }
}

impl From<&NewDirectConceptDelta> for Concept {
    fn from(ndcd: &NewDirectConceptDelta) -> Self {
        Self {
            id: ndcd.new_concept_id,
            concrete_part: ndcd.into(),
            specific_part: (&ndcd.delta).into(),
        }
    }
}

impl Concept {
    pub const fn id(&self) -> usize {
        self.id
    }

    pub fn compose_delta(
        &self,
        left_id: usize,
        right_id: usize,
    ) -> ZiaResult<DirectConceptDelta> {
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

    pub fn make_free_variable(id: usize) -> Self {
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

    pub fn make_bound_variable(id: usize) -> Self {
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

    pub fn change_reduction(&mut self, change: Change<usize>) {
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

    pub fn apply_indirect(&mut self, delta: &IndirectConceptDelta) {
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
                MaybeComposition::Leaf(LeafCharacter::Constant)
                | MaybeComposition::Leaf(LeafCharacter::BoundVariable) => false,
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
                MaybeComposition::Leaf(LeafCharacter::Constant)
                | MaybeComposition::Leaf(LeafCharacter::FreeVariable) => false,
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
                MaybeComposition::Leaf(LeafCharacter::BoundVariable)
                | MaybeComposition::Leaf(LeafCharacter::FreeVariable) => true,
                MaybeComposition::Leaf(LeafCharacter::Constant) => false,
            }
        } else {
            false
        }
    }

    pub fn remove_reduction(&self) -> ZiaResult<usize> {
        self.get_reduction().ok_or(ZiaError::RedundantReduction)
    }

    pub fn find_what_reduces_to_it(
        &self,
    ) -> std::collections::hash_set::Iter<usize> {
        self.concrete_part.reduces_from.iter()
    }

    /// Gets the `String` value associated with `self` if it is a string concept. Otherwise returns `None`.
    pub fn get_string(&self) -> Option<String> {
        match self.specific_part {
            SpecificPart::String(ref s) => Some(s.clone()),
            _ => None,
        }
    }

    pub const fn get_lefthand_of(&self) -> &HashMap<usize, usize> {
        &self.concrete_part.lefthand_of
    }

    pub const fn get_righthand_of(&self) -> &HashMap<usize, usize> {
        &self.concrete_part.righthand_of
    }

    pub const fn get_hand_of(&self, hand: Hand) -> &HashMap<usize, usize> {
        match hand {
            Hand::Left => self.get_lefthand_of(),
            Hand::Right => self.get_righthand_of(),
        }
    }

    /// Gets the index of the concept that `self` may reduce to.
    pub const fn get_reduction(&self) -> Option<usize> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => c.reduces_to,
            _ => None,
        }
    }

    /// If concept is abstract and has a composition returns the indices of the left and right concepts that compose it as `Some((left, right))`. Otherwise returns `None`.
    pub const fn get_composition(&self) -> Option<(usize, usize)> {
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
        right_id: usize,
    ) -> Option<usize> {
        self.concrete_part.lefthand_of.get(&right_id).copied()
    }

    pub fn find_as_righthand_in_composition_with_lefthand(
        &self,
        left_id: usize,
    ) -> Option<usize> {
        self.concrete_part.righthand_of.get(&left_id).copied()
    }

    pub const fn get_concrete_concept_type(
        &self,
    ) -> Option<ConcreteConceptType> {
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
            panic!("Cannot reduce a concrete concept")
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

    pub fn composition_of(
        id: usize,
        left: &mut Self,
        right: &mut Self,
    ) -> Self {
        Self {
            id,
            specific_part: SpecificPart::composition_of(id, left, right),
            concrete_part: ConcreteConcept::default(),
        }
    }

    pub fn lefthand_of(
        id: usize,
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
        id: usize,
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
        id: usize,
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

    pub fn reduction_to(id: usize, reduction: &mut Self) -> Self {
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
pub enum SpecificPart {
    /// A concrete concept cannot be further reduced or defined as a composition.
    Concrete(ConcreteConceptType),
    /// An abstract concept can reduce to any other concept (whose normal form isn't the former
    /// concept) and can be defined as the composition of any two concepts.
    Abstract(AbstractPart),
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

impl From<ConcreteConceptType> for SpecificPart {
    fn from(cc: ConcreteConceptType) -> Self {
        Self::Concrete(cc)
    }
}

impl From<&NewConceptDelta> for SpecificPart {
    fn from(delta: &NewConceptDelta) -> Self {
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

impl SpecificPart {
    pub const fn free_variable() -> Self {
        Self::Abstract(AbstractPart {
            composition: MaybeComposition::Leaf(LeafCharacter::FreeVariable),
            reduces_to: None,
        })
    }

    pub const fn bound_variable() -> Self {
        Self::Abstract(AbstractPart {
            composition: MaybeComposition::Leaf(LeafCharacter::BoundVariable),
            reduces_to: None,
        })
    }

    fn composition_of(
        composition_id: usize,
        left: &mut Concept,
        right: &mut Concept,
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

impl Default for SpecificPart {
    fn default() -> Self {
        Self::Abstract(AbstractPart::default())
    }
}

impl Debug for SpecificPart {
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

impl From<(AbstractPart, usize)> for Concept {
    fn from((ap, id): (AbstractPart, usize)) -> Self {
        (SpecificPart::Abstract(ap), id).into()
    }
}

impl<T: Into<SpecificPart>> From<(T, usize)> for Concept {
    fn from((sp, id): (T, usize)) -> Self {
        Self {
            id,
            concrete_part: ConcreteConcept::default(),
            specific_part: sp.into(),
        }
    }
}

impl From<(String, usize)> for Concept {
    fn from((string, id): (String, usize)) -> Self {
        (SpecificPart::String(string), id).into()
    }
}

/// An abstract concept can reduce to other concepts and be defined as a composition of two other concepts.
#[derive(Clone, PartialEq)]
pub struct AbstractPart {
    /// The concept may be defined as a composition of two other concepts.
    composition: MaybeComposition,
    /// The concept may reduce to another concept.
    reduces_to: Option<usize>,
}

#[derive(Clone, PartialEq)]
pub struct CompositePart {
    /// concept id for lefthand part of the composition
    lefthand: usize,
    /// concept id for righthand part of the composition
    righthand: usize,
    /// The concept's composition might contain free variables
    free_variables: HashSet<usize>,
    /// The concept's composition might contain binding variables
    binding_variables: HashSet<usize>,
}

#[derive(Clone, PartialEq)]
pub enum MaybeComposition {
    Composition(CompositePart),
    // true if concept is variable
    Leaf(LeafCharacter),
}

#[derive(Clone, PartialEq)]
pub enum LeafCharacter {
    Constant,      // Has a constant meaning regardless of the context search
    FreeVariable, /* Can substitute consistently with any other concept within a context search when finding generalisations */
    BoundVariable, /* Cannot be substituted for when finding generalisations, instead used to find examples that satisfy a property */
}

impl MaybeComposition {
    fn composition_of(
        composition_id: usize,
        left: &mut Concept,
        right: &mut Concept,
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

impl Debug for AbstractPart {
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

impl Default for AbstractPart {
    /// The default concept doesn't have a composition and doesn't further reduce.
    fn default() -> Self {
        Self {
            composition: MaybeComposition::Leaf(LeafCharacter::Constant),
            reduces_to: None,
        }
    }
}

#[derive(Clone, PartialEq, Default)]
pub struct ConcreteConcept {
    /// Maps each concept that is the righthand of a composition with the current concept being the lefthand to that composition's concept
    lefthand_of: HashMap<usize, usize>,
    /// Maps each concept that is the lefthand of a composition with the current concept being the righthand to that composition's concept
    righthand_of: HashMap<usize, usize>,
    /// Set of all indices of the concepts which reduce to this concept.
    reduces_from: HashSet<usize>,
}

impl From<&NewDirectConceptDelta> for ConcreteConcept {
    fn from(delta: &NewDirectConceptDelta) -> Self {
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
pub struct LefthandOf {
    pub composition: usize,
    pub righthand: usize,
}

impl LefthandOf {
    fn start_mapping(&self) -> HashMap<usize, usize> {
        hashmap! {self.righthand => self.composition}
    }

    fn insert_into(&self, map: &mut HashMap<usize, usize>) {
        if let Some(prev_comp) = map.insert(self.righthand, self.composition) {
            debug_assert_eq!(self.composition, prev_comp, "at most one concept can be the composition of a given pair of concepts");
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RighthandOf {
    pub composition: usize,
    pub lefthand: usize,
}

impl RighthandOf {
    fn start_mapping(&self) -> HashMap<usize, usize> {
        hashmap! {self.lefthand => self.composition}
    }

    fn insert_into(&self, map: &mut HashMap<usize, usize>) {
        if let Some(prev_comp) = map.insert(self.lefthand, self.composition) {
            debug_assert_eq!(self.composition, prev_comp, "at most one concept can be the composition of a given pair of concepts");
        }
    }
}
