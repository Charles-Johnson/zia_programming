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
use maplit::hashset;
use std::{collections::HashSet, fmt::Debug};

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
                self.concrete_part.lefthand_of.iter().collect();
            unorder_keys.sort();
            for key in unorder_keys {
                string += &format!("{},", key);
            }
            string += "},";
        }
        if !self.concrete_part.righthand_of.is_empty() {
            string += " righthand_of: {";
            let mut unorder_keys: Vec<&usize> =
                self.concrete_part.righthand_of.iter().collect();
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
            concrete_part: (&ndcd.delta).into(),
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
                    MaybeComposition::Leaf(false) => Change::Create(after),
                    MaybeComposition::Leaf(true) => {
                        panic!("Not sure what you are trying to do here ...")
                    },
                },
                composition_id: self.id,
            })
        } else {
            Err(ZiaError::SettingCompositionOfConcrete)
        }
    }

    pub fn make_variable(id: usize) -> Concept {
        Self {
            id,
            concrete_part: Default::default(),
            specific_part: SpecificPart::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(true),
                ..Default::default()
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
            IndirectConceptDelta::LefthandOf(composition_id) => {
                self.concrete_part.lefthand_of.insert(*composition_id);
            },
            IndirectConceptDelta::RighthandOf(composition_id) => {
                self.concrete_part.righthand_of.insert(*composition_id);
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

    pub fn variable(&self) -> bool {
        if let SpecificPart::Abstract(AbstractPart {
            composition,
            ..
        }) = &self.specific_part
        {
            match composition {
                MaybeComposition::Composition(CompositePart {
                    free_variables,
                    binding_variables,
                    ..
                }) => {
                    !free_variables.is_empty() || !binding_variables.is_empty()
                },
                MaybeComposition::Leaf(is_variable) => *is_variable,
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

    pub const fn get_lefthand_of(&self) -> &HashSet<usize> {
        &self.concrete_part.lefthand_of
    }

    pub const fn get_righthand_of(&self) -> &HashSet<usize> {
        &self.concrete_part.righthand_of
    }

    /// Gets the index of the concept that `self` may reduce to.
    pub fn get_reduction(&self) -> Option<usize> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => c.reduces_to,
            _ => None,
        }
    }

    /// If concept is abstract and has a composition returns the indices of the left and right concepts that compose it as `Some((left, right))`. Otherwise returns `None`.
    pub fn get_composition(&self) -> Option<(usize, usize)> {
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
        change: Change<[&mut Concept; 2]>,
    ) -> ZiaResult<()> {
        let variable = self.variable();
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
                        c.composition = MaybeComposition::Leaf(variable);
                    },
                }
                Ok(())
            },
            _ => Err(ZiaError::SettingCompositionOfConcrete),
        }
    }

    pub fn find_definition(&self, right: &Self) -> Option<usize> {
        let mut candidates = self
            .concrete_part
            .lefthand_of
            .intersection(&right.concrete_part.righthand_of);
        candidates.next().map(|index| {
            candidates.next().map_or(*index, |_| {
                panic!("Multiple definitions with the same lefthand and righthand pair exist.")
            })
        })
    }

    pub fn get_concrete_concept_type(&self) -> Option<ConcreteConceptType> {
        match &self.specific_part {
            SpecificPart::Concrete(cc) => Some(*cc),
            _ => None,
        }
    }

    pub fn make_reduce_to(&mut self, other: &mut Concept) {
        if let SpecificPart::Abstract(ref mut ap) = &mut self.specific_part {
            ap.reduces_to = Some(other.id);
            other.concrete_part.reduces_from.insert(self.id);
        } else {
            panic!("Cannot reduce a concrete concept")
        }
    }

    pub fn make_no_longer_reduce_to(
        &mut self,
        before_reduced_concept: &mut Concept,
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
        left: &mut Concept,
        right: &mut Concept,
    ) -> Self {
        Self {
            id,
            specific_part: SpecificPart::composition_of(id, left, right),
            concrete_part: ConcreteConcept::default(),
        }
    }

    pub fn lefthand_of(
        id: usize,
        right: &mut Concept,
        composition: &mut Concept,
        concrete_concept_type: Option<ConcreteConceptType>,
    ) -> ZiaResult<Self> {
        if let SpecificPart::Abstract(ap) = &mut composition.specific_part {
            match ap.composition {
                MaybeComposition::Composition(_)
                | MaybeComposition::Leaf(true) => Err(ZiaError::BadComposition),
                MaybeComposition::Leaf(false) => {
                    right.concrete_part.righthand_of.insert(composition.id);
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
                    ap.composition =
                        MaybeComposition::Composition(CompositePart {
                            binding_variables,
                            free_variables,
                            lefthand: id,
                            righthand: right.id,
                        });
                    Ok(Self {
                        concrete_part: ConcreteConcept {
                            lefthand_of: hashset! {composition.id},
                            ..Default::default()
                        },
                        id,
                        specific_part: concrete_concept_type.map_or_else(
                            || {
                                SpecificPart::Abstract(AbstractPart {
                                    composition: MaybeComposition::Leaf(false),
                                    ..Default::default()
                                })
                            },
                            SpecificPart::Concrete,
                        ),
                    })
                },
            }
        } else {
            Err(ZiaError::SettingCompositionOfConcrete)
        }
    }

    pub fn righthand_of(
        id: usize,
        left: &mut Concept,
        composition: &mut Concept,
        concrete_concept_type: Option<ConcreteConceptType>,
    ) -> ZiaResult<Self> {
        if let SpecificPart::Abstract(ap) = &mut composition.specific_part {
            match ap.composition {
                MaybeComposition::Composition(_)
                | MaybeComposition::Leaf(true) => Err(ZiaError::BadComposition),
                MaybeComposition::Leaf(false) => {
                    left.concrete_part.lefthand_of.insert(composition.id);
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
                    ap.composition =
                        MaybeComposition::Composition(CompositePart {
                            binding_variables,
                            free_variables,
                            lefthand: left.id,
                            righthand: id,
                        });
                    Ok(Self {
                        concrete_part: ConcreteConcept {
                            righthand_of: hashset! {composition.id},
                            ..Default::default()
                        },
                        id,
                        specific_part: concrete_concept_type.map_or_else(
                            || {
                                SpecificPart::Abstract(AbstractPart {
                                    composition: MaybeComposition::Leaf(false),
                                    ..Default::default()
                                })
                            },
                            SpecificPart::Concrete,
                        ),
                    })
                },
            }
        } else {
            Err(ZiaError::SettingCompositionOfConcrete)
        }
    }

    pub fn double(
        id: usize,
        composition: &mut Concept,
        concrete_concept_type: Option<ConcreteConceptType>,
    ) -> Self {
        let concept = Self {
            concrete_part: ConcreteConcept {
                lefthand_of: hashset! {composition.id},
                righthand_of: hashset! {composition.id},
                reduces_from: hashset! {},
            },
            id,
            specific_part: concrete_concept_type.map_or_else(
                || {
                    SpecificPart::Abstract(AbstractPart {
                        composition: MaybeComposition::Leaf(false),
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

    pub fn reduction_to(
        id: usize,
        reduction: &mut Concept,
        variable: bool,
    ) -> Self {
        let new_concept = Self {
            concrete_part: Default::default(),
            id,
            specific_part: SpecificPart::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(variable),
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
            NewConceptDelta::Variable => Self::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(true),
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
                        composition: MaybeComposition::Leaf(false),
                        reduces_to: None,
                    })
                },
                Self::Concrete,
            ),
            NewConceptDelta::String(s) => Self::String(s.clone()),
            NewConceptDelta::ReducesTo {
                reduction,
                variable,
            } => Self::Abstract(AbstractPart {
                composition: MaybeComposition::Leaf(*variable),
                reduces_to: Some(*reduction),
            }),
        }
    }
}

impl SpecificPart {
    pub const fn variable() -> Self {
        Self::Abstract(AbstractPart {
            composition: MaybeComposition::Leaf(true),
            reduces_to: None,
        })
    }

    fn composition_of(
        composition_id: usize,
        left: &mut Concept,
        right: &mut Concept,
    ) -> Self {
        left.concrete_part.lefthand_of.insert(composition_id);
        right.concrete_part.righthand_of.insert(composition_id);
        let mut free_variables = hashset! {};
        let mut binding_variables = hashset! {};
        let right_is_quantifier = match &right.specific_part {
            SpecificPart::Abstract(ap) => {
                match &ap.composition {
                    MaybeComposition::Composition(cp) => {
                        free_variables.extend(&cp.free_variables);
                        binding_variables.extend(&cp.binding_variables);
                    },
                    MaybeComposition::Leaf(true) => {
                        free_variables.insert(right.id);
                    },
                    MaybeComposition::Leaf(false) => {},
                }
                false
            },
            SpecificPart::Concrete(cct) => {
                cct == &ConcreteConceptType::ExistsSuchThat
            },
            SpecificPart::String(_) => false,
        };
        if let SpecificPart::Abstract(ap) = &left.specific_part {
            match &ap.composition {
                MaybeComposition::Composition(cp) => {
                    free_variables
                        .retain(|v| !cp.binding_variables.contains(v));
                    free_variables.extend(&cp.free_variables);
                    binding_variables
                        .retain(|v| !cp.free_variables.contains(v));
                    binding_variables.extend(&cp.binding_variables);
                },
                MaybeComposition::Leaf(true) => {
                    if right_is_quantifier {
                        binding_variables.insert(left.id);
                    } else {
                        free_variables.insert(left.id);
                    }
                },
                MaybeComposition::Leaf(false) => {},
            }
        }
        Self::Abstract(AbstractPart {
            composition: MaybeComposition::Composition(CompositePart {
                lefthand: left.id,
                righthand: right.id,
                free_variables,
                binding_variables,
            }),
            ..Default::default()
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
    Leaf(bool),
}

impl MaybeComposition {
    fn composition_of(
        composition_id: usize,
        left: &mut Concept,
        right: &mut Concept,
    ) -> Self {
        left.concrete_part.lefthand_of.insert(composition_id);
        right.concrete_part.righthand_of.insert(composition_id);
        let mut free_variables = hashset! {};
        let mut binding_variables = hashset! {};
        let right_is_quantifier = match &right.specific_part {
            SpecificPart::Abstract(ap) => {
                match &ap.composition {
                    MaybeComposition::Composition(cp) => {
                        free_variables.extend(&cp.free_variables);
                        binding_variables.extend(&cp.binding_variables);
                    },
                    MaybeComposition::Leaf(true) => {
                        free_variables.insert(right.id);
                    },
                    MaybeComposition::Leaf(false) => {},
                }
                false
            },
            SpecificPart::Concrete(cct) => {
                cct == &ConcreteConceptType::ExistsSuchThat
            },
            SpecificPart::String(_) => false,
        };
        if let SpecificPart::Abstract(ap) = &left.specific_part {
            match &ap.composition {
                MaybeComposition::Composition(cp) => {
                    free_variables
                        .retain(|v| !cp.binding_variables.contains(v));
                    free_variables.extend(&cp.free_variables);
                    binding_variables
                        .retain(|v| !cp.free_variables.contains(v));
                    binding_variables.extend(&cp.binding_variables);
                },
                MaybeComposition::Leaf(true) => {
                    if right_is_quantifier {
                        binding_variables.insert(left.id);
                    } else {
                        free_variables.insert(left.id);
                    }
                },
                MaybeComposition::Leaf(false) => {},
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
            composition: MaybeComposition::Leaf(false),
            reduces_to: None,
        }
    }
}

#[derive(Clone, PartialEq, Default)]
pub struct ConcreteConcept {
    /// Set of all indices of the concepts which have this concept as the lefthand of their composition
    lefthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which have this concept as the righthand of their composition
    righthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which reduce to this concept.
    reduces_from: HashSet<usize>,
}

impl From<&NewConceptDelta> for ConcreteConcept {
    fn from(delta: &NewConceptDelta) -> Self {
        match delta {
            NewConceptDelta::Variable => Self::default(),
            NewConceptDelta::Composition(_)
            | NewConceptDelta::ReducesTo {
                ..
            }
            | NewConceptDelta::String(_) => Self::default(),
            NewConceptDelta::Double {
                composition_id,
                ..
            } => Self {
                lefthand_of: hashset! {*composition_id},
                righthand_of: hashset! {*composition_id},
                ..Default::default()
            },
            NewConceptDelta::Left {
                composition_id,
                ..
            } => Self {
                lefthand_of: hashset! {*composition_id},
                ..Default::default()
            },
            NewConceptDelta::Right {
                composition_id,
                ..
            } => Self {
                righthand_of: hashset! {*composition_id},
                ..Default::default()
            },
        }
    }
}
