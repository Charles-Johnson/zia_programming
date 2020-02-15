//  Library for the Zia programming language.
// Copyright (C) 2018 to 2019 Charles Johnson
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
mod abstract_part;

pub use self::abstract_part::{AbstractDelta, AbstractPart};
use crate::delta::{Apply, Change, Delta, SetChange};
use crate::errors::{ZiaError, ZiaResult};
use maplit::hashset;
use std::{collections::HashSet, fmt::Debug};

/// Data type for any type of concept.
#[derive(Clone, Default, PartialEq)]
pub struct Concept {
    /// Set of all indices of the concepts which have this concept as the lefthand of their definition
    lefthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which have this concept as the righthand of their definition
    righthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which reduce to this concept.
    reduces_from: HashSet<usize>,
    specific_part: SpecificPart,
}

impl Debug for Concept {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let mut string = "{".to_string();
        if !self.lefthand_of.is_empty() {
            string += " lefthand_of: {";
            let mut unorder_keys: Vec<&usize> =
                self.lefthand_of.iter().collect();
            unorder_keys.sort();
            for key in unorder_keys {
                string += &format!("{},", key);
            }
            string += "},";
        }
        if !self.righthand_of.is_empty() {
            string += " righthand_of: {";
            let mut unorder_keys: Vec<&usize> =
                self.righthand_of.iter().collect();
            unorder_keys.sort();
            for key in unorder_keys {
                string += &format!("{},", key);
            }
            string += "},";
        }
        if !self.reduces_from.is_empty() {
            string += " reduces_from: {";
            let mut unorder_keys: Vec<&usize> =
                self.reduces_from.iter().collect();
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

impl Concept {
    pub fn delete_definition(&self, id: usize) -> [ConceptDelta; 3] {
        [
            AbstractDelta {
                definition: self.get_definition().diff(None),
                reduction: Change::Same,
            }
            .into(),
            ConceptDelta {
                lefthand_of: SetChange {
                    remove: hashset! {id},
                    add: hashset! {},
                },
                reduces_from: SetChange::default(),
                righthand_of: SetChange::default(),
                specific_part: AbstractDelta::default(),
            },
            ConceptDelta {
                righthand_of: SetChange {
                    remove: hashset! {id},
                    add: hashset! {},
                },
                reduces_from: SetChange::default(),
                lefthand_of: SetChange::default(),
                specific_part: AbstractDelta::default(),
            },
        ]
    }

    pub fn remove_reduction(
        &self,
        id: usize,
    ) -> ZiaResult<[(usize, ConceptDelta); 2]> {
        self.get_reduction()
            .map(|reduction| {
                [
                    (
                        id,
                        AbstractDelta {
                            reduction: self.get_reduction().diff(None),
                            definition: Change::Same,
                        }
                        .into(),
                    ),
                    (
                        reduction,
                        ConceptDelta {
                            reduces_from: SetChange {
                                remove: hashset! {id},
                                add: hashset! {},
                            },
                            lefthand_of: SetChange::default(),
                            righthand_of: SetChange::default(),
                            specific_part: AbstractDelta::default(),
                        },
                    ),
                ]
            })
            .ok_or(ZiaError::RedundantReduction)
    }

    pub fn find_what_reduces_to_it(
        &self,
    ) -> std::collections::hash_set::Iter<usize> {
        self.reduces_from.iter()
    }

    /// Gets the `String` value associated with `self` if it is a string concept. Otherwise returns `None`.
    pub fn get_string(&self) -> Option<String> {
        match self.specific_part {
            SpecificPart::String(ref s) => Some(s.clone()),
            _ => None,
        }
    }

    pub const fn get_lefthand_of(&self) -> &HashSet<usize> {
        &self.lefthand_of
    }

    pub const fn get_righthand_of(&self) -> &HashSet<usize> {
        &self.righthand_of
    }

    /// Gets the index of the concept that `self` may reduce to.
    pub fn get_reduction(&self) -> Option<usize> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => c.reduces_to,
            _ => None,
        }
    }

    /// If concept is abstract and has a definition returns the indices of the left and right concepts that compose it as `Some((left, right))`. Otherwise returns `None`.
    pub fn get_definition(&self) -> Option<(usize, usize)> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => c.definition,
            _ => None,
        }
    }

    pub fn reduce_to(
        &self,
        id: usize,
        reduction: usize,
    ) -> ZiaResult<[ConceptDelta; 2]> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => Ok([
                c.make_reduce_to_delta(reduction).into(),
                ConceptDelta {
                    reduces_from: SetChange {
                        add: hashset! {id},
                        remove: hashset! {},
                    },
                    lefthand_of: SetChange::default(),
                    righthand_of: SetChange::default(),
                    specific_part: AbstractDelta::default(),
                },
            ]),
            _ => Err(ZiaError::ConcreteReduction),
        }
    }

    pub fn set_definition(
        &self,
        d: usize,
        l: usize,
        r: usize,
    ) -> ZiaResult<[ConceptDelta; 3]> {
        match &self.specific_part {
            SpecificPart::Abstract(c) => Ok([
                c.set_definition_delta(l, r).into(),
                ConceptDelta {
                    lefthand_of: SetChange {
                        add: hashset! {d},
                        remove: hashset! {},
                    },
                    righthand_of: SetChange::default(),
                    reduces_from: SetChange::default(),
                    specific_part: AbstractDelta::default(),
                },
                ConceptDelta {
                    lefthand_of: SetChange::default(),
                    righthand_of: SetChange {
                        add: hashset! {d},
                        remove: hashset! {},
                    },
                    reduces_from: SetChange::default(),
                    specific_part: AbstractDelta::default(),
                },
            ]),
            _ => Err(ZiaError::SettingDefinitionOfConcrete),
        }
    }
}

#[derive(Clone, PartialEq)]
enum SpecificPart {
    /// A concrete concept cannot be further reduced or defined as a composition.
    Concrete,
    /// An abstract concept can reduce to any other concept (whose normal form isn't the former
    /// concept) and can be defined as the composition of any two concepts.
    Abstract(AbstractPart),
    /// A string concept is associated with a `String` value by the `MaybeString` trait.
    String(String),
}

impl Debug for SpecificPart {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str(&match *self {
            Self::Concrete => "Concrete".to_string(),
            Self::Abstract(ref ap) => format!("{:#?}", ap),
            Self::String(ref s) => format_string(s),
        })
    }
}

pub fn format_string(s: &str) -> String {
    format!("\"{}\"", s)
}

impl Default for SpecificPart {
    fn default() -> Self {
        Self::Concrete
    }
}

#[derive(Clone, Default)]
pub struct ConceptDelta {
    specific_part: AbstractDelta,
    lefthand_of: SetChange,
    righthand_of: SetChange,
    reduces_from: SetChange,
}

impl std::fmt::Debug for ConceptDelta {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let mut string = "{".to_string();
        if let Change::Different {
            before,
            after,
        } = self.specific_part.definition
        {
            string += &format!(" definition: {:?} -> {:?},", before, after);
        }
        if let Change::Different {
            before,
            after,
        } = self.specific_part.reduction
        {
            string += &format!(" reduction: {:?} -> {:?},", before, after);
        }
        if !self.lefthand_of.is_same() {
            string += &format!(" lefthand_of: {:#?},", self.lefthand_of);
        }
        if !self.righthand_of.is_same() {
            string += &format!(" righthand_of: {:#?},", self.righthand_of);
        }
        if !self.reduces_from.is_same() {
            string += &format!(" reduces_from: {:#?},", self.reduces_from);
        }
        formatter.write_str(&(string + "}"))
    }
}

impl Delta for ConceptDelta {
    fn combine(&mut self, other: Self) {
        self.specific_part.combine(other.specific_part);
        self.lefthand_of.combine(other.lefthand_of);
        self.righthand_of.combine(other.righthand_of);
        self.reduces_from.combine(other.reduces_from);
    }
}

impl Apply for Concept {
    type Delta = ConceptDelta;

    fn apply(&mut self, delta: ConceptDelta) {
        let ConceptDelta {
            lefthand_of,
            righthand_of,
            reduces_from,
            specific_part,
        } = delta;
        self.lefthand_of.apply(lefthand_of);
        self.righthand_of.apply(righthand_of);
        self.reduces_from.apply(reduces_from);
        if let SpecificPart::Abstract(ref mut ap) = self.specific_part {
            ap.apply(specific_part);
        };
    }

    fn diff(&self, next: Self) -> ConceptDelta {
        let lefthand_of = self.lefthand_of.diff(next.lefthand_of);
        let righthand_of = self.righthand_of.diff(next.righthand_of);
        let reduces_from = self.reduces_from.diff(next.reduces_from);
        ConceptDelta {
            lefthand_of,
            righthand_of,
            reduces_from,
            specific_part: match (&self.specific_part, next.specific_part) {
                (
                    SpecificPart::Abstract(ap1),
                    SpecificPart::Abstract(ref ap2),
                ) => ap1.diff(ap2.clone()),
                (SpecificPart::Abstract(ap1), _) => {
                    ap1.diff(AbstractPart::default())
                },
                (_, SpecificPart::Abstract(ap2)) => {
                    AbstractPart::default().diff(ap2)
                },
                _ => AbstractDelta::default(),
            },
        }
    }
}

impl From<AbstractPart> for Concept {
    fn from(ap: AbstractPart) -> Self {
        SpecificPart::Abstract(ap).into()
    }
}

impl From<SpecificPart> for Concept {
    fn from(sp: SpecificPart) -> Self {
        Self {
            lefthand_of: HashSet::default(),
            righthand_of: HashSet::default(),
            reduces_from: HashSet::default(),
            specific_part: sp,
        }
    }
}

impl From<AbstractDelta> for ConceptDelta {
    fn from(ap: AbstractDelta) -> Self {
        Self {
            lefthand_of: SetChange::default(),
            righthand_of: SetChange::default(),
            reduces_from: SetChange::default(),
            specific_part: ap,
        }
    }
}

impl From<String> for Concept {
    fn from(string: String) -> Self {
        SpecificPart::String(string).into()
    }
}
