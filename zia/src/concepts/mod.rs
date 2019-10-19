/*  Library for the Zia programming language.
    Copyright (C) 2018 to 2019 Charles Johnson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
*/
mod abstract_part;

pub use self::abstract_part::{AbstractDelta, AbstractPart};
use delta::{ApplyDelta, Change, Delta, SetChange};
use errors::{ZiaError, ZiaResult};
use std::{collections::HashSet, iter::FromIterator};

/// Data type for any type of concept.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Concept {
    /// Set of all indices of the concepts which have this concept as the lefthand of their definition
    lefthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which have this concept as the righthand of their definition
    righthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which reduce to this concept.
    reduces_from: HashSet<usize>,
    specific_part: SpecificPart,
}

impl Concept {
    pub fn remove_as_lefthand_of_delta(
        &self,
        delta: &ConceptDelta,
        concept: usize,
    ) -> ConceptDelta {
        assert!({
            let lefthand_of_concept = self.lefthand_of.contains(&concept);
            let SetChange { add, remove } = &delta.lefthand_of;
            if remove.contains(&concept) {
                false
            } else if add.contains(&concept) {
                true
            } else {
                lefthand_of_concept
            }
        });
        ConceptDelta {
            lefthand_of: SetChange {
                remove: hashset! {concept},
                add: hashset! {},
            },
            reduces_from: SetChange::default(),
            righthand_of: SetChange::default(),
            specific_part: AbstractDelta::default(),
        }
    }
    pub fn remove_as_righthand_of_delta(
        &self,
        delta: &ConceptDelta,
        concept: usize,
    ) -> ConceptDelta {
        assert!({
            let righthand_of_concept = self.righthand_of.contains(&concept);
            let SetChange { add, remove } = &delta.righthand_of;
            if remove.contains(&concept) {
                false
            } else if add.contains(&concept) {
                true
            } else {
                righthand_of_concept
            }
        });
        ConceptDelta {
            righthand_of: SetChange {
                remove: hashset! {concept},
                add: hashset! {},
            },
            reduces_from: SetChange::default(),
            lefthand_of: SetChange::default(),
            specific_part: AbstractDelta::default(),
        }
    }
    pub fn remove_definition_delta(&self, delta: &ConceptDelta) -> ConceptDelta {
        let definition = self.get_definition();
        assert!({
            match delta.specific_part.definition {
                Change::Different { after, .. } => after.is_some(),
                _ => definition.is_some(),
            }
        });
        AbstractDelta {
            definition: definition.diff(None),
            reduction: Change::Same,
        }
        .into()
    }
    pub fn no_longer_reduces_from_delta(
        &self,
        delta: &ConceptDelta,
        concept: usize,
    ) -> ConceptDelta {
        assert!({
            let reduces_from_concept = self.reduces_from.contains(&concept);
            let SetChange { add, remove } = &delta.reduces_from;
            if remove.contains(&concept) {
                false
            } else if add.contains(&concept) {
                true
            } else {
                reduces_from_concept
            }
        });
        ConceptDelta {
            reduces_from: SetChange {
                remove: HashSet::from_iter(std::iter::once(concept)),
                add: HashSet::default(),
            },
            lefthand_of: SetChange::default(),
            righthand_of: SetChange::default(),
            specific_part: AbstractDelta::default(),
        }
    }
    pub fn make_reduce_to_none_delta(&self, delta: &ConceptDelta) -> ConceptDelta {
        assert!({
            let reduces = self.get_reduction().is_some();
            match delta.specific_part.reduction {
                Change::Different { after, .. } => after.is_some(),
                Change::Same => reduces,
            }
        });
        AbstractDelta {
            reduction: self.get_reduction().diff(None),
            definition: Change::Same,
        }
        .into()
    }
    pub fn find_what_reduces_to_it(&self) -> &HashSet<usize> {
        &self.reduces_from
    }
    /// Gets the `String` value associated with `self` if it is a string concept. Otherwise returns `None`.
    pub fn get_string(&self) -> Option<String> {
        match self.specific_part {
            SpecificPart::String(ref s) => Some(s.clone()),
            _ => None,
        }
    }
    pub fn get_lefthand_of(&self) -> &HashSet<usize> {
        &self.lefthand_of
    }
    pub fn get_righthand_of(&self) -> &HashSet<usize> {
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
    pub fn make_reduce_from_delta(&self, index: usize) -> ConceptDelta {
        ConceptDelta {
            reduces_from: SetChange {
                add: HashSet::from_iter(std::iter::once(index)),
                remove: HashSet::default(),
            },
            lefthand_of: SetChange::default(),
            righthand_of: SetChange::default(),
            specific_part: AbstractDelta::default(),
        }
    }
    pub fn make_reduce_to_delta(&self, concept: usize) -> ZiaResult<ConceptDelta> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => Ok(c.make_reduce_to_delta(concept).into()),
            _ => Err(ZiaError::ConcreteReduction),
        }
    }
    pub fn set_definition(&self, d: usize, l: usize, r: usize) -> ZiaResult<[ConceptDelta; 3]> {
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

#[derive(Clone, Debug, PartialEq)]
enum SpecificPart {
    /// A concrete concept cannot be further reduced or defined as a composition.
    Concrete,
    /// An abstract concept can reduce to any other concept (whose normal form isn't the former
    /// concept) and can be defined as the composition of any two concepts.
    Abstract(AbstractPart),
    /// A string concept is associated with a `String` value by the `MaybeString` trait.
    String(String),
}

impl Default for SpecificPart {
    fn default() -> SpecificPart {
        SpecificPart::Concrete
    }
}

#[derive(Clone, Debug, Default)]
pub struct ConceptDelta {
    specific_part: AbstractDelta,
    lefthand_of: SetChange,
    righthand_of: SetChange,
    reduces_from: SetChange,
}

impl Delta for ConceptDelta {
    fn combine(&mut self, other: ConceptDelta) {
        self.specific_part.combine(other.specific_part);
        self.lefthand_of.combine(other.lefthand_of);
        self.righthand_of.combine(other.righthand_of);
        self.reduces_from.combine(other.reduces_from);
    }
}

impl ApplyDelta for Concept {
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
        match self.specific_part {
            SpecificPart::Abstract(ref mut ap) => ap.apply(specific_part),
            _ => (),
        };
    }
    fn diff(&self, next: Concept) -> ConceptDelta {
        let lefthand_of = self.lefthand_of.diff(next.lefthand_of);
        let righthand_of = self.righthand_of.diff(next.righthand_of);
        let reduces_from = self.reduces_from.diff(next.reduces_from);
        ConceptDelta {
            lefthand_of,
            righthand_of,
            reduces_from,
            specific_part: match (&self.specific_part, next.specific_part) {
                (SpecificPart::Abstract(ap1), SpecificPart::Abstract(ref ap2)) => {
                    ap1.diff(ap2.clone())
                }
                (SpecificPart::Abstract(ap1), _) => ap1.diff(AbstractPart::default()),
                (_, SpecificPart::Abstract(ap2)) => AbstractPart::default().diff(ap2),
                _ => AbstractDelta::default(),
            },
        }
    }
}

impl From<AbstractPart> for Concept {
    fn from(ap: AbstractPart) -> Concept {
        SpecificPart::Abstract(ap).into()
    }
}

impl From<SpecificPart> for Concept {
    fn from(sp: SpecificPart) -> Concept {
        Concept {
            lefthand_of: HashSet::default(),
            righthand_of: HashSet::default(),
            reduces_from: HashSet::default(),
            specific_part: sp,
        }
    }
}

impl From<AbstractDelta> for ConceptDelta {
    fn from(ap: AbstractDelta) -> ConceptDelta {
        ConceptDelta {
            lefthand_of: SetChange::default(),
            righthand_of: SetChange::default(),
            reduces_from: SetChange::default(),
            specific_part: ap,
        }
    }
}

impl From<String> for Concept {
    fn from(string: String) -> Concept {
        SpecificPart::String(string).into()
    }
}
