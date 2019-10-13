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
use writing::{
    MakeReduceFrom, MakeReduceFromDelta, NoLongerReducesFrom, RemoveDefinition, RemoveReduction,
    SetAsDefinitionOf, SetAsDefinitionOfDelta, SetDefinition, SetDefinitionDelta, SetReduction,
    SetReductionDelta,
};

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
            let lefthand_of_concept = self.get_lefthand_of().contains(&concept);
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
            let righthand_of_concept = self.get_righthand_of().contains(&concept);
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
            definition: Change::Different {
                before: definition,
                after: None,
            },
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
            let reduces_from_concept = self.find_what_reduces_to_it().contains(&concept);
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
            reduction: Change::Different {
                before: self.get_reduction(),
                after: None,
            },
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
    pub fn remove_as_lefthand_of(&mut self, index: usize) {
        self.lefthand_of.remove(&index);
    }
    pub fn remove_as_righthand_of(&mut self, index: usize) {
        self.righthand_of.remove(&index);
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
        self.lefthand_of.retain(|c| !lefthand_of.remove.contains(c));
        self.lefthand_of.extend(lefthand_of.add);
        self.righthand_of
            .retain(|c| !righthand_of.remove.contains(c));
        self.righthand_of.extend(righthand_of.add);
        self.reduces_from
            .retain(|c| !reduces_from.remove.contains(c));
        self.reduces_from.extend(reduces_from.add);
        match self.specific_part {
            SpecificPart::Abstract(ref mut ap) => ap.apply(specific_part),
            _ => (),
        };
    }
    fn diff(&self, next: Concept) -> ConceptDelta {
        let mut lefthand_of = SetChange::default();
        for next_item in &next.lefthand_of {
            if self.lefthand_of.get(&next_item).is_none() {
                lefthand_of.add.insert(*next_item);
            }
        }
        for prev_item in &self.lefthand_of {
            if next.lefthand_of.get(&prev_item).is_none() {
                lefthand_of.remove.insert(*prev_item);
            }
        }
        let mut righthand_of = SetChange::default();
        for next_item in &next.righthand_of {
            if self.righthand_of.get(&next_item).is_none() {
                righthand_of.add.insert(*next_item);
            }
        }
        for prev_item in &self.righthand_of {
            if next.righthand_of.get(&prev_item).is_none() {
                righthand_of.remove.insert(*prev_item);
            }
        }
        let mut reduces_from = SetChange::default();
        for next_item in &next.reduces_from {
            if self.reduces_from.get(&next_item).is_none() {
                reduces_from.add.insert(*next_item);
            }
        }
        for prev_item in &self.lefthand_of {
            if next.lefthand_of.get(&prev_item).is_none() {
                reduces_from.remove.insert(*prev_item);
            }
        }
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

impl NoLongerReducesFrom for Concept {
    fn no_longer_reduces_from(&mut self, index: usize) {
        self.reduces_from.remove(&index);
    }
}

impl MakeReduceFrom for Concept {
    fn make_reduce_from(&mut self, index: usize) {
        self.reduces_from.insert(index);
    }
}

impl MakeReduceFromDelta for Concept {
    fn make_reduce_from_delta(&self, index: usize) -> ConceptDelta {
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
}

impl SetAsDefinitionOf for Concept {
    fn add_as_lefthand_of(&mut self, index: usize) {
        self.lefthand_of.insert(index);
    }
    fn add_as_righthand_of(&mut self, index: usize) {
        self.righthand_of.insert(index);
    }
}

impl SetAsDefinitionOfDelta for Concept {
    fn add_as_lefthand_of_delta(&self, index: usize) -> ConceptDelta {
        ConceptDelta {
            lefthand_of: SetChange {
                add: HashSet::from_iter(std::iter::once(index)),
                remove: HashSet::default(),
            },
            righthand_of: SetChange::default(),
            reduces_from: SetChange::default(),
            specific_part: AbstractDelta::default(),
        }
    }
    fn add_as_righthand_of_delta(&self, index: usize) -> ConceptDelta {
        ConceptDelta {
            righthand_of: SetChange {
                add: HashSet::from_iter(std::iter::once(index)),
                remove: HashSet::default(),
            },
            lefthand_of: SetChange::default(),
            reduces_from: SetChange::default(),
            specific_part: AbstractDelta::default(),
        }
    }
}

impl SetDefinition for Concept {
    /// Sets the definition of the concept if abstract, otherwise returns an error.
    fn set_definition(&mut self, lefthand: usize, righthand: usize) -> ZiaResult<()> {
        match self.specific_part {
            SpecificPart::Abstract(ref mut c) => {
                c.set_definition(lefthand, righthand);
                Ok(())
            }
            _ => Err(ZiaError::SettingDefinitionOfConcrete),
        }
    }
}

impl SetDefinitionDelta for Concept {
    fn set_definition_delta(&self, lefthand: usize, righthand: usize) -> ZiaResult<ConceptDelta> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => Ok(c.set_definition_delta(lefthand, righthand).into()),
            _ => Err(ZiaError::SettingDefinitionOfConcrete),
        }
    }
}

impl RemoveDefinition for Concept {
    /// Removes the definition of the concept if abstract, otherwise panics.
    fn remove_definition(&mut self) {
        match self.specific_part {
            SpecificPart::Abstract(ref mut c) => c.remove_definition(),
            SpecificPart::String(_) => panic!("String concepts do not have a definition to remove"),
            SpecificPart::Concrete => {
                panic!("Concrete concepts do not have a definition to remove")
            }
        }
    }
}

impl SetReduction for Concept {
    /// Sets the index of the concept that `self` reduces to if abstract. Otherwise returns an error.
    fn make_reduce_to(&mut self, concept: usize) -> ZiaResult<()> {
        match self.specific_part {
            SpecificPart::Abstract(ref mut c) => {
                c.make_reduce_to(concept);
                Ok(())
            }
            _ => Err(ZiaError::ConcreteReduction),
        }
    }
}

impl SetReductionDelta for Concept {
    fn make_reduce_to_delta(&self, concept: usize) -> ZiaResult<ConceptDelta> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => Ok(c.make_reduce_to_delta(concept).into()),
            _ => Err(ZiaError::ConcreteReduction),
        }
    }
}

impl RemoveReduction for Concept {
    /// Removes the reduction rule of the concept if abstract, otherwise panics.
    fn make_reduce_to_none(&mut self) {
        match self.specific_part {
            SpecificPart::Abstract(ref mut c) => c.make_reduce_to_none(),
            SpecificPart::String(_) => panic!("String concepts have no reduction rule to remove"),
            SpecificPart::Concrete => panic!("Concrete concepts have no reduction rule to remove"),
        };
    }
}

impl From<String> for Concept {
    fn from(string: String) -> Concept {
        SpecificPart::String(string).into()
    }
}
