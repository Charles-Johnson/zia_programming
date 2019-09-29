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
mod common_part;

pub use self::abstract_part::{AbstractDelta, AbstractPart};
pub use self::common_part::{CommonDelta, CommonPart};
use delta::{Change, SetChange, ApplyDelta, Delta};
use errors::{ZiaError, ZiaResult};
use reading::{FindWhatReducesToIt, GetDefinition, GetDefinitionOf, GetReduction, MaybeString};
use std::{collections::HashSet, iter::FromIterator};
use writing::{
    MakeReduceFrom, MakeReduceFromDelta, NoLongerReducesFrom, RemoveAsDefinitionOf,
    RemoveDefinition, RemoveDefinitionDelta, RemoveReduction, RemoveReductionDelta,
    SetAsDefinitionOf, SetAsDefinitionOfDelta, SetDefinition, SetDefinitionDelta, SetReduction,
    SetReductionDelta,
};

/// Data type for any type of concept.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Concept {
    common_part: CommonPart,
    specific_part: SpecificPart,
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
    common_part: CommonDelta,
}

impl Delta for ConceptDelta {
    fn combine(&mut self, other: ConceptDelta) {
        self.specific_part.combine(other.specific_part);
        self.common_part.combine(other.common_part);
    }
}

impl ApplyDelta for Concept {
    type Delta = ConceptDelta;
    fn apply(&mut self, delta: ConceptDelta) {
        match self.specific_part {
            SpecificPart::Abstract(ref mut ap) => ap.apply(delta.specific_part),
            _ => (),
        };
        self.common_part.apply(delta.common_part);
    }
    fn diff(&self, next: Concept) -> ConceptDelta {
        ConceptDelta{
            common_part: self.common_part.diff(next.common_part),
            specific_part: match (&self.specific_part, next.specific_part) {
                (SpecificPart::Abstract(ap1), SpecificPart::Abstract(ref ap2)) => ap1.diff(ap2.clone()),
                (SpecificPart::Abstract(ap1), _) => ap1.diff(AbstractPart::default()),
                (_, SpecificPart::Abstract(ap2)) => AbstractPart::default().diff(ap2),
                _ => AbstractDelta::default(),
            }
        }
    }
}

impl From<AbstractPart> for Concept {
    fn from(ap: AbstractPart) -> Concept {
        Concept {
            common_part: CommonPart::default(),
            specific_part: SpecificPart::Abstract(ap),
        }
    }
}

impl From<CommonPart> for Concept {
    fn from(cp: CommonPart) -> Concept {
        Concept {
            common_part: cp,
            specific_part: SpecificPart::Concrete,
        }
    }
}

impl GetDefinitionOf for Concept {
    fn get_lefthand_of(&self) -> HashSet<usize> {
        self.common_part.get_lefthand_of()
    }
    fn get_righthand_of(&self) -> HashSet<usize> {
        self.common_part.get_righthand_of()
    }
}

impl FindWhatReducesToIt for Concept {
    fn find_what_reduces_to_it(&self) -> HashSet<usize> {
        self.common_part.find_what_reduces_to_it()
    }
}

impl RemoveAsDefinitionOf for Concept {
    fn remove_as_lefthand_of(&mut self, index: usize) {
        self.common_part.remove_as_lefthand_of(index);
    }
    fn remove_as_righthand_of(&mut self, index: usize) {
        self.common_part.remove_as_righthand_of(index);
    }
}

impl NoLongerReducesFrom for Concept {
    fn no_longer_reduces_from(&mut self, index: usize) {
        self.common_part.no_longer_reduces_from(index);
    }
}

impl MakeReduceFrom for Concept {
    fn make_reduce_from(&mut self, index: usize) {
        self.common_part.make_reduce_from(index);
    }
}

impl MakeReduceFromDelta for Concept {
    fn make_reduce_from_delta(&self, index: usize) -> ConceptDelta {
        ConceptDelta {
            common_part: self.common_part.make_reduce_from_delta(index),
            specific_part: AbstractDelta::default(),
        }
    }
}

impl SetAsDefinitionOf for Concept {
    fn add_as_lefthand_of(&mut self, index: usize) {
        self.common_part.add_as_lefthand_of(index);
    }
    fn add_as_righthand_of(&mut self, index: usize) {
        self.common_part.add_as_righthand_of(index);
    }
}

impl SetAsDefinitionOfDelta for Concept {
    fn add_as_lefthand_of_delta(&self, index: usize) -> ConceptDelta {
        ConceptDelta {
            common_part: self.common_part.add_as_lefthand_of_delta(index),
            specific_part: AbstractDelta::default(),
        }
    }
    fn add_as_righthand_of_delta(&self, index: usize) -> ConceptDelta {
        ConceptDelta {
            common_part: self.common_part.add_as_righthand_of_delta(index),
            specific_part: AbstractDelta::default(),
        }
    }
}

impl GetDefinition for Concept {
    /// If concept is abstract and has a definition returns the indices of the left and right concepts that compose it as `Some((left, right))`. Otherwise returns `None`.
    fn get_definition(&self) -> Option<(usize, usize)> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => c.get_definition(),
            _ => None,
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
            SpecificPart::Abstract(ref c) => Ok(ConceptDelta {
                specific_part: c.set_definition_delta(lefthand, righthand),
                common_part: CommonDelta::default(),
            }),
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

impl GetReduction for Concept {
    /// Gets the index of the concept that `self` may reduce to.
    fn get_reduction(&self) -> Option<usize> {
        match self.specific_part {
            SpecificPart::Abstract(ref c) => c.get_reduction(),
            _ => None,
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
            SpecificPart::Abstract(ref c) => Ok(ConceptDelta {
                specific_part: c.make_reduce_to_delta(concept),
                common_part: CommonDelta::default(),
            }),
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
        Concept {
            common_part: CommonPart::default(),
            specific_part: SpecificPart::String(string),
        }
    }
}

impl MaybeString for Concept {
    /// Gets the `String` value associated with `self` if it is a string concept. Otherwise returns `None`.
    fn get_string(&self) -> Option<String> {
        match self.specific_part {
            SpecificPart::String(ref s) => Some(s.clone()),
            _ => None,
        }
    }
}

impl RemoveReductionDelta for Concept {
    fn no_longer_reduces_from_delta(&self, delta: &Self::Delta, concept: usize) -> Self::Delta {
        assert!({
            let reduces_from_concept = self.find_what_reduces_to_it().contains(&concept);
            let SetChange { add, remove } = &delta.common_part.reduces_from;
            if remove.contains(&concept) {
                false
            } else if add.contains(&concept) {
                true
            } else {
                reduces_from_concept
            }
        });
        ConceptDelta {
            common_part: CommonDelta {
                reduces_from: SetChange {
                    remove: HashSet::from_iter(std::iter::once(concept)),
                    add: HashSet::default(),
                },
                lefthand_of: SetChange::default(),
                righthand_of: SetChange::default(),
            },
            specific_part: AbstractDelta::default(),
        }
    }
    fn make_reduce_to_none_delta(&self, delta: &Self::Delta) -> Self::Delta {
        assert!({
            let reduces = self.get_reduction().is_some();
            match delta
                .specific_part
                .reduction
            {
                Change::Different { after, .. } => after.is_some(),
                Change::Same => reduces,
            }
        });
        ConceptDelta {
            specific_part: AbstractDelta {
                reduction: Change::Different {
                    before: self.get_reduction(),
                    after: None,
                },
                definition: Change::Same,
            },
            common_part: CommonDelta::default(),
        }
    }
}

impl RemoveDefinitionDelta for Concept {
    fn remove_as_lefthand_of_delta(&self, delta: &Self::Delta, concept: usize) -> Self::Delta {
        assert!({
            let lefthand_of_concept = self.get_lefthand_of().contains(&concept);
            let SetChange { add, remove } = &delta.common_part.lefthand_of;
            if remove.contains(&concept) {
                false
            } else if add.contains(&concept) {
                true
            } else {
                lefthand_of_concept
            }
        });
        ConceptDelta {
            common_part: CommonDelta {
                lefthand_of: SetChange {
                    remove: hashset!{concept},
                    add: hashset!{},
                },
                reduces_from: SetChange::default(),
                righthand_of: SetChange::default(),
            },
            specific_part: AbstractDelta::default(),
        }
    }
    fn remove_as_righthand_of_delta(&self, delta: &Self::Delta, concept: usize) -> Self::Delta {
        assert!({
            let righthand_of_concept = self.get_righthand_of().contains(&concept);
            let SetChange { add, remove } = &delta.common_part.righthand_of;
            if remove.contains(&concept) {
                false
            } else if add.contains(&concept) {
                true
            } else {
                righthand_of_concept
            }
        });
        ConceptDelta {
            common_part: CommonDelta {
                righthand_of: SetChange {
                    remove: hashset!{concept},
                    add: hashset!{},
                },
                reduces_from: SetChange::default(),
                lefthand_of: SetChange::default(),
            },
            specific_part: AbstractDelta::default(),
        }
    }
    fn remove_definition_delta(&self, delta: &Self::Delta) -> Self::Delta {
        let definition = self.get_definition();
        assert!({
            match delta.specific_part.definition {
                Change::Different { after, .. } => after.is_some(),
                _ => definition.is_some(),
            }
        });
        ConceptDelta {
            specific_part: AbstractDelta {
                definition: Change::Different {
                    before: definition,
                    after: None,
                },
                reduction: Change::Same,
            },
            common_part: CommonDelta::default(),
        }
    }
}
