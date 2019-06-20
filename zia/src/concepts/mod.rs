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
use delta::Delta;
use errors::{ZiaError, ZiaResult};
use reading::{FindWhatReducesToIt, GetDefinition, GetDefinitionOf, GetReduction, MaybeString};
use std::collections::HashSet;
use writing::{
    MakeReduceFrom, NoLongerReducesFrom, RemoveAsDefinitionOf, RemoveDefinition, RemoveReduction,
    SetAsDefinitionOf, SetAsDefinitionOfDelta, SetDefinition, SetDefinitionDelta, SetReduction,
};

/// Data type for any type of concept.
#[derive(Clone, Debug)]
pub struct Concept {
    common_part: CommonPart,
    specific_part: SpecificPart,
}

#[derive(Clone, Debug)]
enum SpecificPart {
    /// A concrete concept cannot be further reduced or defined as a composition.
    Concrete,
    /// An abstract concept can reduce to any other concept (whose normal form isn't the former
    /// concept) and can be defined as the composition of any two concepts.
    Abstract(AbstractPart),
    /// A string concept is associated with a `String` value by the `MaybeString` trait.
    String(String),
}

#[derive(Clone)]
pub enum ConceptDelta {
    Abstract(AbstractDelta),
    Common(CommonDelta),
}

impl Delta for Concept {
    type Delta = ConceptDelta;
    fn apply(&mut self, delta: &ConceptDelta) {
        match delta {
            ConceptDelta::Abstract(ad) => match self.specific_part {
                SpecificPart::Abstract(ref mut ap) => ap.apply(ad),
                _ => panic!("AbstractDelta applied to concrete concept."),
            },
            ConceptDelta::Common(cd) => self.common_part.apply(cd),
        };
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
        ConceptDelta::Common(self.common_part.add_as_lefthand_of_delta(index))
    }
    fn add_as_righthand_of_delta(&self, index: usize) -> ConceptDelta {
        ConceptDelta::Common(self.common_part.add_as_righthand_of_delta(index))
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
            SpecificPart::Abstract(ref c) => Ok(ConceptDelta::Abstract(
                c.set_definition_delta(lefthand, righthand),
            )),
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
