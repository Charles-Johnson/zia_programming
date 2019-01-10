/*  Library for the Zia programming language.
    Copyright (C) 2018  Charles Johnson

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

use errors::ZiaResult;
use reading::{GetDefinition, GetReduction};
use writing::{RemoveDefinition, RemoveReduction, SetDefinition, SetReduction};

/// An abstract concept can reduce to other concepts and be defined as a composition of two other concepts.
pub struct AbstractPart {
    /// The concept may be defined as a composition of two other concepts.
    definition: Option<(usize, usize)>,
    /// The concept may reduce to another concept.
    reduces_to: Option<usize>,
}

impl Default for AbstractPart {
    /// The default concept doesn't have a definition and doesn't further reduce.
    fn default() -> AbstractPart {
        AbstractPart {
            definition: None,
            reduces_to: None,
        }
    }
}

impl GetDefinition for AbstractPart {
    fn get_definition(&self) -> Option<(usize, usize)> {
        self.definition
    }
}

impl SetDefinition for AbstractPart {
    fn set_definition(&mut self, lefthand: usize, righthand: usize) -> ZiaResult<()> {
        self.definition = Some((lefthand, righthand));
        Ok(())
    }
}

impl RemoveDefinition for AbstractPart {
    fn remove_definition(&mut self) {
        self.definition = None
    }
}

impl GetReduction for AbstractPart {
    fn get_reduction(&self) -> Option<usize> {
        self.reduces_to
    }
}

impl SetReduction for AbstractPart {
    fn make_reduce_to(&mut self, concept: usize) -> ZiaResult<()> {
        self.reduces_to = Some(concept);
        Ok(())
    }
}

impl RemoveReduction for AbstractPart {
    fn make_reduce_to_none(&mut self) {
        self.reduces_to = None;
    }
}
