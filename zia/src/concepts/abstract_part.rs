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

use delta::{ApplyDelta, Change, Delta};
use std::fmt::Debug;

/// An abstract concept can reduce to other concepts and be defined as a composition of two other concepts.
#[derive(Clone, PartialEq)]
pub struct AbstractPart {
    /// The concept may be defined as a composition of two other concepts.
    pub definition: Option<(usize, usize)>,
    /// The concept may reduce to another concept.
    pub reduces_to: Option<usize>,
}

impl Debug for AbstractPart {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        formatter.write_str("{")?;
        self.definition
            .iter()
            .try_for_each(|(l, r)| formatter.write_str(&format!("definition: {}, {},", l, r)))?;
        self.reduces_to
            .iter()
            .try_for_each(|r| formatter.write_str(&format!("reduces_to: {},", r)))?;
        formatter.write_str("}")
    }
}

impl ApplyDelta for AbstractPart {
    type Delta = AbstractDelta;
    fn apply(&mut self, delta: AbstractDelta) {
        self.definition.apply(delta.definition);
        self.reduces_to.apply(delta.reduction);
    }
    fn diff(&self, next: AbstractPart) -> AbstractDelta {
        AbstractDelta {
            definition: self.definition.diff(next.definition),
            reduction: self.reduces_to.diff(next.reduces_to),
        }
    }
}

impl Delta for AbstractDelta {
    fn combine(&mut self, other: AbstractDelta) {
        self.definition = self.definition.clone().combine(other.definition);
        self.reduction = self.reduction.clone().combine(other.reduction);
    }
}

#[derive(Clone, Debug, Default)]
pub struct AbstractDelta {
    pub definition: Change<Option<(usize, usize)>>,
    pub reduction: Change<Option<usize>>,
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

impl AbstractPart {
    pub fn set_definition_delta(&self, lefthand: usize, righthand: usize) -> AbstractDelta {
        AbstractDelta {
            definition: self.definition.diff(Some((lefthand, righthand))),
            reduction: Change::Same,
        }
    }
    pub fn make_reduce_to_delta(&self, concept: usize) -> AbstractDelta {
        AbstractDelta {
            definition: Change::Same,
            reduction: self.reduces_to.diff(Some(concept)),
        }
    }
}
