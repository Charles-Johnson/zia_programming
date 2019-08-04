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
use delta::Delta;
use reading::{FindWhatReducesToIt, GetDefinitionOf};
use std::collections::HashSet;
use writing::{
    MakeReduceFrom, MakeReduceFromDelta, NoLongerReducesFrom, RemoveAsDefinitionOf,
    SetAsDefinitionOf, SetAsDefinitionOfDelta,
};

#[derive(Default, Clone, Debug)]
pub struct CommonPart {
    /// Set of all indices of the concepts which have this concept as the lefthand of their definition
    lefthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which have this concept as the righthand of their definition
    righthand_of: HashSet<usize>,
    /// Set of all indices of the concepts which reduce to this concept.
    reduces_from: HashSet<usize>,
}

impl Delta for CommonPart {
    type Delta = CommonDelta;
    fn apply(&mut self, delta: &CommonDelta) {
        match delta {
            CommonDelta::AddLeft(left) => self.add_as_lefthand_of(*left),
            CommonDelta::AddRight(right) => self.add_as_righthand_of(*right),
            CommonDelta::RemoveLeft(left) => self.remove_as_lefthand_of(*left),
            CommonDelta::RemoveRight(right) => self.remove_as_righthand_of(*right),
            CommonDelta::AddReducesFrom(concept) => self.make_reduce_from(*concept),
            CommonDelta::RemoveReducesFrom(concept) => self.no_longer_reduces_from(*concept),
        };
    }
}

#[derive(Clone, Debug)]
pub enum CommonDelta {
    AddLeft(usize),
    AddRight(usize),
    RemoveLeft(usize),
    RemoveRight(usize),
    AddReducesFrom(usize),
    RemoveReducesFrom(usize),
}

impl GetDefinitionOf for CommonPart {
    fn get_lefthand_of(&self) -> HashSet<usize> {
        self.lefthand_of.clone()
    }
    fn get_righthand_of(&self) -> HashSet<usize> {
        self.righthand_of.clone()
    }
}

impl FindWhatReducesToIt for CommonPart {
    fn find_what_reduces_to_it(&self) -> HashSet<usize> {
        self.reduces_from.clone()
    }
}

impl SetAsDefinitionOf for CommonPart {
    fn add_as_lefthand_of(&mut self, index: usize) {
        self.lefthand_of.insert(index);
    }
    fn add_as_righthand_of(&mut self, index: usize) {
        self.righthand_of.insert(index);
    }
}

impl SetAsDefinitionOfDelta for CommonPart {
    fn add_as_lefthand_of_delta(&self, index: usize) -> CommonDelta {
        CommonDelta::AddLeft(index)
    }
    fn add_as_righthand_of_delta(&self, index: usize) -> CommonDelta {
        CommonDelta::AddRight(index)
    }
}

impl MakeReduceFrom for CommonPart {
    fn make_reduce_from(&mut self, index: usize) {
        self.reduces_from.insert(index);
    }
}

impl MakeReduceFromDelta for CommonPart {
    fn make_reduce_from_delta(&self, index: usize) -> CommonDelta {
        CommonDelta::AddReducesFrom(index)
    }
}

impl RemoveAsDefinitionOf for CommonPart {
    fn remove_as_lefthand_of(&mut self, index: usize) {
        self.lefthand_of.remove(&index);
    }
    fn remove_as_righthand_of(&mut self, index: usize) {
        self.righthand_of.remove(&index);
    }
}

impl NoLongerReducesFrom for CommonPart {
    fn no_longer_reduces_from(&mut self, index: usize) {
        self.reduces_from.remove(&index);
    }
}
