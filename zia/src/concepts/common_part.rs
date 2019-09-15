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
use delta::{CollectionChange, Delta};
use reading::{FindWhatReducesToIt, GetDefinitionOf};
use std::{collections::HashSet, iter::FromIterator};
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
    fn apply(&mut self, delta: CommonDelta) {
        let CommonDelta{lefthand_of, righthand_of, reduces_from} = delta;
        self.lefthand_of
            .retain(|c| !lefthand_of.remove.contains(c));
        self.lefthand_of.extend(lefthand_of.add);
        self.righthand_of
            .retain(|c| !righthand_of.remove.contains(c));
        self.righthand_of.extend(righthand_of.add);
        self.reduces_from
            .retain(|c| !reduces_from.remove.contains(c));
        self.reduces_from.extend(reduces_from.add);
    }
}

#[derive(Clone, Debug, Default)]
pub struct CommonDelta {
    pub lefthand_of: CollectionChange<HashSet<usize>>,
    pub righthand_of: CollectionChange<HashSet<usize>>,
    pub reduces_from: CollectionChange<HashSet<usize>>,
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
        CommonDelta {
            lefthand_of: CollectionChange {
                add: HashSet::from_iter(std::iter::once(index)),
                remove: HashSet::default(),
            },
            righthand_of: CollectionChange::default(),
            reduces_from: CollectionChange::default(),
        }
    }
    fn add_as_righthand_of_delta(&self, index: usize) -> CommonDelta {
        CommonDelta {
            righthand_of: CollectionChange {
                add: HashSet::from_iter(std::iter::once(index)),
                remove: HashSet::default(),
            },
            lefthand_of: CollectionChange::default(),
            reduces_from: CollectionChange::default(),
        }
    }
}

impl MakeReduceFrom for CommonPart {
    fn make_reduce_from(&mut self, index: usize) {
        self.reduces_from.insert(index);
    }
}

impl MakeReduceFromDelta for CommonPart {
    fn make_reduce_from_delta(&self, index: usize) -> CommonDelta {
        CommonDelta {
            reduces_from: CollectionChange {
                add: HashSet::from_iter(std::iter::once(index)),
                remove: HashSet::default(),
            },
            lefthand_of: CollectionChange::default(),
            righthand_of: CollectionChange::default(),
        }
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
