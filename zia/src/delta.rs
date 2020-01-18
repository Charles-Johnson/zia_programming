//  Library for the Zia programming language.
// Copyright (C) 2018 to 2019  Charles Johnson
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

use std::collections::HashSet;

pub trait Apply {
    type Delta;
    fn apply(&mut self, Self::Delta);
    fn diff(&self, Self) -> Self::Delta;
}

impl<T> Apply for Option<T>
where
    T: PartialEq + Clone,
{
    type Delta = Change<Self>;

    fn apply(&mut self, delta: Self::Delta) {
        if let Change::Different {
            after,
            ..
        } = delta
        {
            *self = after;
        }
    }

    fn diff(&self, next: Self) -> Change<Self> {
        if self == &next {
            Change::Same
        } else {
            Change::Different {
                before: self.as_ref().cloned(),
                after: next,
            }
        }
    }
}
pub trait Delta {
    fn combine(&mut self, Self);
}

#[derive(Clone, Debug)]
pub enum Change<T> {
    Same,
    Different {
        before: T,
        after: T,
    },
}

impl<T> Default for Change<T> {
    fn default() -> Self {
        Self::Same
    }
}

impl<T> Change<T>
where
    T: PartialEq,
{
    pub fn combine(self, other: Self) -> Self {
        match (self, other) {
            (Self::Same, x) | (x, Self::Same) => x,
            (
                Self::Different {
                    after: y1,
                    before: x,
                },
                Self::Different {
                    before: y2,
                    after: z,
                },
            ) => {
                if y1 == y2 {
                    Self::Different {
                        before: x,
                        after: z,
                    }
                } else {
                    panic!("Deltas do not align")
                }
            },
        }
    }
}

#[derive(Clone, Default)]
pub struct SetChange {
    pub remove: HashSet<usize>,
    pub add: HashSet<usize>,
}

impl SetChange {
    pub fn is_same(&self) -> bool {
        self.remove.is_empty() && self.add.is_empty()
    }
}

impl std::fmt::Debug for SetChange {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let mut string = "{".to_string();
        if !self.remove.is_empty() {
            string += " remove:";
            let mut indices: Vec<&usize> = self.remove.iter().collect();
            indices.sort();
            for index in indices {
                string += &format!(" {},", index);
            }
        }
        if !self.add.is_empty() {
            string += " add:";
            let mut indices: Vec<&usize> = self.add.iter().collect();
            indices.sort();
            for index in indices {
                string += &format!(" {},", index);
            }
        }
        formatter.write_str(&(string + "}"))
    }
}

impl Delta for SetChange {
    fn combine(&mut self, other: Self) {
        other.remove.iter().for_each(|item| {
            if self.add.contains(item) {
                self.add.remove(item);
            } else {
                self.remove.insert(*item);
            }
        });
        other.add.iter().for_each(|item| {
            if self.remove.contains(item) {
                self.remove.remove(item);
            } else {
                self.add.insert(*item);
            }
        });
    }
}

impl Apply for HashSet<usize> {
    type Delta = SetChange;

    fn apply(&mut self, delta: SetChange) {
        self.retain(|c| !delta.remove.contains(c));
        self.extend(delta.add);
    }

    fn diff(&self, next: Self) -> SetChange {
        let mut set_change = SetChange::default();
        for next_item in &next {
            if self.get(next_item).is_none() {
                set_change.add.insert(*next_item);
            }
        }
        for prev_item in self {
            if next.get(prev_item).is_none() {
                set_change.remove.insert(*prev_item);
            }
        }
        set_change
    }
}
