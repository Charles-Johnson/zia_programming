/*  Library for the Zia programming language.
    Copyright (C) 2018 to 2019  Charles Johnson

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
use std::{iter::from_fn, collections::HashSet};

pub trait ApplyDelta {
    type Delta;
    fn apply(&mut self, Self::Delta);
}
pub trait Delta {
    fn combine(&mut self, &Self);
    // Repeat mutation, f, n times on self and return vector of n results
    fn repeat<F>(&mut self, mut f: F, n: usize) -> Vec<usize>
    where
        F: for<'a> FnMut(&'a mut Self) -> usize,
    {
        let mut counter = 0;
        from_fn(|| {
            if counter < n {
                counter += 1;
                Some(f(self))
            } else {
                None
            }
        })
        .collect()
    }
    fn multiply<F>(
        &mut self,
        mut f: F,
        ns: Vec<usize>,
        ms: Vec<&str>,
    ) -> ZiaResult<()>
    where
        F: for<'a> FnMut(&'a mut Self, usize, &str) -> ZiaResult<()>,
    {
        ns.iter().zip(ms).try_for_each(|(n, m)| f(self, *n, m))
    }
}

#[derive(Clone, Debug)]
pub enum Change<T> {
    Same,
    Different { before: T, after: T },
}

impl<T> Default for Change<T> {
    fn default() -> Self {
        Change::Same
    }
}

impl<T> Delta for Change<T>
where
    T: PartialEq,
{
    fn combine(&mut self, other: &Change<T>) {
        match (self, other) {
            (Change::Same, Change::Same) => (),
            (Change::Same, _) => *self = *other,
            (_, Change::Same) => (),
            (Change::Different{before: x, after: y1}, Change::Different{before: y2, after: z}) if y1 == y2 => *self = Change::Different{before: *x, after: *z},
            _ => panic!("Deltas do not align"),
        };
    }
}

#[derive(Clone, Debug, Default)]
pub struct CollectionChange {
    pub remove: HashSet<usize>,
    pub add: HashSet<usize>,
}

impl Delta for CollectionChange {
    fn combine(&mut self, other: &CollectionChange) {
        other.remove.iter().for_each(|item| if self.add.contains(item) {
            self.add.remove(item);
        } else {
            self.remove.insert(*item);
        });
        other.add.iter().for_each(|item| if self.remove.contains(item) {
            self.remove.remove(item);
        } else {
            self.add.insert(*item);
        });
    }
}
