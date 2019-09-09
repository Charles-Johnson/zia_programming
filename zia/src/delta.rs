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
use std::iter::from_fn;

pub trait Delta {
    type Delta;
    fn apply(&mut self, Self::Delta);
    fn apply_all(&mut self, deltas: Vec<Self::Delta>) {
        for delta in deltas {
            self.apply(delta);
        }
    }
    // Repeat mutation, f, n times on self and return vector of n results
    fn repeat<F>(deltas: &mut Vec<Self::Delta>, mut f: F, n: usize) -> Vec<usize>
    where
        F: for<'a> FnMut(&'a mut Vec<Self::Delta>) -> usize,
    {
        let mut counter = 0;
        from_fn(|| {
            if counter < n {
                counter += 1;
                Some(f(deltas))
            } else {
                None
            }
        })
        .collect()
    }
    fn multiply<F>(
        deltas: &mut Vec<Self::Delta>,
        mut f: F,
        ns: Vec<usize>,
        ms: Vec<&str>,
    ) -> ZiaResult<()>
    where
        F: for<'a> FnMut(&'a mut Vec<Self::Delta>, usize, &str) -> ZiaResult<()>,
    {
        ns.iter().zip(ms).try_for_each(|(n, m)| f(deltas, *n, m))
    }
}
