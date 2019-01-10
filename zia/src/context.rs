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

use adding::{ConceptAdder, StringAdder};
use reading::ConceptReader;
use removing::{BlindConceptRemover, StringRemover};
use std::collections::HashMap;
use translating::StringConcept;
use writing::ConceptWriter;

/// A container for adding, reading, writing and removing concepts of generic type `T`.
pub struct Context<T> {
    /// Relates a String value to the index where the concept corresponding to the String is stored
    /// in the `concepts` field.
    string_map: HashMap<String, usize>,
    /// Concepts may be stored at an index of this vector as `Some(T)`. If that concept is removed
    /// from the context, `None` will be left at its index.
    concepts: Vec<Option<T>>,
    /// Keeps track of indices of the `concepts` field that have `None`.
    gaps: Vec<usize>,
}

impl<T> Default for Context<T> {
    fn default() -> Context<T> {
        Context::<T> {
            string_map: HashMap::new(),
            concepts: Vec::new(),
            gaps: Vec::new(),
        }
    }
}

impl<T> StringAdder for Context<T> {
    fn add_string(&mut self, string_id: usize, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }
}

impl<T> ConceptWriter<T> for Context<T> {
    fn write_concept(&mut self, id: usize) -> &mut T {
        match self.concepts[id] {
            Some(ref mut c) => c,
            None => panic!("No concept with id = {}", id),
        }
    }
}

impl<T> ConceptReader<T> for Context<T> {
    fn read_concept(&self, id: usize) -> &T {
        match self.concepts[id] {
            Some(ref c) => c,
            None => panic!("No concept with id = {}", id),
        }
    }
}

impl<T> BlindConceptRemover for Context<T> {
    fn blindly_remove_concept(&mut self, id: usize) {
        self.concepts[id] = None;
        self.gaps.push(id);
    }
}

impl<T> StringRemover for Context<T> {
    fn remove_string(&mut self, string: &str) {
        self.string_map.remove(string);
    }
}

impl<T> ConceptAdder<T> for Context<T> {
    fn add_concept(&mut self, concept: T) -> usize {
        match self.gaps.pop() {
            None => {
                let index = self.concepts.len();
                self.concepts.push(Some(concept));
                index
            }
            Some(index) => {
                self.concepts[index] = Some(concept);
                index
            }
        }
    }
}

impl<T> StringConcept for Context<T> {
    fn get_string_concept(&self, s: &str) -> Option<usize> {
        match self.string_map.get(s) {
            None => None,
            Some(sr) => Some(*sr),
        }
    }
}
