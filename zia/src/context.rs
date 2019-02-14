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

use adding::{ConceptAdder, StringAdder, StringAdderDelta, ConceptAdderDelta};
use delta::Delta;
use errors::ZiaResult;
use reading::ConceptReader;
use removing::{BlindConceptRemover, StringRemover};
use std::collections::{HashMap, HashSet};
use translating::StringConcept;
use writing::{ConceptWriter, SetConceptDefinitionDelta, SetDefinitionDelta};

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

pub enum ContextDelta<T> 
where
	T: Delta,
{
	String(String, StringDelta),
	Concept(usize, ConceptDelta<T>)
}

pub enum StringDelta {
	Insert(usize), 
	Remove,
}

pub enum ConceptDelta<T> 
where
	T: Delta,
{
	Insert(T),
	Remove,
	Update(T::Delta),
}

impl<T> Delta for Context<T> 
where
	T: Delta,
{
	type Delta = ContextDelta<T>;
	fn apply(&mut self, delta: ContextDelta<T>) {
		match delta {
			ContextDelta::<T>::String(s, sd) => match sd {
				StringDelta::Insert(id) => self.add_string(id, &s),
				StringDelta::Remove => self.remove_string(&s),
			},
			ContextDelta::<T>::Concept(id, cd) => match cd {
				ConceptDelta::<T>::Insert(c) => {
					self.add_concept(c);
				},
				ConceptDelta::<T>::Remove => self.blindly_remove_concept(id),
				ConceptDelta::<T>::Update(d) => self.write_concept(id).apply(d),
			},
		};
	}
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

impl<T> StringAdderDelta for Context<T> 
where
	T: Delta,
{
	fn add_string_delta(&self, string_id: usize, string: &str) -> ContextDelta<T> {
		ContextDelta::<T>::String(string.to_string(), StringDelta::Insert(string_id))
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

impl<T> SetConceptDefinitionDelta for Context<T> 
where
	Self: ConceptReader<T>,
	T: SetDefinitionDelta,
{
	fn set_concept_definition_delta(&self, concept: usize, lefthand: usize, righthand: usize) -> ZiaResult<ContextDelta<T>> {
		Ok(ContextDelta::<T>::Concept(concept, ConceptDelta::<T>::Update(try!(self.read_concept(concept).set_definition_delta(lefthand, righthand)))))
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
        match self.string_map.remove(string) {
			Some(_) => (),
			None => panic!("No string to remove!"),
		};
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

impl<T> ConceptAdderDelta<T> for Context<T> 
where
	T: Delta,
{
	fn add_concept_delta(&self, deltas: &Vec<Self::Delta>, concept: T) -> (usize, Self::Delta) {
		let mut added_gaps = Vec::<usize>::new();
		let mut removed_gaps = HashSet::<usize>::new();
		let mut new_concept_length = self.concepts.len();
		for delta in deltas {
			match delta {
				ContextDelta::<T>::Concept(id, ConceptDelta::<T>::Insert(_)) => if *id < new_concept_length {
					removed_gaps.insert(*id);
				} else {
					new_concept_length += 1
				},
				ContextDelta::<T>::Concept(id, ConceptDelta::<T>::Remove) => {
					added_gaps.push(*id);
					removed_gaps.remove(&id);
				},
				_ => (),
			};
		}
		let index: usize;
		let mut gap_index = if self.gaps.len() == 0 {
			None
		} else {
			Some(self.gaps.len() - 1)
		};
		loop {
			match added_gaps.pop() {
				Some(id) => if removed_gaps.contains(&id) {
					continue;
				} else {
					index = id;
					break;
				},
				None => match gap_index {
					Some(gi) => if removed_gaps.contains(&self.gaps[gi]) {
						if gi == 0 {
							index = new_concept_length;
							break;
						} else {
							gap_index = Some(gi - 1);
							continue;
						}
					} else {
						index = self.gaps[gi];
						break;
					},
					None => {
						index = new_concept_length;
						break;
					}
				},
			};
		}
		(index, ContextDelta::<T>::Concept(index, ConceptDelta::Insert(concept)))
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

#[cfg(test)]
mod tests {
	use adding::{StringAdder, ConceptAdder};
	use concepts::{Concept, AbstractPart};
	use context::Context;
	use reading::{ConceptReader, GetReduction};
	use removing::{BlindConceptRemover, StringRemover};
	use translating::StringConcept;
	use writing::{ConceptWriter, SetReduction};
	proptest!{
		#[test]
		fn getting_the_string_that_was_added(id: usize, string: String) {
			let mut cont = Context::<Concept>::default();
			cont.add_string(id, &string);
			assert_eq!(cont.get_string_concept(&string), Some(id))
		}
		#[test]
		fn not_getting_the_string_that_was_not_added(string: String) {
			let cont = Context::<Concept>::default();
			assert_eq!(cont.get_string_concept(&string), None);
		}
		#[test]
		fn not_getting_the_string_that_was_removed(id: usize, string: String) {
			let mut cont = Context::<Concept>::default();
			cont.add_string(id, &string);
			cont.remove_string(&string);
			assert_eq!(cont.get_string_concept(&string), None);
		}
		#[test]
		#[should_panic]
		fn removing_a_string_that_was_not_added(string: String) {
			let mut cont = Context::<Concept>::default();
			cont.remove_string(&string);
		}
		#[test]
		fn reading_and_writing_a_concept_that_was_added(reduction: usize) {
			let mut cont = Context::<Concept>::default();
			let concept_id = cont.add_concept(AbstractPart::default().into());
			try!(cont.write_concept(concept_id).make_reduce_to(reduction));
			assert_eq!(cont.read_concept(concept_id).get_reduction(), Some(reduction));
		}
		#[test]
		#[should_panic]
		fn reading_a_concept_that_was_not_added(id: usize) {
			let cont = Context::<Concept>::default();
			cont.read_concept(id);		
		}
		#[test]
		#[should_panic]
		fn writing_a_concept_that_was_not_added(id: usize) {
			let mut cont = Context::<Concept>::default();
			cont.write_concept(id);		
		}
		#[test]
		#[should_panic]
		fn removing_a_concept_that_was_not_added(id: usize) {
			let mut cont = Context::<Concept>::default();
			cont.blindly_remove_concept(id);
		}
	}
	#[test]
	#[should_panic]
	fn reading_a_concept_that_was_removed() {
		let mut cont = Context::<Concept>::default();
		let concept_id = cont.add_concept(AbstractPart::default().into());
		cont.blindly_remove_concept(concept_id);
		cont.read_concept(concept_id);
	}
	#[test]
	#[should_panic]
	fn writing_a_concept_that_was_removed() {
		let mut cont = Context::<Concept>::default();
		let concept_id = cont.add_concept(AbstractPart::default().into());
		cont.blindly_remove_concept(concept_id);
		cont.write_concept(concept_id);
	}
}
