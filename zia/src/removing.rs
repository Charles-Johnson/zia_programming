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
use errors::{ZiaError, ZiaResult};
use reading::{FindWhatReducesToIt, MaybeDisconnected, MaybeString};
use std::fmt::Debug;
use writing::{
    ConceptReader, DeleteDefinition, GetDefinition, GetDefinitionOf, GetReduction,
    NoLongerReducesFrom, RemoveAsDefinitionOf, RemoveDefinition, RemoveReduction, Unlabeller,
};

pub trait DefinitionDeleter<T>
where
    Self: MaybeDisconnected<T> + ConceptRemover<T> + DeleteDefinition<T> + Unlabeller<T>,
    T: RemoveDefinition
        + RemoveAsDefinitionOf
        + RemoveReduction
        + NoLongerReducesFrom
        + GetDefinitionOf
        + GetDefinition
        + FindWhatReducesToIt
        + GetReduction
        + MaybeString
        + Debug,
{
    fn cleanly_delete_definition(&self, deltas: Vec<Self::Delta>, concept: usize) -> ZiaResult<Vec<Self::Delta>> {
        match self.read_concept(&deltas, concept).get_definition() {
            None => Err(ZiaError::RedundantDefinitionRemoval),
            Some((left, right)) => {
                let deltas0 = self.delete_definition(deltas, concept, left, right);
                let deltas1 = self.try_delete_concept(deltas0, concept)?;
                let deltas2 = self.try_delete_concept(deltas1, left)?;
                self.try_delete_concept(deltas2, right)
            }
        }
    }
    fn try_delete_concept(&self, previous_deltas: Vec<Self::Delta>, concept: usize) -> ZiaResult<Vec<Self::Delta>> {
        if self.is_disconnected(concept) {
            let initial_deltas = self.unlabel(previous_deltas, concept)?;
            Ok(self.remove_concept(initial_deltas, concept))
        } else {
            Ok(vec!())
        }
    }
}

impl<S, T> DefinitionDeleter<T> for S
where
    S: MaybeDisconnected<T> + ConceptRemover<T> + DeleteDefinition<T> + Unlabeller<T>,
    T: RemoveDefinition
        + RemoveAsDefinitionOf
        + RemoveReduction
        + NoLongerReducesFrom
        + GetDefinitionOf
        + GetDefinition
        + FindWhatReducesToIt
        + GetReduction
        + MaybeString
        + Debug,
{
}

pub trait ConceptRemover<T>
where
    Self: BlindConceptRemoverDeltas + ConceptReader<T> + StringRemoverDeltas,
    T: MaybeString,
{
    fn remove_concept(&self, mut deltas: Vec<Self::Delta>, concept: usize) -> Vec<Self::Delta> {
        if let Some(ref s) = self.read_concept(&deltas, concept).get_string() {
            deltas = self.remove_string_deltas(deltas, s);
        }
        self.blindly_remove_concept_deltas(deltas, concept)
    }
}

impl<S, T> ConceptRemover<T> for S
where
    S: BlindConceptRemoverDeltas + ConceptReader<T> + StringRemoverDeltas,
    T: MaybeString,
{
}

pub trait BlindConceptRemover {
    fn blindly_remove_concept(&mut self, usize);
}

pub trait BlindConceptRemoverDeltas 
where
    Self: Delta,
{
    fn blindly_remove_concept_deltas(&self, Vec<Self::Delta>, usize) -> Vec<Self::Delta>;
}

pub trait StringRemover {
    fn remove_string(&mut self, &str);
}

pub trait StringRemoverDeltas 
where
    Self: Delta,
{
    fn remove_string_deltas(&self, deltas: Vec<Self::Delta>, &str) -> Vec<Self::Delta>;
}
