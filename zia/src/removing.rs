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
use reading::{FindWhatReducesToIt, MaybeConcept, MaybeDisconnected, MaybeString};
use std::fmt::{Debug, Display};
use writing::{
    ConceptReader, DeleteDefinition, GetDefinition, GetDefinitionOf, GetReduction,
    NoLongerReducesFrom, RemoveAsDefinitionOf, RemoveDefinition, RemoveReduction, Unlabeller,
};

pub trait DefinitionDeleter<T, U>
where
    Self: MaybeDisconnected<T> + ConceptRemover<T> + DeleteDefinition<T> + Unlabeller<T, U>,
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
    Self::Delta: Clone,
    U: MaybeConcept + Display,
{
    fn cleanly_delete_definition(
        &self,
        deltas: &mut Vec<Self::Delta>,
        concept: usize,
    ) -> ZiaResult<()> {
        match self.read_concept(deltas, concept).get_definition() {
            None => Err(ZiaError::RedundantDefinitionRemoval),
            Some((left, right)) => {
                let extra_deltas = self.delete_definition(deltas, concept, left, right);
                deltas.extend(extra_deltas.iter().cloned());
                self.try_delete_concept(deltas, concept)?;
                self.try_delete_concept(deltas, left)?;
                self.try_delete_concept(deltas, right)
            }
        }
    }
    fn try_delete_concept(
        &self,
        previous_deltas: &mut Vec<Self::Delta>,
        concept: usize,
    ) -> ZiaResult<()> {
        if self.is_disconnected(previous_deltas, concept) {
            self.unlabel(previous_deltas, concept)?;
            self.remove_concept(previous_deltas, concept)
        }
        Ok(())
    }
}

impl<S, T, U> DefinitionDeleter<T, U> for S
where
    S: MaybeDisconnected<T> + ConceptRemover<T> + DeleteDefinition<T> + Unlabeller<T, U>,
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
    S::Delta: Clone,
    U: Display + MaybeConcept,
{
}

pub trait ConceptRemover<T>
where
    Self: BlindConceptRemoverDeltas + ConceptReader<T> + StringRemoverDeltas,
    T: MaybeString,
{
    fn remove_concept(&self, deltas: &mut Vec<Self::Delta>, concept: usize) {
        if let Some(ref s) = self.read_concept(deltas, concept).get_string() {
            self.remove_string_deltas(deltas, s);
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
    fn blindly_remove_concept_deltas(&self, &mut Vec<Self::Delta>, usize);
}

pub trait StringRemover {
    fn remove_string(&mut self, &str);
}

pub trait StringRemoverDeltas
where
    Self: Delta,
{
    fn remove_string_deltas(&self, &mut Vec<Self::Delta>, &str);
}
