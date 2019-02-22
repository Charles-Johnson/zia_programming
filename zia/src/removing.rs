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

use errors::{ZiaError, ZiaResult};
use reading::{FindWhatReducesToIt, MaybeDisconnected, MaybeString};
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
        + MaybeString,
{
    fn cleanly_delete_definition(&mut self, concept: usize) -> ZiaResult<()> {
        match self.read_concept(&[], concept).get_definition() {
            None => Err(ZiaError::RedundantDefinitionRemoval),
            Some((left, right)) => {
                self.delete_definition(concept, left, right);
                try!(self.try_delete_concept(concept));
                try!(self.try_delete_concept(left));
                self.try_delete_concept(right)
            }
        }
    }
    fn try_delete_concept(&mut self, concept: usize) -> ZiaResult<()> {
        if self.is_disconnected(concept) {
            try!(self.unlabel(concept));
            self.remove_concept(concept);
        }
        Ok(())
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
        + MaybeString,
{
}

pub trait ConceptRemover<T>
where
    Self: BlindConceptRemover + ConceptReader<T> + StringRemover,
    T: MaybeString,
{
    fn remove_concept(&mut self, concept: usize) {
        if let Some(ref s) = self.read_concept(&[], concept).get_string() {
            self.remove_string(s);
        }
        self.blindly_remove_concept(concept);
    }
}

impl<S, T> ConceptRemover<T> for S
where
    S: BlindConceptRemover + ConceptReader<T> + StringRemover,
    T: MaybeString,
{
}

pub trait BlindConceptRemover {
    fn blindly_remove_concept(&mut self, usize);
}

pub trait StringRemover {
    fn remove_string(&mut self, &str);
}
