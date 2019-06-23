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

pub use delta::Delta;
pub use errors::{ZiaError, ZiaResult};
use logging::Logger;
pub use reading::{
    ConceptReader, FindDefinition, GetDefinition, GetDefinitionOf, GetNormalForm, GetReduction,
    MaybeConcept,
};
use reading::{Container, GetConceptOfLabel};
use std::fmt::{Debug, Display};
pub trait Unlabeller<T>
where
    T: GetReduction
        + RemoveReduction
        + NoLongerReducesFrom
        + GetDefinition
        + GetDefinitionOf
        + Debug,
    Self: DeleteReduction<T> + GetConceptOfLabel<T>,
{
    fn unlabel(&mut self, concept: usize) -> ZiaResult<()> {
        match self.get_concept_of_label(concept) {
            None => panic!("No label to remove"),
            Some(d) => self.delete_reduction(d),
        }
    }
}

impl<S, T> Unlabeller<T> for S
where
    T: GetReduction
        + RemoveReduction
        + NoLongerReducesFrom
        + GetDefinitionOf
        + GetDefinition
        + Debug,
    S: DeleteReduction<T> + GetConceptOfLabel<T>,
{
}

pub trait DeleteReduction<T>
where
    T: GetReduction + RemoveReduction + NoLongerReducesFrom,
    Self: ConceptWriter<T> + ConceptReader<T> + Logger,
{
    fn try_removing_reduction<U: MaybeConcept + Display>(&mut self, syntax: &U) -> ZiaResult<()> {
        info!(
            self.logger(),
            "try_removing_reduction({})",
            syntax,
        );
        if let Some(c) = syntax.get_concept() {
            self.delete_reduction(c)
        } else {
            Err(ZiaError::RedundantReduction)
        }
    }
    fn delete_reduction(&mut self, concept: usize) -> ZiaResult<()> {
        match self.read_concept(&[], concept).get_reduction() {
            None => Err(ZiaError::RedundantReduction),
            Some(n) => {
                self.write_concept(n).no_longer_reduces_from(concept);
                self.write_concept(concept).make_reduce_to_none();
                Ok(())
            }
        }
    }
}

impl<S, T> DeleteReduction<T> for S
where
    S: ConceptWriter<T> + ConceptReader<T> + Logger,
    T: GetReduction + RemoveReduction + NoLongerReducesFrom,
{
}

pub trait DeleteDefinition<T>
where
    T: GetDefinition + RemoveDefinition + RemoveAsDefinitionOf + Sized,
    Self: ConceptReader<T> + ConceptWriter<T>,
{
    fn delete_definition(&mut self, concept: usize, left: usize, right: usize) {
        self.write_concept(left).remove_as_lefthand_of(concept);
        self.write_concept(right).remove_as_righthand_of(concept);
        self.write_concept(concept).remove_definition();
    }
}

impl<S, T> DeleteDefinition<T> for S
where
    T: GetDefinition + RemoveDefinition + RemoveAsDefinitionOf + Sized,
    S: ConceptReader<T> + ConceptWriter<T>,
{
}

pub trait UpdateReduction<T>
where
    T: SetReduction + MakeReduceFrom + GetReduction + GetDefinition + GetDefinitionOf,
    Self: GetNormalForm<T> + FindDefinition<T> + SetConceptReductionDelta,
{
    fn update_reduction(&self, deltas: &[Self::Delta], concept: usize, reduction: usize) -> ZiaResult<Vec<Self::Delta>> {
        if let Some(n) = self.get_normal_form(deltas, reduction) {
            if concept == n {
                return Err(ZiaError::CyclicReduction);
            }
        }
        if let Some(r) = self.read_concept(deltas, concept).get_reduction() {
            if r == reduction {
                return Err(ZiaError::RedundantReduction);
            }
        }
        let r = try!(self.get_reduction_of_composition(deltas, concept));
        if r == reduction {
            return Err(ZiaError::RedundantReduction);
        } else if r != concept {
            return Err(ZiaError::MultipleReductionPaths);
        }
        self.concept_reduction_deltas(deltas, concept, reduction)
    }
    fn get_reduction_of_composition(&self, deltas: &[Self::Delta], concept: usize) -> ZiaResult<usize> {
        if let Some((left, right)) = self.read_concept(deltas, concept).get_definition() {
            let lc = if let Some(l) = self.read_concept(deltas, left).get_reduction() {
                l
            } else {
                try!(self.get_reduction_of_composition(deltas, left))
            };
            let rc = if let Some(r) = self.read_concept(deltas, right).get_reduction() {
                r
            } else {
                try!(self.get_reduction_of_composition(deltas, right))
            };
            match self.find_definition(deltas, lc, rc) {
                Some(dc) => Ok(dc),
                None => Err(ZiaError::MultipleReductionPaths),
            }
        } else {
            Ok(concept)
        }
    }
}

impl<S, T> UpdateReduction<T> for S
where
    T: SetReduction + MakeReduceFrom + GetReduction + GetDefinition + GetDefinitionOf,
    S: ConceptWriter<T> + GetNormalForm<T> + FindDefinition<T> + Logger + SetConceptReductionDelta,
{
}

pub trait InsertDefinition<T>
where
    T: Sized + GetDefinition + GetReduction,
    Self: ConceptWriter<T> + Container<T> + SetConceptDefinitionDeltas + Logger,
    Self::Delta: Clone + Debug,
{
    fn insert_definition(
        &self,
        deltas: &Vec<Self::Delta>,        
        definition: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<Vec<Self::Delta>> {
        if self.contains(deltas, lefthand, definition) || self.contains(deltas, righthand, definition) {
            Err(ZiaError::InfiniteDefinition)
        } else {
            try!(self.check_reductions(deltas, definition, lefthand));
            try!(self.check_reductions(deltas, definition, righthand));
            let new_deltas = try!(self.set_concept_definition_deltas(deltas, definition, lefthand, righthand));
            Ok(new_deltas)
        }
    }
    fn check_reductions(&self, deltas: &[Self::Delta], outer_concept: usize, inner_concept: usize) -> ZiaResult<()> {
        if let Some(r) = self.read_concept(deltas, inner_concept).get_reduction() {
            if r == outer_concept || self.contains(deltas, r, outer_concept) {
                Err(ZiaError::InfiniteDefinition)
            } else {
                self.check_reductions(deltas, outer_concept, r)
            }
        } else {
            Ok(())
        }
    }
}

impl<S, T> InsertDefinition<T> for S
where
    T: Sized + GetDefinition + GetReduction,
    S: ConceptWriter<T> + Container<T> + SetConceptDefinitionDeltas + Logger,
    Self::Delta: Clone + Debug,
{
}

pub trait ConceptWriter<T> {
    fn write_concept(&mut self, usize) -> &mut T;
}

pub trait RemoveReduction {
    fn make_reduce_to_none(&mut self);
}

pub trait NoLongerReducesFrom {
    fn no_longer_reduces_from(&mut self, usize);
}

pub trait SetConceptDefinitionDeltas
where
    Self: Delta,
{
    fn set_concept_definition_deltas(&self, &Vec<Self::Delta>, usize, usize, usize) -> ZiaResult<Vec<Self::Delta>>;
}

pub trait SetDefinitionDelta
where
    Self: Delta,
{
    fn set_definition_delta(&self, usize, usize) -> ZiaResult<Self::Delta>;
}

pub trait SetDefinition {
    fn set_definition(&mut self, usize, usize) -> ZiaResult<()>;
}

pub trait SetAsDefinitionOf {
    fn add_as_lefthand_of(&mut self, usize);
    fn add_as_righthand_of(&mut self, usize);
}

pub trait SetAsDefinitionOfDelta
where
    Self: Delta,
{
    fn add_as_lefthand_of_delta(&self, usize) -> Self::Delta;
    fn add_as_righthand_of_delta(&self, usize) -> Self::Delta;
}

pub trait SetReduction {
    fn make_reduce_to(&mut self, usize) -> ZiaResult<()>;
}

pub trait SetReductionDelta 
where
    Self: Delta,
{
    fn make_reduce_to_delta(&self, usize) -> ZiaResult<Self::Delta>;
}

pub trait SetConceptReductionDelta
where
    Self: Delta,
{
    fn concept_reduction_deltas(&self, &[Self::Delta], usize, usize) -> ZiaResult<Vec<Self::Delta>>;
}

pub trait MakeReduceFrom {
    fn make_reduce_from(&mut self, usize);
}

pub trait MakeReduceFromDelta 
where
    Self: Delta,
{
    fn make_reduce_from_delta(&self, usize) -> Self::Delta;
}

pub trait RemoveDefinition {
    fn remove_definition(&mut self);
}

pub trait RemoveAsDefinitionOf {
    fn remove_as_lefthand_of(&mut self, usize);
    fn remove_as_righthand_of(&mut self, usize);
}
