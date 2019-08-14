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
    T: GetReduction + GetDefinition + GetDefinitionOf + Debug,
    Self: DeleteReduction<T> + GetConceptOfLabel<T> + Delta,
    Self::Delta: Clone,
{
    fn unlabel(&self, deltas: &mut Vec<Self::Delta>, concept: usize) -> ZiaResult<()> {
        let concept_of_label = self
            .get_concept_of_label(deltas, concept)
            .expect("No label to remove");
        self.delete_reduction(deltas, concept_of_label)
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
    S::Delta: Clone,
{
}

pub trait DeleteReduction<T>
where
    T: GetReduction,
    Self: ConceptWriter<T> + ConceptReader<T> + Delta + RemoveConceptReduction,
    Self::Delta: Clone,
{
    fn try_removing_reduction<U: MaybeConcept + Display>(
        &self,
        deltas: &mut Vec<Self::Delta>,
        syntax: &U,
    ) -> ZiaResult<()> {
        if let Some(c) = syntax.get_concept() {
            self.delete_reduction(deltas, c)
        } else {
            Err(ZiaError::RedundantReduction)
        }
    }
    fn delete_reduction(
        &self,
        deltas: &mut Vec<Self::Delta>,
        concept: usize,
    ) -> ZiaResult<()> {
        self.read_concept(deltas, concept)
            .get_reduction()
            .map(|n| {
                let extra_deltas = self.remove_concept_reduction(deltas, concept, n);
                deltas.extend(extra_deltas.iter().cloned());
            })
            .ok_or(ZiaError::RedundantReduction)
    }
}

impl<S, T> DeleteReduction<T> for S
where
    S: ConceptWriter<T> + ConceptReader<T> + RemoveConceptReduction,
    T: GetReduction,
    S::Delta: Clone,
{
}

pub trait DeleteDefinition<T>
where
    Self: Delta,
{
    fn delete_definition(&self, &[Self::Delta], usize, usize, usize) -> [Self::Delta; 3];
}

pub trait UpdateReduction<T>
where
    T: SetReduction + MakeReduceFrom + GetReduction + GetDefinition + GetDefinitionOf,
    Self: GetNormalForm<T> + FindDefinition<T> + SetConceptReductionDelta,
{
    fn update_reduction(
        &self,
        deltas: &mut Vec<Self::Delta>,
        concept: usize,
        reduction: usize,
    ) -> ZiaResult<()> {
        self.get_normal_form(deltas, reduction)
            .and_then(|n| {
                if concept == n {
                    Some(Err(ZiaError::CyclicReduction))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                self.read_concept(deltas, concept)
                    .get_reduction()
                    .and_then(|r| {
                        if r == reduction {
                            Some(Err(ZiaError::RedundantReduction))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        if reduction == self.get_reduction_of_composition(deltas, concept) {
                            Err(ZiaError::RedundantReduction)
                        } else {
                            self.concept_reduction_deltas(deltas, concept, reduction)
                        }
                    })
            })
    }
    fn get_reduction_of_composition(&self, deltas: &[Self::Delta], concept: usize) -> usize {
        self.read_concept(deltas, concept)
            .get_definition()
            .and_then(|(left, right)| {
                self.find_definition(
                    deltas,
                    self.get_reduction_or_reduction_of_composition(deltas, left),
                    self.get_reduction_or_reduction_of_composition(deltas, right),
                )
            })
            .unwrap_or(concept)
    }
    fn get_reduction_or_reduction_of_composition(
        &self,
        deltas: &[Self::Delta],
        concept: usize,
    ) -> usize {
        self.read_concept(deltas, concept)
            .get_reduction()
            .unwrap_or_else(|| self.get_reduction_of_composition(deltas, concept))
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
        deltas: &mut Vec<Self::Delta>,
        definition: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<()> {
        if self.contains(deltas, lefthand, definition)
            || self.contains(deltas, righthand, definition)
        {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.check_reductions(deltas, definition, lefthand)?;
            self.check_reductions(deltas, definition, righthand)?;
            self.set_concept_definition_deltas(deltas, definition, lefthand, righthand)?;
            Ok(())
        }
    }
    fn check_reductions(
        &self,
        deltas: &[Self::Delta],
        outer_concept: usize,
        inner_concept: usize,
    ) -> ZiaResult<()> {
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
    fn set_concept_definition_deltas(
        &self,
        &mut Vec<Self::Delta>,
        usize,
        usize,
        usize,
    ) -> ZiaResult<()>;
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
    fn concept_reduction_deltas(&self, &mut Vec<Self::Delta>, usize, usize) -> ZiaResult<()>;
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

pub trait RemoveConceptReduction
where
    Self: Delta,
{
    fn remove_concept_reduction(&self, &[Self::Delta], usize, usize) -> [Self::Delta; 2];
}

pub trait RemoveReductionDelta
where
    Self: Delta,
{
    fn no_longer_reduces_from_delta(&self, &[Self::Delta], usize) -> Self::Delta;
    fn make_reduce_to_none_delta(&self, &[Self::Delta]) -> Self::Delta;
}

pub trait RemoveDefinitionDelta
where
    Self: Delta,
{
    fn remove_as_lefthand_of_delta(&self, &[Self::Delta], usize) -> Self::Delta;
    fn remove_as_righthand_of_delta(&self, &[Self::Delta], usize) -> Self::Delta;
    fn remove_definition_delta(&self, &[Self::Delta]) -> Self::Delta;
}
