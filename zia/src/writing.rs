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

pub use delta::{ApplyDelta, Delta};
pub use errors::{ZiaError, ZiaResult};
pub use reading::{
    FindDefinition, GetDefinition, GetDefinitionOf, GetNormalForm, GetReduction,
    MaybeConcept,
};

pub trait Unlabeller: ApplyDelta
{
    fn unlabel(&self, deltas: &mut Self::Delta, concept: usize) -> ZiaResult<()>;
}

pub trait DeleteReduction<U>: ApplyDelta
{
    fn try_removing_reduction(&self, deltas: &mut Self::Delta, syntax: &U) -> ZiaResult<()>;
    fn delete_reduction(&self, delta: &mut Self::Delta, concept: usize) -> ZiaResult<()>;
}

pub trait UpdateReduction: ApplyDelta
{
    fn update_reduction(
        &self,
        deltas: &mut Self::Delta,
        concept: usize,
        reduction: usize,
    ) -> ZiaResult<()>;
    fn get_reduction_of_composition(&self, deltas: &Self::Delta, concept: usize) -> usize;
    fn get_reduction_or_reduction_of_composition(
        &self,
        deltas: &Self::Delta,
        concept: usize,
    ) -> usize;
}

pub trait InsertDefinition: ApplyDelta
{
    fn insert_definition(
        &self,
        deltas: &mut Self::Delta,
        definition: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<()>;
    fn check_reductions(
        &self,
        deltas: &Self::Delta,
        outer_concept: usize,
        inner_concept: usize,
    ) -> ZiaResult<()>;
}

pub trait RemoveReduction {
    fn make_reduce_to_none(&mut self);
}

pub trait NoLongerReducesFrom {
    fn no_longer_reduces_from(&mut self, usize);
}

pub trait SetConceptDefinitionDeltas
where
    Self: ApplyDelta,
{
    fn set_concept_definition_deltas(
        &self,
        &mut Self::Delta,
        usize,
        usize,
        usize,
    ) -> ZiaResult<()>;
}

pub trait SetDefinitionDelta
where
    Self: ApplyDelta,
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
    Self: ApplyDelta,
{
    fn add_as_lefthand_of_delta(&self, usize) -> Self::Delta;
    fn add_as_righthand_of_delta(&self, usize) -> Self::Delta;
}

pub trait SetReduction {
    fn make_reduce_to(&mut self, usize) -> ZiaResult<()>;
}

pub trait SetReductionDelta
where
    Self: ApplyDelta,
{
    fn make_reduce_to_delta(&self, usize) -> ZiaResult<Self::Delta>;
}

pub trait SetConceptReductionDelta
where
    Self: ApplyDelta,
{
    fn concept_reduction_deltas(&self, &mut Self::Delta, usize, usize) -> ZiaResult<()>;
}

pub trait MakeReduceFrom {
    fn make_reduce_from(&mut self, usize);
}

pub trait MakeReduceFromDelta
where
    Self: ApplyDelta,
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
    Self: ApplyDelta,
{
    fn remove_concept_reduction(&self, &Self::Delta, usize, usize) -> Self::Delta;
}

pub trait RemoveReductionDelta
where
    Self: ApplyDelta,
{
    fn no_longer_reduces_from_delta(&self, &Self::Delta, usize) -> Self::Delta;
    fn make_reduce_to_none_delta(&self, &Self::Delta) -> Self::Delta;
}

pub trait RemoveDefinitionDelta
where
    Self: ApplyDelta,
{
    fn remove_as_lefthand_of_delta(&self, &Self::Delta, usize) -> Self::Delta;
    fn remove_as_righthand_of_delta(&self, &Self::Delta, usize) -> Self::Delta;
    fn remove_definition_delta(&self, &Self::Delta) -> Self::Delta;
}
