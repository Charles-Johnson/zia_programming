use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use maplit::hashmap;

use crate::{
    and_also::AndAlso, consistent_merge::ConsistentMerge,
    mixed_concept::ConceptId, nester::SharedReference,
    substitute::Substitutions, variable_mask_list::VariableMask,
};

pub trait SyntaxTree<SR: SharedReference>
where
    Self: Clone
        + Debug
        + Display
        + Eq
        + for<'a> From<&'a str>
        + From<String>
        + Hash,
{
    type ConceptId: ConceptId;
    fn share(self) -> SR::Share<Self> {
        SR::share(self)
    }

    fn make_mut(refcounter: &mut SR::Share<Self>) -> &mut Self {
        SR::make_mut(refcounter)
    }

    fn is_leaf_variable(&self) -> bool;

    fn new_constant_concept(concept_id: impl Into<Self::ConceptId>) -> Self;

    fn new_quantifier_concept(concept_id: Self::ConceptId) -> Self;

    fn new_pair(left: SR::Share<Self>, right: SR::Share<Self>) -> Self;

    fn new_leaf_variable(concept_id: Self::ConceptId) -> Self;

    fn bind_nonquantifier_concept(
        self,
        concept: impl Into<Self::ConceptId>,
    ) -> Self;

    fn bind_nonquantifier_concept_as_ref(&mut self, concept: Self::ConceptId);

    fn bind_quantifier_concept(self, concept: Self::ConceptId) -> Self;

    fn contains(&self, other: &Self) -> bool;

    /// An expression does have an expansion while a symbol does not.
    fn get_expansion(&self) -> Option<(SR::Share<Self>, SR::Share<Self>)>;

    fn get_expansion_mut(&mut self) -> Option<(&mut Self, &mut Self)>;

    fn bind_pair(self, left: SR::Share<Self>, right: SR::Share<Self>) -> Self;

    fn get_concept(&self) -> Option<Self::ConceptId>;

    fn is_variable(&self) -> bool;

    fn check_example(
        example: &SR::Share<Self>,
        generalisation: &SR::Share<Self>,
    ) -> Option<ExampleSubstitutions<Self::ConceptId, SR>>;
}

#[derive(Clone)]
pub struct ExampleSubstitutions<CI: ConceptId, SR: SharedReference> {
    pub generalisation: Substitutions<CI, SR>,
    pub example: VariableMask<CI, SR>,
}

impl<CI: ConceptId, SR: SharedReference> Default
    for ExampleSubstitutions<CI, SR>
{
    fn default() -> Self {
        Self {
            generalisation: hashmap! {},
            example: hashmap! {},
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> ConsistentMerge<SR>
    for ExampleSubstitutions<CI, SR>
{
    type Output = Self;

    fn consistent_merge(self, other: Self) -> Option<Self::Output> {
        <Substitutions<CI, SR> as ConsistentMerge<SR>>::consistent_merge(
            self.generalisation,
            other.generalisation,
        )
        .and_also_move(
            <VariableMask<CI, SR> as ConsistentMerge<SR>>::consistent_merge(
                self.example,
                other.example,
            ),
        )
        .map(|(generalisation, example)| Self {
            generalisation,
            example,
        })
    }
}
