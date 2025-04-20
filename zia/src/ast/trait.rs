use maplit::hashmap;

use crate::{
    and_also::AndAlso, consistent_merge::ConsistentMerge,
    mixed_concept::ConceptId, nester::SharedReference,
    substitute::Substitutions, variable_mask_list::VariableMask,
};

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
