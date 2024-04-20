use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

use maplit::hashmap;

use crate::{
    and_also::AndAlso, consistent_merge::ConsistentMerge,
    substitute::Substitutions, variable_mask_list::VariableMask,
};

pub trait SyntaxTree
where
    Self: Clone
        + Debug
        + Display
        + Eq
        + for<'a> From<&'a str>
        + From<String>
        + Hash
        + PartialEq<Self::SharedSyntax>,
{
    type SharedSyntax: AsRef<Self>
        + Borrow<Self>
        + Clone
        + Debug
        + Deref<Target = Self>
        + Display
        + Eq
        + Hash;
    type ConceptId: Copy + Debug + Display + Eq + Hash;
    fn share(self) -> Self::SharedSyntax;

    fn make_mut(refcounter: &mut Self::SharedSyntax) -> &mut Self;

    fn is_leaf_variable(&self) -> bool;

    fn new_constant_concept(concept_id: impl Into<Self::ConceptId>) -> Self;

    fn new_quantifier_concept(concept_id: Self::ConceptId) -> Self;

    fn new_pair(left: Self::SharedSyntax, right: Self::SharedSyntax) -> Self;

    fn new_leaf_variable(concept_id: Self::ConceptId) -> Self;

    fn bind_nonquantifier_concept(
        self,
        concept: impl Into<Self::ConceptId>,
    ) -> Self;

    fn bind_nonquantifier_concept_as_ref(&mut self, concept: Self::ConceptId);

    fn bind_quantifier_concept(self, concept: Self::ConceptId) -> Self;

    fn contains(&self, other: &Self) -> bool;

    /// An expression does have an expansion while a symbol does not.
    fn get_expansion(&self)
        -> Option<(Self::SharedSyntax, Self::SharedSyntax)>;

    fn get_expansion_mut(&mut self) -> Option<(&mut Self, &mut Self)>;

    fn bind_pair(
        self,
        left: Self::SharedSyntax,
        right: Self::SharedSyntax,
    ) -> Self;

    fn get_concept(&self) -> Option<Self::ConceptId>;

    fn is_variable(&self) -> bool;

    fn check_example(
        example: &Self::SharedSyntax,
        generalisation: &Self::SharedSyntax,
    ) -> Option<ExampleSubstitutions<Self>>;
}

#[derive(Clone, Debug)]
pub struct ExampleSubstitutions<S: SyntaxTree> {
    pub generalisation: Substitutions<S::SharedSyntax>,
    pub example: VariableMask<S>,
}

impl<S: SyntaxTree> Default for ExampleSubstitutions<S> {
    fn default() -> Self {
        Self {
            generalisation: hashmap! {},
            example: hashmap! {},
        }
    }
}

impl<S: SyntaxTree> ConsistentMerge for ExampleSubstitutions<S> {
    type Output = Self;

    fn consistent_merge(self, other: Self) -> Option<Self::Output> {
        self.generalisation
            .consistent_merge(other.generalisation)
            .and_also_move(self.example.consistent_merge(other.example))
            .map(|(generalisation, example)| Self {
                generalisation,
                example,
            })
    }
}
