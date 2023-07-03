use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
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

    fn bind_nonquantifier_concept(self, concept: impl Into<Self::ConceptId>) -> Self;

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
    ) -> Option<HashMap<Self::SharedSyntax, Self::SharedSyntax>>;
}
