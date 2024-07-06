use std::collections::HashSet;

use crate::{ast::SyntaxTree, context_search::Generalisation};

pub trait Iteration<'a, Syntax: SyntaxTree> {
    type ConceptId;
    fn filter_generalisations_from_candidates(
        &self,
        example: Syntax::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> impl Iterator<Item = Generalisation<Syntax>> + 'a;
}
