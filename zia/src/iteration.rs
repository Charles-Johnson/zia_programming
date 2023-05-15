use std::collections::HashSet;

use crate::{ast::SyntaxTree, context_search::Generalisations};

pub trait Iteration {
    type Syntax: SyntaxTree;
    type ConceptId;
    fn filter_generalisations_from_candidates(
        &self,
        example: &<Self::Syntax as SyntaxTree>::SharedSyntax,
        candidates: HashSet<Self::ConceptId>,
    ) -> Generalisations<Self::Syntax>;
}