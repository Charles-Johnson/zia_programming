use std::collections::HashMap;

use crate::{
    ast::{GenericSyntaxTree, SyntaxKey, SyntaxTree},
    context_cache::SharedSyntax,
    mixed_concept::ConceptId,
    nester::SharedReference,
};

pub type Substitutions<CI, SR> = HashMap<SyntaxKey<CI>, SharedSyntax<CI, SR>>;

pub fn substitute<CI: ConceptId, SR: SharedReference>(
    syntax: &mut GenericSyntaxTree<CI, SR>,
    substitutions: &Substitutions<CI, SR>,
) {
    if let Some(substitution) = substitutions.get(&syntax.key()) {
        *syntax = substitution.as_ref().clone();
    } else if let Some((left, right)) = syntax.get_expansion_mut() {
        substitute(left, substitutions);
        substitute(right, substitutions);
    }
}
