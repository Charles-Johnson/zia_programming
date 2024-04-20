use std::collections::HashMap;

use crate::ast::SyntaxTree;

pub type Substitutions<SharedSyntax> = HashMap<SharedSyntax, SharedSyntax>;

pub fn substitute<Syntax: SyntaxTree>(
    syntax: &mut Syntax,
    substitutions: &Substitutions<Syntax::SharedSyntax>,
) {
    if let Some(substitution) = substitutions.get(syntax) {
        *syntax = substitution.as_ref().clone();
    } else if let Some((left, right)) = syntax.get_expansion_mut() {
        substitute(left, substitutions);
        substitute(right, substitutions);
    }
}
