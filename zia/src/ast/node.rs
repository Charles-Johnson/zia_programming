use std::{collections::HashSet, fmt::Debug};

use crate::{mixed_concept::ConceptId, nester::SharedReference};

use super::{GenericSyntaxTree, SyntaxKey};

#[derive(Clone)]
pub enum Node<SR: SharedReference, CI: ConceptId> {
    /// This syntax tree may branch to two subtrees
    Branch {
        left: SR::Share<GenericSyntaxTree<CI, SR>>,
        right: SR::Share<GenericSyntaxTree<CI, SR>>,
        free_variables: HashSet<SyntaxKey<CI>>,
        binding_variables: HashSet<SyntaxKey<CI>>,
    },
    /// or have no descendants
    Leaf(SyntaxLeaf),
}

impl<SR: SharedReference, CI: ConceptId> Debug for Node<SR, CI> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Branch {
                left,
                right,
                free_variables,
                binding_variables,
            } => f
                .debug_struct("Branch")
                .field("left", &left.as_ref())
                .field("right", &right.as_ref())
                .field("free_variables", free_variables)
                .field("binding_variables", binding_variables)
                .finish(),
            Self::Leaf(arg0) => f.debug_tuple("Leaf").field(arg0).finish(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SyntaxLeaf {
    Variable,
    Constant,
    Quantifier,
}

// the variables fields can be derived from left and right so no need to check them for equality
impl<CI: ConceptId, SR: SharedReference> PartialEq for Node<SR, CI> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Branch {
                    left,
                    right,
                    ..
                },
                Self::Branch {
                    left: other_left,
                    right: other_right,
                    ..
                },
            ) => {
                left.key() == other_left.key()
                    && right.key() == other_right.key()
            },
            (Self::Leaf(leaf), Self::Leaf(other_leaf)) => leaf == other_leaf,
            (
                Self::Leaf(_),
                Self::Branch {
                    ..
                },
            )
            | (
                Self::Branch {
                    ..
                },
                Self::Leaf(_),
            ) => false,
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> Node<SR, CI> {
    pub fn new_pair(
        left: SR::Share<GenericSyntaxTree<CI, SR>>,
        right: SR::Share<GenericSyntaxTree<CI, SR>>,
    ) -> Self {
        let mut free_variables = HashSet::<SyntaxKey<CI>>::new();
        let mut binding_variables = HashSet::<SyntaxKey<CI>>::new();
        let right_node: &Self = &right.node;
        let right_is_quantifier = match right_node {
            Self::Branch {
                free_variables: fv,
                binding_variables: bv,
                ..
            } => {
                free_variables.extend(fv.iter().cloned());
                binding_variables.extend(bv.iter().cloned());
                false
            },
            Self::Leaf(SyntaxLeaf::Variable) => {
                free_variables.insert(right.key());
                false
            },
            Self::Leaf(SyntaxLeaf::Constant) => false,
            Self::Leaf(SyntaxLeaf::Quantifier) => true,
        };
        let left_node: &Self = &left.node;
        match left_node {
            Self::Branch {
                free_variables: fv,
                binding_variables: bv,
                ..
            } => {
                free_variables.retain(|v| !bv.contains(v));
                free_variables.extend(fv.iter().cloned());
                binding_variables.retain(|v| !fv.contains(v));
                binding_variables.extend(bv.iter().cloned());
            },
            Self::Leaf(SyntaxLeaf::Variable) => {
                if right_is_quantifier {
                    binding_variables.insert(left.key());
                } else {
                    free_variables.insert(left.key());
                }
            },
            Self::Leaf(_) => {},
        }
        Self::Branch {
            left,
            right,
            binding_variables,
            free_variables,
        }
    }
}
