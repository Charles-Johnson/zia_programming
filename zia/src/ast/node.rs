use std::{collections::HashSet, fmt::Debug, hash::Hash};

#[derive(Clone, Debug)]
pub enum Node<SharedSyntax> {
    /// This syntax tree may branch to two subtrees
    Branch {
        left: SharedSyntax,
        right: SharedSyntax,
        free_variables: HashSet<SharedSyntax>,
        binding_variables: HashSet<SharedSyntax>,
    },
    /// or have no descendants
    Leaf(SyntaxLeaf),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SyntaxLeaf {
    Variable,
    Constant,
    Quantifier,
}

// the variables fields can be derived from left and right so no need to check them for equality
impl<SharedSyntax: PartialEq> PartialEq for Node<SharedSyntax> {
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
            ) => left == other_left && right == other_right,
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

impl<SharedSyntax> Node<SharedSyntax>
where
    SharedSyntax: Clone + Eq + Hash,
    for<'a> &'a Self: From<&'a SharedSyntax>,
{
    pub fn new_pair(left: SharedSyntax, right: SharedSyntax) -> Self {
        let mut free_variables = HashSet::<SharedSyntax>::new();
        let mut binding_variables = HashSet::<SharedSyntax>::new();
        let right_node: &Self = (&right).into();
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
                free_variables.insert(right.clone());
                false
            },
            Self::Leaf(SyntaxLeaf::Constant) => false,
            Self::Leaf(SyntaxLeaf::Quantifier) => true,
        };
        let left_node: &Self = (&left).into();
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
                    binding_variables.insert(left.clone());
                } else {
                    free_variables.insert(left.clone());
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
