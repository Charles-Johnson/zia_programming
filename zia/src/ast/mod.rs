//  Library for the Zia programming language.
// Copyright (C) 2018 to 2019 Charles Johnson
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Display},
    hash::{Hash, Hasher},
    ops::Deref,
    rc::Rc,
    sync::Arc,
};

use maplit::hashmap;

use crate::{and_also::AndAlso, consistent_merge::ConsistentMerge};

/// Represents syntax as a full binary tree and links syntax to concepts where possible.
macro_rules! define_syntax_tree {
    ($refcounter:tt, $syntax_tree:tt, $concept_id:ty) => {
        #[derive(Clone)]
        pub struct $syntax_tree {
            /// The root of this syntax tree, represented as a `String`.
            syntax: Option<String>,
            /// Index of the concept that the syntax may represent.
            concept: Option<$concept_id>,
            ///
            node: SyntaxNode<$refcounter<$syntax_tree>>,
        }

        impl Debug for $syntax_tree {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.syntax.as_ref().map_or(Ok(()), |s| f.write_str(s))
            }
        }

        impl PartialEq<Self> for $syntax_tree {
            /// `SyntaxTree`s are equal if the syntax they represent is the same.
            fn eq(&self, other: &Self) -> bool {
                if let (Some(ss), Some(os)) = (&self.syntax, &other.syntax) {
                    ss == os
                } else if let (Some(sc), Some(oc)) = (&self.concept, &other.concept) {
                    sc == oc
                } else {
                    self.node == other.node
                }
            }
        }

        impl PartialEq<$refcounter<Self>> for $syntax_tree {
            /// `SyntaxTree`s are equal if the syntax they represent is the same.
            fn eq(&self, other: &$refcounter<Self>) -> bool {
                if let (Some(ss), Some(os)) = (&self.syntax, &other.syntax) {
                    ss == os
                } else {
                    self.concept == other.concept
                }
            }
        }

        impl Eq for $syntax_tree {}

        impl Hash for $syntax_tree {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.syntax.hash(state);
                self.concept.hash(state);
            }
        }

        impl fmt::Display for $syntax_tree
        {
            /// Displays the same as the inside of an `SyntaxTree` variant.
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}",
                    self.syntax.clone().unwrap_or_else(|| self
                        .get_expansion()
                        .map_or_else(
                            || "".into(),
                            |(left, right)| left.to_string() + " " + &right.to_string()
                        ))
                )
            }
        }

        impl<S> From<S> for $syntax_tree
        where
            S: Into<String>,
        {
            fn from(syntax: S) -> Self {
                let syntax = syntax.into();
                let node = if is_variable(&syntax) {
                    SyntaxNode::Leaf(SyntaxLeaf::Variable)
                } else {
                    SyntaxNode::Leaf(SyntaxLeaf::Constant)
                };
                Self {
                    syntax: Some(syntax),
                    concept: None,
                    node,
                }
            }
        }

        impl<'a> From<&'a $refcounter<$syntax_tree>> for &'a SyntaxNode<$refcounter<$syntax_tree>> {
            fn from(syntax_tree: &'a $refcounter<$syntax_tree>) -> Self {
                &syntax_tree.node
            }
        }

        impl SyntaxTree for $syntax_tree {
            type SharedSyntax = $refcounter<Self>;
            type ConceptId = $concept_id;

            fn make_mut<'a>(refcounter: &'a mut Self::SharedSyntax) -> &'a mut Self {
                $refcounter::make_mut(refcounter)
            }

            fn share(self) -> Self::SharedSyntax {
                $refcounter::new(self)
            }

            fn is_leaf_variable(&self) -> bool {
                matches!(self.node, SyntaxNode::Leaf(SyntaxLeaf::Variable))
            }

            fn new_constant_concept(concept_id: Self::ConceptId) -> Self {
                Self {
                    syntax: None,
                    concept: Some(concept_id),
                    node: SyntaxNode::Leaf(SyntaxLeaf::Constant),
                }
            }

            fn new_quantifier_concept(concept_id: Self::ConceptId) -> Self {
                Self {
                    syntax: None,
                    concept: Some(concept_id),
                    node: SyntaxNode::Leaf(SyntaxLeaf::Quantifier),
                }
            }

            fn new_pair(left: Self::SharedSyntax, right: Self::SharedSyntax) -> Self {
                Self {
                    syntax: None,
                    concept: None,
                    node: SyntaxNode::new_pair(left, right),
                }
            }

            fn new_leaf_variable(concept_id: Self::ConceptId) -> Self {
                Self {
                    syntax: None,
                    concept: Some(concept_id),
                    node: SyntaxNode::Leaf(SyntaxLeaf::Variable),
                }
            }

            fn bind_nonquantifier_concept(mut self, concept: Self::ConceptId) -> Self {
                self.concept = Some(concept);
                self
            }

            fn bind_nonquantifier_concept_as_ref(&mut self, concept: Self::ConceptId) {
                self.concept = Some(concept);
            }

            fn bind_quantifier_concept(mut self, concept: Self::ConceptId) -> Self {
                debug_assert_eq!(self.node, SyntaxNode::Leaf(SyntaxLeaf::Constant));
                self.concept = Some(concept);
                self.node = SyntaxNode::Leaf(SyntaxLeaf::Quantifier);
                self
            }

            fn contains(&self, other: &Self) -> bool {
                if let Some((ref left, ref right)) = self.get_expansion() {
                    other == left
                        || other == right
                        || left.contains(other)
                        || right.contains(other)
                } else {
                    false
                }
            }

            /// An expression does have an expansion while a symbol does not.
            fn get_expansion(&self) -> Option<(Self::SharedSyntax, Self::SharedSyntax)> {
                if let SyntaxNode::Branch {
                    left,
                    right,
                    ..
                } = &self.node
                {
                    Some((left.clone(), right.clone()))
                } else {
                    None
                }
            }

            fn get_expansion_mut(&mut self) -> Option<(&mut Self, &mut Self)> {
                if let SyntaxNode::Branch {
                    left,
                    right,
                    ..
                } = &mut self.node
                {
                    Some(($refcounter::make_mut(left), $refcounter::make_mut(right)))
                } else {
                    None
                }
            }

            fn bind_pair(mut self, left: Self::SharedSyntax, right: Self::SharedSyntax) -> Self {
                self.node = SyntaxNode::new_pair(left, right);
                self
            }

            fn get_concept(&self) -> Option<Self::ConceptId> {
                self.concept
            }

            fn is_variable(&self) -> bool {
                match &self.node {
                    SyntaxNode::Branch {
                        free_variables,
                        binding_variables,
                        ..
                    } => !free_variables.is_empty() || !binding_variables.is_empty(),
                    SyntaxNode::Leaf(SyntaxLeaf::Variable) => true,
                    SyntaxNode::Leaf(_) => false,
                }
            }

            fn check_example(
                example: &Self::SharedSyntax,
                generalisation: &Self::SharedSyntax,
            ) -> Option<HashMap<Self::SharedSyntax, Self::SharedSyntax>> {
                match (example.get_expansion(), generalisation.get_expansion()) {
                    (
                        Some((left_example, right_example)),
                        Some((left_gen, right_gen)),
                    ) => Self::check_example(&left_example, &left_gen)
                        .and_also_move(Self::check_example(&right_example, &right_gen))
                        .and_then(|(left_vm, right_vm)| {
                            left_vm.consistent_merge(right_vm)
                        }),
                    (Some(_), None) => generalisation
                        .is_variable()
                        .then(|| hashmap! {generalisation.clone() => example.clone()}),
                    (None, Some(_)) => None,
                    (None, None) => generalisation
                        .is_variable()
                        .then(|| hashmap! {generalisation.clone() => example.clone()})
                        .or_else(|| (example == generalisation).then(|| hashmap! {})),
                }
            }
        }
    };
}

define_syntax_tree!(Arc, MultiThreadedSyntaxTree, usize);
define_syntax_tree!(Rc, SingleThreadedSyntaxTree, usize);

pub trait SyntaxTree
where
    Self: Clone
        + Debug
        + Display
        + Eq
        + for<'a> From<&'a str>
        + From<String>
        + for<'a> From<&'a String>
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

    fn new_constant_concept(concept_id: Self::ConceptId) -> Self;

    fn new_quantifier_concept(concept_id: Self::ConceptId) -> Self;

    fn new_pair(left: Self::SharedSyntax, right: Self::SharedSyntax) -> Self;

    fn new_leaf_variable(concept_id: Self::ConceptId) -> Self;

    fn bind_nonquantifier_concept(self, concept: Self::ConceptId) -> Self;

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

#[derive(Clone, Debug)]
enum SyntaxNode<SharedSyntax> {
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

#[derive(Clone, Copy, Debug, PartialEq)]
enum SyntaxLeaf {
    Variable,
    Constant,
    Quantifier,
}

// the variables fields can be derived from left and right so no need to check them for equality
impl<SharedSyntax: PartialEq> PartialEq for SyntaxNode<SharedSyntax> {
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

impl<SharedSyntax> SyntaxNode<SharedSyntax>
where
    SharedSyntax: Clone + Eq + Hash,
    for<'a> &'a Self: From<&'a SharedSyntax>,
{
    fn new_pair(left: SharedSyntax, right: SharedSyntax) -> Self {
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

#[derive(Clone, Debug)]
struct SyntaxExpansion<SharedSyntax> {
    left: SharedSyntax,
    right: SharedSyntax,
    /// The variables that aren't bound by any quantifiers
    free_variables: HashSet<SharedSyntax>,
    /// The variables that bind the equivalent free variable via quantifiers
    binding_variables: HashSet<SharedSyntax>,
}

pub fn is_variable(string: &str) -> bool {
    string.starts_with('_') && string.ends_with('_') && !string.contains(' ')
}
