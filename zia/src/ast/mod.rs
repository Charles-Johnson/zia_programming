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
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    sync::Arc,
};

use maplit::hashmap;

use crate::{and_also::AndAlso, consistent_merge::ConsistentMerge};

/// Represents syntax as a full binary tree and links syntax to concepts where possible.
#[derive(Clone)]
pub struct SyntaxTree {
    /// The root of this syntax tree, represented as a `String`.
    syntax: Option<String>,
    /// Index of the concept that the syntax may represent.
    concept: Option<usize>,
    ///
    node: SyntaxNode,
}

impl Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.syntax.as_ref().map_or(Ok(()), |s| f.write_str(s))
    }
}

#[derive(Clone, Debug)]
enum SyntaxNode {
    /// This syntax tree may branch to two subtrees
    Branch {
        left: Arc<SyntaxTree>,
        right: Arc<SyntaxTree>,
        free_variables: HashSet<Arc<SyntaxTree>>,
        binding_variables: HashSet<Arc<SyntaxTree>>,
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

#[derive(Clone, Debug)]
struct SyntaxExpansion {
    left: Arc<SyntaxTree>,
    right: Arc<SyntaxTree>,
    /// The variables that aren't bound by any quantifiers
    free_variables: HashSet<Arc<SyntaxTree>>,
    /// The variables that bind the equivalent free variable via quantifiers
    binding_variables: HashSet<Arc<SyntaxTree>>,
}

impl SyntaxNode {
    fn new_pair(
        left: impl Into<Arc<SyntaxTree>>,
        right: impl Into<Arc<SyntaxTree>>,
    ) -> Self {
        let left: Arc<_> = left.into();
        let right: Arc<_> = right.into();
        let mut free_variables = HashSet::<Arc<SyntaxTree>>::new();
        let mut binding_variables = HashSet::<Arc<SyntaxTree>>::new();
        let right_is_quantifier = match &right.node {
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
        match &left.node {
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

// the variables fields can be derived from left and right so no need to check them for equality
impl PartialEq for SyntaxNode {
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

impl PartialEq<SyntaxTree> for SyntaxTree {
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

impl PartialEq<Arc<SyntaxTree>> for SyntaxTree {
    /// `SyntaxTree`s are equal if the syntax they represent is the same.
    fn eq(&self, other: &Arc<Self>) -> bool {
        if let (Some(ss), Some(os)) = (&self.syntax, &other.syntax) {
            ss == os
        } else {
            self.concept == other.concept
        }
    }
}

impl fmt::Display for SyntaxTree {
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

impl<S> From<S> for SyntaxTree
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

impl Eq for SyntaxTree {}

impl Hash for SyntaxTree {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.syntax.hash(state);
        self.concept.hash(state);
    }
}

impl SyntaxTree {
    pub const fn new_constant_concept(concept_id: usize) -> Self {
        Self {
            syntax: None,
            concept: Some(concept_id),
            node: SyntaxNode::Leaf(SyntaxLeaf::Constant),
        }
    }

    pub const fn new_quantifier_concept(concept_id: usize) -> Self {
        Self {
            syntax: None,
            concept: Some(concept_id),
            node: SyntaxNode::Leaf(SyntaxLeaf::Quantifier),
        }
    }

    #[cfg(test)]
    pub fn new_pair(
        left: impl Into<Arc<SyntaxTree>>,
        right: impl Into<Arc<SyntaxTree>>,
    ) -> Self {
        Self {
            syntax: None,
            concept: None,
            node: SyntaxNode::new_pair(left, right),
        }
    }

    pub const fn bind_nonquantifier_concept(mut self, concept: usize) -> Self {
        self.concept = Some(concept);
        self
    }

    pub fn bind_quantifier_concept(mut self, concept: usize) -> Self {
        debug_assert_eq!(self.node, SyntaxNode::Leaf(SyntaxLeaf::Constant));
        self.concept = Some(concept);
        self.node = SyntaxNode::Leaf(SyntaxLeaf::Quantifier);
        self
    }

    pub fn contains(&self, other: &Self) -> bool {
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
    pub fn get_expansion(&self) -> Option<(Arc<Self>, Arc<Self>)> {
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

    pub fn bind_pair(
        mut self,
        left: impl Into<Arc<Self>>,
        right: impl Into<Arc<Self>>,
    ) -> Self {
        self.node = SyntaxNode::new_pair(left, right);
        self
    }

    pub const fn get_concept(&self) -> Option<usize> {
        self.concept
    }

    pub fn is_variable(&self) -> bool {
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

    pub fn check_example(
        self: &Arc<Self>,
        generalisation: &Arc<Self>,
    ) -> Option<HashMap<Arc<Self>, Arc<Self>>> {
        match (self.get_expansion(), generalisation.get_expansion()) {
            (
                Some((left_example, right_example)),
                Some((left_gen, right_gen)),
            ) => left_example.check_example(&left_gen)
                .and_also_move(right_example.check_example(&right_gen))
                .and_then(|(left_vm, right_vm)| {
                    left_vm.consistent_merge(right_vm)
                }),
            (Some(_), None) => generalisation
                .is_variable()
                .then(|| hashmap! {generalisation.clone() => self.clone()}),
            (None, Some(_)) => None,
            (None, None) => generalisation
                .is_variable()
                .then(|| hashmap! {generalisation.clone() => self.clone()})
                .or_else(|| (self == generalisation).then(|| hashmap! {})),
        }
    }
}

pub fn is_variable(string: &str) -> bool {
    string.starts_with('_') && string.ends_with('_') && !string.contains(' ')
}
