#![allow(clippy::single_component_path_imports)]

macro_rules! impl_syntax_tree {
    ($refcounter:tt, $syntax_tree:tt) => {
        use maplit::hashmap;
        use std::{
            collections::HashMap,
            fmt,
            hash::{Hash, Hasher}
        };
        use crate::{
            and_also::AndAlso,
            ast::{is_variable, SyntaxLeaf, SyntaxNode},
            consistent_merge::ConsistentMerge
        };
        #[derive(Clone, Debug)]
        pub struct $syntax_tree<ConceptId> {
            /// The root of this syntax tree, represented as a `String`.
            syntax: Option<String>,
            /// Index of the concept that the syntax may represent.
            concept: Option<ConceptId>,
            ///
            node: SyntaxNode<$refcounter<Self>>,
        }

        impl<ConceptId: PartialEq> PartialEq<Self> for $syntax_tree<ConceptId> {
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

        impl<ConceptId: PartialEq> PartialEq<$refcounter<Self>> for $syntax_tree<ConceptId> {
            /// `SyntaxTree`s are equal if the syntax they represent is the same.
            fn eq(&self, other: &$refcounter<Self>) -> bool {
                if let (Some(ss), Some(os)) = (&self.syntax, &other.syntax) {
                    ss == os
                } else {
                    self.concept == other.concept
                }
            }
        }

        impl<ConceptId: PartialEq> Eq for $syntax_tree<ConceptId> {}

        impl<ConceptId: Hash> Hash for $syntax_tree<ConceptId> {
            fn hash<H: Hasher>(&self, state: &mut H) {
                self.syntax.hash(state);
                self.concept.hash(state);
            }
        }

        impl<ConceptId: Copy + Debug + fmt::Display + Eq + Hash> fmt::Display for $syntax_tree<ConceptId>
        {
            /// Displays the same as the inside of an `SyntaxTree` variant.
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(
                    f,
                    "{}",
                    self.syntax.clone().unwrap_or_else(|| self
                        .get_expansion()
                        .map(
                            |(left, right)| left.to_string() + " " + &right.to_string()
                        )
                        .expect(&format!("Tried to display syntax ({:?}) without symbols", self))
                    )
                )
            }
        }

        impl<S, ConceptId> From<S> for $syntax_tree<ConceptId>
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

        impl<'a, ConceptId> From<&'a $refcounter<$syntax_tree<ConceptId>>> for &'a SyntaxNode<$refcounter<$syntax_tree<ConceptId>>> {
            fn from(syntax_tree: &'a $refcounter<$syntax_tree<ConceptId>>) -> Self {
                &syntax_tree.node
            }
        }
        use crate::ast::ExampleSubstitutions;
        impl<ConceptId: Clone + Copy + Debug + fmt::Display + Eq + Hash> SyntaxTree for $syntax_tree<ConceptId> {
            type SharedSyntax = $refcounter<Self>;
            type ConceptId = ConceptId;

            fn make_mut<'a>(refcounter: &'a mut Self::SharedSyntax) -> &'a mut Self {
                $refcounter::make_mut(refcounter)
            }

            fn share(self) -> Self::SharedSyntax {
                $refcounter::new(self)
            }

            fn is_leaf_variable(&self) -> bool {
                matches!(self.node, SyntaxNode::Leaf(SyntaxLeaf::Variable))
            }

            fn new_constant_concept(concept_id: impl Into<Self::ConceptId>) -> Self {
                let concept_id = concept_id.into();
                Self {
                    syntax: Some(format!("constant {concept_id}")),
                    concept: Some(concept_id),
                    node: SyntaxNode::Leaf(SyntaxLeaf::Constant),
                }
            }

            fn new_quantifier_concept(concept_id: Self::ConceptId) -> Self {
                Self {
                    syntax: Some(format!("quantifier {concept_id}")),
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
                    syntax: Some(format!("variable {concept_id}")),
                    concept: Some(concept_id),
                    node: SyntaxNode::Leaf(SyntaxLeaf::Variable),
                }
            }

            fn bind_nonquantifier_concept(mut self, concept: impl Into<Self::ConceptId>) -> Self {
                self.concept = Some(concept.into());
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
            ) -> Option<ExampleSubstitutions<Self>> {
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
                        .then(|| ExampleSubstitutions{
                            generalisation: hashmap! {generalisation.clone() => example.clone()},
                            example: hashmap!{}
                        }),
                    (None, Some(_)) => None,
                    (None, None) => generalisation
                        .is_variable()
                        .then(|| ExampleSubstitutions{
                            generalisation: hashmap! {generalisation.clone() => example.clone()},
                            example: hashmap!{}
                        })
                        .or_else(|| example.get_concept().and_then(|id| example.is_variable().then(|| ExampleSubstitutions{
                            generalisation: hashmap! {},
                            example: hashmap!{id => generalisation.clone()}
                        })))
                        .or_else(|| (example == generalisation).then(|| ExampleSubstitutions::default())),
                }
            }
        }
    };
}

pub(crate) use impl_syntax_tree;
