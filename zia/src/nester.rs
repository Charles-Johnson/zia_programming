use crate::{ast::SyntaxLeaf, mixed_concept::ConceptId, ConceptKind};
use std::{borrow::Borrow, fmt::Debug, ops::Deref};

#[derive(Eq, PartialEq)]
pub enum Node<NestedSyntax> {
    Parent {
        children: Vec<NestedSyntax>,
    },
    Leaf(SyntaxLeaf),
}

pub trait SharedReference: Debug + Clone + PartialEq + Eq {
    type Share<T>: AsRef<T> + Borrow<T> + Clone + Deref<Target = T>;
    fn share<T>(owned: T) -> Self::Share<T>;

    fn make_mut<T: Clone>(refcounter: &mut Self::Share<T>) -> &mut T;
}

#[derive(PartialEq, Eq)]
pub struct NestedSyntaxTree<CI: ConceptId, SR: SharedReference> {
    pub node: Node<SR::Share<Self>>,
    pub syntax: String,
    pub concept: Option<CI>,
}
impl<CI: ConceptId, SR: SharedReference> NestedSyntaxTree<CI, SR> {
    pub const fn from_concept_kind(
        c: &ConceptKind<CI>,
        syntax: String,
    ) -> Self {
        Self {
            node: Node::Leaf(match c {
                ConceptKind::Variable => SyntaxLeaf::Variable, /* TODO: determine */
                // if whether
                // concept is
                // quantifier
                ConceptKind::Abstract {
                    ..
                }
                | ConceptKind::Concrete {
                    ..
                }
                | ConceptKind::New => SyntaxLeaf::Constant,
            }),
            syntax,
            concept: match c {
                ConceptKind::New | ConceptKind::Variable => None,
                ConceptKind::Concrete {
                    id,
                }
                | ConceptKind::Abstract {
                    id,
                } => Some(*id),
            },
        }
    }

    pub fn append_node(self, new_nested_syntax: Self) -> Self {
        let share_nested_syntax = SR::share(new_nested_syntax);
        let shared_self = SR::share(self);
        Self {
            node: Node::Parent {
                children: match &shared_self.node {
                    Node::Leaf(_) => {
                        vec![shared_self.clone(), share_nested_syntax.clone()]
                    },
                    Node::Parent {
                        children,
                    } => {
                        let mut new_children = children.clone();
                        new_children.push(share_nested_syntax.clone());
                        new_children
                    },
                },
            },
            syntax: format!(
                "{} {}",
                shared_self.syntax, share_nested_syntax.syntax
            ),
            concept: None, /* TODO: consider trying to identify a concept
                            * from pair */
        }
    }
}
