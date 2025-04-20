use crate::{
    ast::SyntaxLeaf, concepts::ConcreteConceptType, mixed_concept::ConceptId,
    ConceptKind,
};
use std::{borrow::Borrow, fmt::Debug, marker::PhantomData, ops::Deref};

pub enum Node<NestedSyntax, SR: SharedReference> {
    Parent {
        children: Vec<SR::Share<NestedSyntax>>,
    },
    Leaf(SyntaxLeaf),
}

impl<NestedSyntax: PartialEq, SR: SharedReference> PartialEq
    for Node<NestedSyntax, SR>
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Parent {
                    children: l_children,
                },
                Self::Parent {
                    children: r_children,
                },
            ) => l_children
                .iter()
                .zip(r_children.iter())
                .all(|(l, r)| l.as_ref() == r.as_ref()),
            (Self::Leaf(l0), Self::Leaf(r0)) => l0 == r0,
            _ => false,
        }
    }
}
impl<NestedSyntax: Debug, SR: SharedReference> Debug
    for Node<NestedSyntax, SR>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parent {
                children,
            } => f
                .debug_struct("Parent")
                .field(
                    "children",
                    &children
                        .iter()
                        .map(std::convert::AsRef::as_ref)
                        .collect::<Vec<_>>(),
                )
                .finish(),
            Self::Leaf(arg0) => f.debug_tuple("Leaf").field(arg0).finish(),
        }
    }
}
impl<NestedSyntax: PartialEq, SR: SharedReference> Eq
    for Node<NestedSyntax, SR>
{
}
pub trait SharedReference: Debug + Clone + PartialEq + Eq {
    type Share<T>: AsRef<T> + Borrow<T> + Clone + Deref<Target = T>;
    fn share<T>(owned: T) -> Self::Share<T>;

    fn make_mut<T: Clone>(refcounter: &mut Self::Share<T>) -> &mut T;
}

#[derive(PartialEq, Eq)]
pub struct NestedSyntaxTree<CI: ConceptId, SR: SharedReference> {
    pub node: Node<Self, SR>,
    pub syntax: String,
    pub concept: Option<CI>,
    pub _phantom: PhantomData<SR>,
}
impl<CI: ConceptId, SR: SharedReference> Debug for NestedSyntaxTree<CI, SR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NestedSyntaxTree")
            .field("node", &self.node)
            .field("syntax", &self.syntax)
            .field("concept", &self.concept)
            .finish()
    }
}
impl<CI: ConceptId, SR: SharedReference> NestedSyntaxTree<CI, SR> {
    pub fn from_concept_kind(c: &ConceptKind<CI>, syntax: String) -> Self {
        Self {
            node: Node::Leaf(match c {
                ConceptKind::Variable => SyntaxLeaf::Variable,
                ConceptKind::Concrete {
                    concrete_type,
                    ..
                } if concrete_type == &ConcreteConceptType::ExistsSuchThat => {
                    SyntaxLeaf::Quantifier
                },
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
                    ..
                }
                | ConceptKind::Abstract {
                    id,
                } => Some(*id),
            },
            _phantom: PhantomData,
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
            _phantom: PhantomData,
        }
    }
}
