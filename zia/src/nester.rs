use crate::{ast::SyntaxLeaf, mixed_concept::ConceptId};
use std::{borrow::Borrow, fmt::Debug, hash::Hash, ops::Deref};

#[derive(Eq, PartialEq)]
pub enum Node<NestedSyntax> {
    Parent {
        children: Vec<NestedSyntax>,
    },
    Leaf(SyntaxLeaf),
}

impl<NestedSyntax> Node<NestedSyntax>
where
    NestedSyntax: Clone + Eq + Hash,
    for<'a> &'a Self: From<&'a NestedSyntax>,
{
    pub fn new_parent(children: Vec<NestedSyntax>) -> Self {
        Self::Parent {
            children,
        }
    }
}

pub trait SharedReference: Debug + Clone + PartialEq + Eq {
    type Share<T>: AsRef<T> + Borrow<T> + Clone + Deref<Target = T>;
    fn share<T>(owned: T) -> Self::Share<T>;

    fn make_mut<T: Clone>(refcounter: &mut Self::Share<T>) -> &mut T;
}

#[derive(PartialEq, Eq)]
pub struct NestedSyntaxTree<CI: ConceptId, SR: SharedReference> {
    node: Node<SR::Share<Self>>,
    syntax: Option<String>,
    concept: Option<CI>,
}
