#![allow(clippy::single_component_path_imports)]

use std::{collections::HashMap, ops::Deref};

use crate::ast::SyntaxTree;

macro_rules! impl_variable_mask_list {
    ($refcounter:tt, $vml:tt) => {
        use crate::variable_mask_list::{VariableMask, VariableMaskList};
        #[derive(Clone, PartialEq, Debug)]
        pub struct $vml<Syntax: SyntaxTree> {
            head: VariableMask<Syntax>,
            tail: Option<$refcounter<$vml<Syntax>>>,
        }
        impl<Syntax: SyntaxTree> From<VariableMask<Syntax>> for $vml<Syntax> {
            fn from(head: VariableMask<Syntax>) -> Self {
                Self {
                    head,
                    tail: None,
                }
            }
        }

        impl<Syntax: SyntaxTree> VariableMaskList for $vml<Syntax> {
            type Shared = $refcounter<$vml<Syntax>>;
            type Syntax = Syntax;

            /// returns None if `head` is equal to one of the nodes.
            /// This prevents cycles in reduction evaluations
            fn push(
                list: &$refcounter<Self>,
                head: VariableMask<Syntax>,
            ) -> Option<Self> {
                (!list.contains(&head)).then(|| Self {
                    head,
                    tail: Some(list.clone()),
                })
            }

            fn contains(&self, node: &VariableMask<Syntax>) -> bool {
                &self.head == node
                    || self
                        .tail
                        .as_ref()
                        .map_or(false, |vml| vml.contains(node))
            }

            fn get(
                &self,
                concept_id: Syntax::ConceptId,
            ) -> Option<&Syntax::SharedSyntax> {
                self.head.get(&concept_id).or_else(|| {
                    self.tail.as_ref().and_then(|vml| vml.get(concept_id))
                })
            }

            fn tail(&self) -> Option<&$refcounter<Self>> {
                self.tail.as_ref()
            }
        }
    };
}

pub(crate) use impl_variable_mask_list;

pub trait VariableMaskList: Sized + From<VariableMask<Self::Syntax>> {
    type Shared: Clone + Deref<Target = Self> + From<Self>;
    type Syntax: SyntaxTree;
    fn push(
        list: &Self::Shared,
        head: VariableMask<Self::Syntax>,
    ) -> Option<Self>;

    fn contains(&self, node: &VariableMask<Self::Syntax>) -> bool;

    fn get(
        &self,
        concept_id: <Self::Syntax as SyntaxTree>::ConceptId,
    ) -> Option<&<Self::Syntax as SyntaxTree>::SharedSyntax>;

    fn tail(&self) -> Option<&Self::Shared>;
}

pub type VariableMask<Syntax> = HashMap<
    <Syntax as SyntaxTree>::ConceptId,
    <Syntax as SyntaxTree>::SharedSyntax,
>;
