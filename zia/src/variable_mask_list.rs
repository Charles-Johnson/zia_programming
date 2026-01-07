#![allow(clippy::single_component_path_imports)]
use std::{collections::HashMap, fmt::Debug};

use crate::{
    context_cache::SharedSyntax, mixed_concept::ConceptId,
    nester::SharedReference, reduction_reason::compare_syntax_maps,
};

#[derive(Clone)]
pub struct VariableMaskList<CI: ConceptId, SR: SharedReference> {
    head: VariableMask<CI, SR>,
    tail: Option<SR::Share<Self>>,
}

impl<CI: ConceptId, SR: SharedReference> PartialEq
    for VariableMaskList<CI, SR>
{
    fn eq(&self, other: &Self) -> bool {
        compare_syntax_maps::<CI, CI, SR>(&self.head, &other.head)
            && self.tail.as_ref().map(std::convert::AsRef::as_ref)
                == other.tail.as_ref().map(std::convert::AsRef::as_ref)
    }
}
impl<CI: ConceptId, SR: SharedReference> Eq for VariableMaskList<CI, SR> {}
impl<CI: ConceptId, SR: SharedReference> Debug for VariableMaskList<CI, SR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tail = self.tail.as_ref().map(std::convert::AsRef::as_ref);
        f.debug_struct("GenericVariableMaskList")
            .field(
                "head",
                &self
                    .head
                    .iter()
                    .map(|(k, v)| (*k, v.as_ref()))
                    .collect::<HashMap<_, _>>(),
            )
            .field("tail", &tail)
            .finish()
    }
}
impl<CI: ConceptId, SR: SharedReference> From<VariableMask<CI, SR>>
    for VariableMaskList<CI, SR>
{
    fn from(head: VariableMask<CI, SR>) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> VariableMaskList<CI, SR> {
    /// returns None if `head` is equal to one of the nodes.
    /// This prevents cycles in reduction evaluations
    pub fn push(
        list: &SR::Share<Self>,
        head: VariableMask<CI, SR>,
    ) -> Option<Self> {
        (!list.as_ref().contains(&head)).then(|| Self {
            head,
            tail: Some(list.clone()),
        })
    }

    pub fn contains(&self, node: &VariableMask<CI, SR>) -> bool {
        compare_syntax_maps::<CI, CI, SR>(&self.head, node)
            || self.tail.as_ref().is_some_and(|vml| vml.as_ref().contains(node))
    }

    pub fn get(&self, concept_id: CI) -> Option<&SharedSyntax<CI, SR>> {
        self.head.get(&concept_id).or_else(|| {
            self.tail.as_ref().and_then(|vml| vml.as_ref().get(concept_id))
        })
    }

    pub const fn tail(&self) -> Option<&SR::Share<Self>> {
        self.tail.as_ref()
    }
}

pub type VariableMask<CI, SR> = HashMap<CI, SharedSyntax<CI, SR>>;
