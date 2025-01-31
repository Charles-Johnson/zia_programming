#![allow(clippy::single_component_path_imports)]
use std::collections::HashMap;
use std::fmt::Debug;

use crate::context_cache::SharedSyntax;
use crate::mixed_concept::ConceptId;
use crate::nester::SharedReference;
use crate::reduction_reason::convert_to_syntax_keys;

#[derive(Clone)]
pub struct GenericVariableMaskList<CI: ConceptId, SR: SharedReference> {
    head: VariableMask<CI, SR>,
    tail: Option<SR::Share<Self>>,
}

impl<CI: ConceptId, SR: SharedReference> PartialEq
    for GenericVariableMaskList<CI, SR>
{
    fn eq(&self, other: &Self) -> bool {
        convert_to_syntax_keys::<CI, CI, SR>(&self.head)
            == convert_to_syntax_keys::<CI, CI, SR>(&other.head)
            && self.tail.as_ref().map(std::convert::AsRef::as_ref)
                == other.tail.as_ref().map(std::convert::AsRef::as_ref)
    }
}
impl<CI: ConceptId, SR: SharedReference> Eq
    for GenericVariableMaskList<CI, SR>
{
}
impl<CI: ConceptId, SR: SharedReference> Debug
    for GenericVariableMaskList<CI, SR>
{
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
    for GenericVariableMaskList<CI, SR>
{
    fn from(head: VariableMask<CI, SR>) -> Self {
        Self {
            head,
            tail: None,
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> VariableMaskList<CI, SR>
    for GenericVariableMaskList<CI, SR>
{
    /// returns None if `head` is equal to one of the nodes.
    /// This prevents cycles in reduction evaluations
    fn push(
        list: &SR::Share<Self>,
        head: VariableMask<CI, SR>,
    ) -> Option<Self> {
        (!list.as_ref().contains(&head)).then(|| Self {
            head,
            tail: Some(list.clone()),
        })
    }

    fn contains(&self, node: &VariableMask<CI, SR>) -> bool {
        convert_to_syntax_keys::<CI, CI, SR>(&self.head)
            == convert_to_syntax_keys::<CI, CI, SR>(node)
            || self
                .tail
                .as_ref()
                .map_or(false, |vml| vml.as_ref().contains(node))
    }

    fn get(&self, concept_id: CI) -> Option<&SharedSyntax<CI, SR>> {
        self.head.get(&concept_id).or_else(|| {
            self.tail.as_ref().and_then(|vml| vml.as_ref().get(concept_id))
        })
    }

    fn tail(&self) -> Option<&SR::Share<Self>> {
        self.tail.as_ref()
    }
}

pub trait VariableMaskList<CI: ConceptId, SR: SharedReference>:
    Sized + From<VariableMask<CI, SR>>
{
    fn push(list: &SR::Share<Self>, head: VariableMask<CI, SR>)
        -> Option<Self>;

    fn contains(&self, node: &VariableMask<CI, SR>) -> bool;

    fn get(&self, concept_id: CI) -> Option<&SharedSyntax<CI, SR>>;

    fn tail(&self) -> Option<&SR::Share<Self>>;
}

pub type VariableMask<CI, SR> = HashMap<CI, SharedSyntax<CI, SR>>;
