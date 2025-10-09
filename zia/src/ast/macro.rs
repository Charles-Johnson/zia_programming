#![allow(clippy::single_component_path_imports)]

use crate::{
    and_also::AndAlso,
    ast::{is_variable, SyntaxLeaf, SyntaxNode},
    consistent_merge::ConsistentMerge,
    mixed_concept::ConceptId,
    nester::SharedReference,
};
use maplit::hashmap;
use std::{
    fmt,
    hash::{Hash, Hasher},
};
#[derive(Clone)]
pub struct GenericSyntaxTree<CI: ConceptId, SR: SharedReference> {
    /// The root of this syntax tree, represented as a `String`.
    pub syntax: Option<String>,
    /// Index of the concept that the syntax may represent.
    concept: Option<CI>,
    pub node: SyntaxNode<SR, CI>,
}

impl<CI: ConceptId, SR: SharedReference> std::fmt::Debug
    for GenericSyntaxTree<CI, SR>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GenericSyntaxTree")
            .field("syntax", &self.syntax)
            .field("concept", &self.concept)
            .field("node", &self.node)
            .finish()
    }
}

impl<CI: ConceptId, SR: SharedReference> GenericSyntaxTree<CI, SR> {
    pub fn key(&self) -> SyntaxKey<CI> {
        SyntaxKey {
            syntax: self.syntax.clone(),
            concept: self.concept,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SyntaxKey<CI: ConceptId> {
    syntax: Option<String>,
    concept: Option<CI>,
}

impl<CI: ConceptId, SR: SharedReference> PartialEq<Self>
    for GenericSyntaxTree<CI, SR>
{
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

impl<CI: ConceptId, SR: SharedReference> Eq for GenericSyntaxTree<CI, SR> {}

impl<CI: ConceptId, SR: SharedReference> Hash for GenericSyntaxTree<CI, SR> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.syntax.hash(state);
        self.concept.hash(state);
    }
}

impl<CI: ConceptId, SR: SharedReference> fmt::Display
    for GenericSyntaxTree<CI, SR>
{
    /// Displays the same as the inside of an `SyntaxTree` variant.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.syntax.clone().unwrap_or_else(|| self
                .get_expansion()
                .map_or_else(
                    || panic!(
                        "Tried to display syntax ({self:?}) without symbols"
                    ),
                    |(left, right)| left.to_string() + " " + &right.to_string()
                ))
        )
    }
}

impl<S, CI: ConceptId, SR: SharedReference> From<S>
    for GenericSyntaxTree<CI, SR>
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

use crate::ast::ExampleSubstitutions;

impl<CI: ConceptId, SR: SharedReference> GenericSyntaxTree<CI, SR> {
    pub fn make_mut(refcounter: &mut SR::Share<Self>) -> &mut Self {
        SR::make_mut(refcounter)
    }

    pub fn share(self) -> SR::Share<Self> {
        SR::share(self)
    }

    pub const fn is_leaf_variable(&self) -> bool {
        matches!(self.node, SyntaxNode::Leaf(SyntaxLeaf::Variable))
    }

    pub fn new_constant_concept(concept_id: impl Into<CI>) -> Self {
        let concept_id = concept_id.into();
        Self {
            syntax: Some(format!("constant {concept_id}")),
            concept: Some(concept_id),
            node: SyntaxNode::Leaf(SyntaxLeaf::Constant),
        }
    }

    pub fn new_quantifier_concept(concept_id: CI) -> Self {
        Self {
            syntax: Some(format!("quantifier {concept_id}")),
            concept: Some(concept_id),
            node: SyntaxNode::Leaf(SyntaxLeaf::Quantifier),
        }
    }

    pub fn new_pair(left: SR::Share<Self>, right: SR::Share<Self>) -> Self {
        Self {
            syntax: None,
            concept: None,
            node: SyntaxNode::new_pair(left, right),
        }
    }

    pub fn new_leaf_variable(concept_id: CI) -> Self {
        Self {
            syntax: Some(format!("variable {concept_id}")),
            concept: Some(concept_id),
            node: SyntaxNode::Leaf(SyntaxLeaf::Variable),
        }
    }

    pub fn bind_nonquantifier_concept(
        mut self,
        concept: impl Into<CI>,
    ) -> Self {
        self.concept = Some(concept.into());
        self
    }

    pub fn bind_nonquantifier_concept_as_ref(&mut self, concept: CI) {
        self.concept = Some(concept);
    }

    pub fn bind_quantifier_concept(mut self, concept: CI) -> Self {
        debug_assert_eq!(self.node, SyntaxNode::Leaf(SyntaxLeaf::Constant));
        self.concept = Some(concept);
        self.node = SyntaxNode::Leaf(SyntaxLeaf::Quantifier);
        self
    }

    pub fn contains(&self, other: &Self) -> bool {
        if let Some((ref left, ref right)) = self.get_expansion() {
            other.key() == left.key()
                || other.key() == right.key()
                || left.contains(other)
                || right.contains(other)
        } else {
            false
        }
    }

    /// An expression does have an expansion while a symbol does not.
    pub fn get_expansion(&self) -> Option<(SR::Share<Self>, SR::Share<Self>)> {
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

    pub fn get_expansion_mut(&mut self) -> Option<(&mut Self, &mut Self)> {
        if let SyntaxNode::Branch {
            left,
            right,
            ..
        } = &mut self.node
        {
            Some((SR::make_mut(left), SR::make_mut(right)))
        } else {
            None
        }
    }

    pub fn bind_pair(
        mut self,
        left: SR::Share<Self>,
        right: SR::Share<Self>,
    ) -> Self {
        self.node = SyntaxNode::new_pair(left, right);
        self
    }

    pub const fn get_concept(&self) -> Option<CI> {
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
        example: &SR::Share<Self>,
        generalisation: &SR::Share<Self>,
    ) -> Option<ExampleSubstitutions<CI, SR>> {
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
                            generalisation: hashmap! {generalisation.key() => example.clone()},
                            example: hashmap!{}
                        }),
                    (None, Some(_)) => None,
                    (None, None) => generalisation
                        .is_variable()
                        .then(|| ExampleSubstitutions{
                            generalisation: hashmap! {generalisation.key() => example.clone()},
                            example: hashmap!{}
                        })
                        .or_else(|| example.get_concept().and_then(|id| example.is_variable().then(|| ExampleSubstitutions{
                            generalisation: hashmap! {},
                            example: hashmap!{id => generalisation.clone()}
                        })))
                        .or_else(|| (example.key() == generalisation.key()).then(ExampleSubstitutions::default)),
                }
    }
}
