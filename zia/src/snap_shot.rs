use crate::{
    ast::SyntaxTree,
    concepts::{ConceptTrait, ConcreteConceptType, Hand},
    context_delta::{
        Composition, ConceptDelta, DirectConceptDelta,
        NewDirectConceptDelta, ValueChange, NestedContextDelta, SharedDelta,
    },
    errors::{ZiaError, ZiaResult},
    mixed_concept::MixedConcept,
};
use std::{
    convert::TryFrom,
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Reader<SDCD>
where
    SDCD: Clone
        + AsRef<DirectConceptDelta<Self::ConceptId>>
        + From<DirectConceptDelta<Self::ConceptId>>
        + Debug,
    Self::CommittedConceptId: TryFrom<Self::ConceptId>,
    Self::ConceptId: From<Self::CommittedConceptId>,
{
    type ConceptId: MixedConcept
        + Send
        + Sync;
    type CommittedConceptId: Copy + Eq + Hash + Display + Debug + Send + Sync;
    type MixedConcept<'a>: ConceptTrait<Id = Self::ConceptId>
        + Clone
        + Debug
        + for<'b> From<
            &'b NewDirectConceptDelta<Self::ConceptId, Self::ConceptId>,
        >
    where
        Self: 'a;
    fn get_concept(
        &self,
        concept_id: Self::ConceptId,
    ) -> Option<Self::MixedConcept<'_>>;
    fn read_concept<'a, D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &'a self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        id: Self::ConceptId,
    ) -> Self::MixedConcept<'a> {
        delta
            .get_concept(&id)
            .and_then(|cds| {
                let mut concept = self.get_concept(id);
                for cd in cds {
                    match cd {
                        ConceptDelta::Direct(dcd) => match dcd.as_ref() {
                            DirectConceptDelta::New(delta) => {
                                debug_assert!(concept.is_none());
                                concept = Some((&NewDirectConceptDelta{
                                    delta: delta.clone(),
                                    new_concept_id: id
                                }).into());
                            },
                            DirectConceptDelta::Remove(concept_id) => {
                                debug_assert_eq!(*concept_id, id);
                                debug_assert!(concept.is_some());
                                concept = None;
                            },
                            DirectConceptDelta::Compose {
                                change,
                                composition_id,
                            } => {
                                debug_assert_eq!(*composition_id, id);
                                match change {
                                    ValueChange::Create(comp) => {
                                        let [mut left, mut right] = self.concepts_from_composition(delta, *comp);
                                        concept
                                            .as_mut()
                                            .unwrap()
                                            .change_composition(ValueChange::Create(
                                                [&mut left, &mut right],
                                            ))
                                            .unwrap();
                                    },
                                    ValueChange::Update {
                                        before,
                                        after,
                                    } => {
                                        let [mut before_left, mut before_right] = self.concepts_from_composition(delta, *before);
                                        let [mut after_left, mut after_right] = self.concepts_from_composition(delta, *after);
                                        concept
                                            .as_mut()
                                            .unwrap()
                                            .change_composition(
                                                ValueChange::Update {
                                                    before: [
                                                        &mut before_left,
                                                        &mut before_right,
                                                    ],
                                                    after: [
                                                        &mut after_left,
                                                        &mut after_right,
                                                    ],
                                                },
                                            )
                                            .unwrap();
                                    },
                                    ValueChange::Remove(comp) => {
                                        let [mut left, mut right] = self.concepts_from_composition(delta, *comp);
                                        concept
                                            .as_mut()
                                            .unwrap()
                                            .change_composition(ValueChange::Remove(
                                                [&mut left, &mut right],
                                            ))
                                            .unwrap();
                                    },
                                };
                            },
                            DirectConceptDelta::Reduce {
                                change,
                                unreduced_id,
                            } => {
                                debug_assert_eq!(*unreduced_id, id);
                                concept
                                    .as_mut()
                                    .expect("concept must already exist")
                                    .change_reduction(*change);
                            },
                        },
                        ConceptDelta::Indirect(delta) => {
                            let c = concept.as_mut().expect("Concept doesn't exist");
                            c.apply_indirect(delta);
                        },
                    }
                }
                concept
            })
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .unwrap_or_else(|| panic!("No concept with id = {}", id))
            })
    }
    fn concepts_from_composition<'a, D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &'a self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        comp: Composition<Self::ConceptId>,
    ) -> [Self::MixedConcept<'a>; 2] {
        [
            self.read_concept(delta, comp.left_id),
            self.read_concept(delta, comp.right_id),
        ]
    }
    fn get_label<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        concept_id: Self::ConceptId,
    ) -> Option<String>;
    fn ast_from_symbol<Syntax, D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        s: &str,
    ) -> Syntax
    where
        Syntax: SyntaxTree<ConceptId = Self::ConceptId>,
    {
        self.concept_from_label(delta, s).map_or_else(
            || s.into(),
            |concept| {
                let syntax = Syntax::from(s);
                self.bind_concept_to_syntax(delta, syntax, concept)
            },
        )
    }
    fn bind_concept_to_syntax<Syntax, D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        syntax: Syntax,
        concept: Self::ConceptId,
    ) -> Syntax
    where
        Syntax: SyntaxTree<ConceptId = Self::ConceptId>,
    {
        if self.concrete_concept_type(delta, concept)
            == Some(ConcreteConceptType::ExistsSuchThat)
        {
            syntax.bind_quantifier_concept(concept)
        } else {
            syntax.bind_nonquantifier_concept(concept)
        }
    }
    fn new_syntax_from_concept_that_has_no_label_or_composition<'a, Syntax>(
        &'a self,
        concept: &Self::MixedConcept<'a>,
    ) -> Syntax
    where
        Syntax: SyntaxTree<ConceptId = Self::ConceptId>,
    {
        let quantifier = concept.get_concrete_concept_type()
            == Some(ConcreteConceptType::ExistsSuchThat);
        if quantifier {
            SyntaxTree::new_quantifier_concept(concept.id())
        } else if concept.anonymous_variable() {
            SyntaxTree::new_leaf_variable(concept.id())
        } else {
            SyntaxTree::new_constant_concept(concept.id())
        }
    }
    fn concept_from_label<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        s: &str,
    ) -> Option<Self::ConceptId>;
    fn get_reduction_of_composition<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        concept: Self::ConceptId,
    ) -> Self::ConceptId {
        self.read_concept(delta, concept)
            .get_composition()
            .and_then(|(left, right)| {
                self.get_reduction_or_reduction_of_composition(delta, left)
                    .find_as_hand_in_composition_with(
                        self.get_reduction_or_reduction_of_composition(
                            delta, right,
                        )
                        .id(),
                        Hand::Left,
                    )
            })
            .unwrap_or(concept)
    }
    fn is_disconnected<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        concept: Self::ConceptId,
    ) -> bool {
        self.read_concept(delta, concept).get_reduction().is_none()
            && self.read_concept(delta, concept).get_composition().is_none()
            && self
                .read_concept(delta, concept)
                .iter_hand_of(Hand::Left)
                .next()
                .is_none()
            && self.righthand_of_without_label_is_empty(delta, concept)
            && self
                .read_concept(delta, concept)
                .find_what_reduces_to_it()
                .next()
                .is_none()
    }
    fn righthand_of_without_label_is_empty<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        con: Self::ConceptId,
    ) -> bool {
        self.concrete_concept_id(delta, ConcreteConceptType::Label)
            .and_then(|label_id| {
                self.read_concept(delta, con)
                    .find_as_hand_in_composition_with(label_id, Hand::Right)
            })
            .is_none()
    }
    fn get_normal_form<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        concept: Self::ConceptId,
    ) -> Option<Self::ConceptId> {
        self.read_concept(delta, concept)
            .get_reduction()
            .map(|n| self.get_normal_form(delta, n).unwrap_or(n))
    }
    fn get_concept_of_label<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        concept: Self::ConceptId,
    ) -> Option<Self::ConceptId> {
        let label_concept_id =
            self.concrete_concept_id(delta, ConcreteConceptType::Label)?;

        self.read_concept(delta, concept)
            .find_as_hand_in_composition_with(label_concept_id, Hand::Right)
    }
    fn contains<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        outer: Self::ConceptId,
        inner: Self::ConceptId,
    ) -> bool {
        if let Some((left, right)) =
            self.read_concept(delta, outer).get_composition()
        {
            left == inner
                || right == inner
                || self.contains(delta, left, inner)
                || self.contains(delta, right, inner)
        } else {
            false
        }
    }
    fn check_reductions<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        outer_concept: Self::ConceptId,
        inner_concept: Self::ConceptId,
    ) -> ZiaResult<()> {
        self.read_concept(delta, inner_concept).get_reduction().map_or(
            Ok(()),
            |r| {
                if r == outer_concept || self.contains(delta, r, outer_concept)
                {
                    Err(ZiaError::InfiniteComposition)
                } else {
                    self.check_reductions(delta, outer_concept, r)
                }
            },
        )
    }
    fn get_reduction_or_reduction_of_composition<'a, D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &'a self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        concept: Self::ConceptId,
    ) -> Self::MixedConcept<'a> {
        self.read_concept(
            delta,
            self.read_concept(delta, concept).get_reduction().unwrap_or_else(
                || self.get_reduction_of_composition(delta, concept),
            ),
        )
    }
    fn concrete_concept_id<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        cc: ConcreteConceptType,
    ) -> Option<Self::ConceptId>;
    fn concrete_concept_type<D: SharedDelta<NestedDelta = NestedContextDelta<Self::ConceptId, SDCD, D>>>(
        &self,
        delta: &NestedContextDelta<Self::ConceptId, SDCD, D>,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType>;
}
