use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType},
    context_delta::{
        Change, Composition, ConceptDelta, ContextDelta, DirectConceptDelta,
        NewDirectConceptDelta,
    },
    errors::{ZiaError, ZiaResult},
};
#[cfg(test)]
use std::collections::HashMap;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Reader {
    type ConceptId: Copy + Eq + Hash + Display + Debug + Send + Sync;
    fn get_concept(
        &self,
        concept_id: Self::ConceptId,
    ) -> Option<&Concept<Self::ConceptId>>;
    fn read_concept(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        id: Self::ConceptId,
    ) -> Concept<Self::ConceptId> {
        delta
            .concept()
            .get(&id)
            .and_then(|cds| {
                let mut concept = self.get_concept(id).cloned();
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
                                    Change::Create(comp) => {
                                        let [mut left, mut right] = self.concepts_from_composition(delta, *comp);
                                        concept
                                            .as_mut()
                                            .unwrap()
                                            .change_composition(Change::Create(
                                                [&mut left, &mut right],
                                            ))
                                            .unwrap();
                                    },
                                    Change::Update {
                                        before,
                                        after,
                                    } => {
                                        let [mut before_left, mut before_right] = self.concepts_from_composition(delta, *before);
                                        let [mut after_left, mut after_right] = self.concepts_from_composition(delta, *after);
                                        concept
                                            .as_mut()
                                            .unwrap()
                                            .change_composition(
                                                Change::Update {
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
                                    Change::Remove(comp) => {
                                        let [mut left, mut right] = self.concepts_from_composition(delta, *comp);
                                        concept
                                            .as_mut()
                                            .unwrap()
                                            .change_composition(Change::Remove(
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
                            if let Some(c) = &mut concept {
                                c.apply_indirect(delta);
                            } else {
                                panic!("Concept doesn't exist");
                            }
                        },
                    }
                }
                concept
            })
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .unwrap_or_else(|| panic!("No concept with id = {}", id))
                    .clone()
            })
    }
    fn concepts_from_composition(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        comp: Composition<Self::ConceptId>,
    ) -> [Concept<Self::ConceptId>; 2] {
        [
            self.read_concept(delta, comp.left_id),
            self.read_concept(delta, comp.right_id),
        ]
    }
    fn lowest_unoccupied_concept_id(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
    ) -> Self::ConceptId;
    fn get_label(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept_id: Self::ConceptId,
    ) -> Option<String>;
    fn ast_from_symbol(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        s: &str,
    ) -> SyntaxTree<Self::ConceptId> {
        self.concept_from_label(delta, s).map_or_else(
            || s.into(),
            |concept| {
                let syntax = SyntaxTree::from(s);
                self.bind_concept_to_syntax(delta, syntax, concept)
            },
        )
    }
    fn bind_concept_to_syntax(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        syntax: SyntaxTree<Self::ConceptId>,
        concept: Self::ConceptId,
    ) -> SyntaxTree<Self::ConceptId> {
        if self.concrete_concept_type(delta, concept)
            == Some(ConcreteConceptType::ExistsSuchThat)
        {
            syntax.bind_quantifier_concept(concept)
        } else {
            syntax.bind_nonquantifier_concept(concept)
        }
    }
    fn new_syntax_from_concept_that_has_no_label_or_composition(
        &self,
        concept: &Concept<Self::ConceptId>,
    ) -> SyntaxTree<Self::ConceptId> {
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
    fn concept_from_label(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        s: &str,
    ) -> Option<Self::ConceptId>;
    fn get_reduction_of_composition(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept: Self::ConceptId,
    ) -> Self::ConceptId {
        self.read_concept(delta, concept)
            .get_composition()
            .and_then(|(left, right)| {
                self.get_reduction_or_reduction_of_composition(delta, left)
                    .find_as_lefthand_in_composition_with_righthand(
                        self.get_reduction_or_reduction_of_composition(
                            delta, right,
                        )
                        .id(),
                    )
            })
            .unwrap_or(concept)
    }
    fn is_disconnected(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept: Self::ConceptId,
    ) -> bool {
        self.read_concept(delta, concept).get_reduction().is_none()
            && self.read_concept(delta, concept).get_composition().is_none()
            && self.read_concept(delta, concept).get_lefthand_of().is_empty()
            && self.righthand_of_without_label_is_empty(delta, concept)
            && self
                .read_concept(delta, concept)
                .find_what_reduces_to_it()
                .next()
                .is_none()
    }
    fn righthand_of_without_label_is_empty(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        con: Self::ConceptId,
    ) -> bool {
        self.concrete_concept_id(delta, ConcreteConceptType::Label)
            .and_then(|label_id| {
                self.read_concept(delta, con)
                    .find_as_righthand_in_composition_with_lefthand(label_id)
            })
            .is_none()
    }
    fn get_normal_form(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept: Self::ConceptId,
    ) -> Option<Self::ConceptId> {
        self.read_concept(delta, concept)
            .get_reduction()
            .map(|n| self.get_normal_form(delta, n).unwrap_or(n))
    }
    fn get_concept_of_label(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept: Self::ConceptId,
    ) -> Option<Self::ConceptId> {
        let label_concept_id =
            self.concrete_concept_id(delta, ConcreteConceptType::Label)?;

        self.read_concept(delta, concept)
            .find_as_righthand_in_composition_with_lefthand(label_concept_id)
    }
    fn contains(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
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
    fn check_reductions(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
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
    fn get_reduction_or_reduction_of_composition(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept: Self::ConceptId,
    ) -> Concept<Self::ConceptId> {
        self.read_concept(
            delta,
            self.read_concept(delta, concept).get_reduction().unwrap_or_else(
                || self.get_reduction_of_composition(delta, concept),
            ),
        )
    }
    fn concrete_concept_id(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        cc: ConcreteConceptType,
    ) -> Option<Self::ConceptId>;
    fn concrete_concept_type(
        &self,
        delta: &ContextDelta<Self::ConceptId>,
        concept_id: Self::ConceptId,
    ) -> Option<ConcreteConceptType>;
    #[cfg(test)]
    fn new_test_case(
        concepts: &[Concept<Self::ConceptId>],
        concept_labels: &HashMap<usize, &'static str>,
    ) -> Self;
}

#[cfg(test)]
pub mod mock {
    use super::Reader;
    use crate::{
        concepts::{Concept, ConcreteConceptType},
        context_delta::ContextDelta,
        context_search_test::check_order,
        delta::Apply,
    };
    use bimap::BiMap;
    use std::collections::HashMap;

    #[derive(Debug, Default)]
    pub struct MockSnapShot {
        concrete_concepts: BiMap<usize, ConcreteConceptType>,
        concept_labels: BiMap<usize, &'static str>,
        concepts: Vec<Concept<usize>>,
    }
    impl Apply for MockSnapShot {
        type Delta = ContextDelta<usize>;

        fn apply(&mut self, _: Self::Delta) {}

        fn diff(&self, _: Self) -> Self::Delta {
            ContextDelta::default()
        }
    }
    impl Reader for MockSnapShot {
        type ConceptId = usize;

        fn new_test_case(
            concepts: &[Concept<Self::ConceptId>],
            concept_labels: &HashMap<usize, &'static str>,
        ) -> Self {
            Self {
                concepts: check_order(concepts),
                concept_labels: concept_labels
                    .iter()
                    .map(|(l, r)| (*l, *r))
                    .collect(),
                concrete_concepts: concepts
                    .iter()
                    .filter_map(|c| {
                        c.get_concrete_concept_type().map(|cc| (c.id(), cc))
                    })
                    .collect(),
            }
        }

        fn get_concept(
            &self,
            concept_id: usize,
        ) -> Option<&Concept<Self::ConceptId>> {
            self.concepts.get(concept_id)
        }

        fn lowest_unoccupied_concept_id(
            &self,
            _: &ContextDelta<Self::ConceptId>,
        ) -> usize {
            self.concepts.len()
        }

        fn get_label(
            &self,
            _: &ContextDelta<Self::ConceptId>,
            concept_id: Self::ConceptId,
        ) -> Option<String> {
            self.concept_labels.get_by_left(&concept_id).map(|s| s.to_string())
        }

        fn concept_from_label(
            &self,
            _: &ContextDelta<Self::ConceptId>,
            s: &str,
        ) -> Option<usize> {
            self.concept_labels.get_by_right(&s).cloned()
        }

        fn concrete_concept_id(
            &self,
            _: &ContextDelta<Self::ConceptId>,
            cc: ConcreteConceptType,
        ) -> Option<usize> {
            self.concrete_concepts.get_by_right(&cc).cloned()
        }

        fn concrete_concept_type(
            &self,
            _: &ContextDelta<Self::ConceptId>,
            concept_id: Self::ConceptId,
        ) -> Option<ConcreteConceptType> {
            self.concrete_concepts.get_by_left(&concept_id).cloned()
        }
    }
}
