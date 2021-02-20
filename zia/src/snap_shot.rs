use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType},
    context_delta::{ConceptDelta, ContextDelta},
    errors::{ZiaError, ZiaResult},
};
#[cfg(test)]
use std::collections::HashMap;

pub trait Reader {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept>;
    fn read_concept(&self, delta: &ContextDelta, id: usize) -> Concept {
        delta
            .concept().get(&id)
            .and_then(|cds| {for (cd, _) in cds {
                match cd {
                ConceptDelta::Direct(dcd) => todo!(),
                ConceptDelta::Indirect(icd) => todo!()
                }}
            todo!()})
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .unwrap_or_else(|| panic!("No concept with id = {}", id))
                    .clone()
            })
    }
    fn lowest_unoccupied_concept_id(&self, delta: &ContextDelta) -> usize;
    fn get_label(
        &self,
        delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String>;
    fn ast_from_symbol(&self, delta: &ContextDelta, s: &str) -> SyntaxTree {
        self.concept_from_label(delta, s).map_or_else(
            || s.into(),
            |concept| {
                let syntax = SyntaxTree::from(s);
                self.bind_concept_to_syntax(delta, syntax, concept)
            }
        )
    }
    fn bind_concept_to_syntax(&self, delta: &ContextDelta, syntax: SyntaxTree, concept: usize) -> SyntaxTree {
        if self.concrete_concept_type(delta,concept) == Some(ConcreteConceptType::ExistsSuchThat) {
            syntax.bind_quantifier_concept(concept)
        } else {
            syntax.bind_nonquantifier_concept(concept)
        }
    }
    fn new_syntax_from_concept(&self, delta: &ContextDelta, concept: usize) -> SyntaxTree {
        if self.concrete_concept_type(delta,concept) == Some(ConcreteConceptType::ExistsSuchThat) {
            SyntaxTree::new_quantifier_concept(concept)
        } else {
            SyntaxTree::new_constant_concept(concept)
        }
    }
    fn concept_from_label(
        &self,
        delta: &ContextDelta,
        s: &str,
    ) -> Option<usize>;
    fn get_reduction_of_composition(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> usize {
        self.read_concept(delta, concept)
            .get_composition()
            .and_then(|(left, right)| {
                self.get_reduction_or_reduction_of_composition(delta, left)
                    .find_definition(
                        &self.get_reduction_or_reduction_of_composition(
                            delta, right,
                        ),
                    )
            })
            .unwrap_or(concept)
    }
    fn is_disconnected(&self, delta: &ContextDelta, concept: usize) -> bool {
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
        delta: &ContextDelta,
        con: usize,
    ) -> bool {
        self.read_concept(delta, con)
            .get_righthand_of()
            .iter()
            .find_map(|concept| {
                self.read_concept(delta, *concept)
                    .get_composition()
                    .filter(|(left, _)| Some(*left) != self.concrete_concept_id(delta, ConcreteConceptType::Label))
            })
            .is_none()
    }
    fn get_normal_form(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> Option<usize> {
        self.read_concept(delta, concept)
            .get_reduction()
            .map(|n| self.get_normal_form(delta, n).unwrap_or(n))
    }
    fn get_concept_of_label(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> Option<usize> {
        self.read_concept(delta, concept)
            .get_righthand_of()
            .iter()
            .find(|candidate| {
                self.concrete_concept_id(delta, ConcreteConceptType::Label) == Some(self.read_concept(delta, **candidate)
                    .get_composition()
                    .expect("Candidate should have a definition!")
                    .0)
            })
            .cloned()
    }
    fn contains(
        &self,
        delta: &ContextDelta,
        outer: usize,
        inner: usize,
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
        delta: &ContextDelta,
        outer_concept: usize,
        inner_concept: usize,
    ) -> ZiaResult<()> {
        if let Some(r) = self.read_concept(delta, inner_concept).get_reduction()
        {
            if r == outer_concept || self.contains(delta, r, outer_concept) {
                Err(ZiaError::InfiniteComposition)
            } else {
                self.check_reductions(delta, outer_concept, r)
            }
        } else {
            Ok(())
        }
    }
    fn get_reduction_or_reduction_of_composition(
        &self,
        delta: &ContextDelta,
        concept: usize,
    ) -> Concept {
        self.read_concept(
            delta,
            self.read_concept(delta, concept).get_reduction().unwrap_or_else(
                || self.get_reduction_of_composition(delta, concept),
            ),
        )
    }
    fn concrete_concept_id(
        &self,
        delta: &ContextDelta,
        cc: ConcreteConceptType,
    ) -> Option<usize>;
    fn concrete_concept_type(
        &self,
        delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<ConcreteConceptType>;
    #[cfg(test)]
    fn new_test_case(
        concepts: &[Concept],
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
        concepts: Vec<Concept>,
    }
    impl Apply for MockSnapShot {
        type Delta = ContextDelta;

        fn apply(&mut self, _: Self::Delta) {}

        fn diff(&self, _: Self) -> Self::Delta {
            ContextDelta::default()
        }
    }
    impl Reader for MockSnapShot {
        fn new_test_case(
            concepts: &[Concept],
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

        fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
            self.concepts.get(concept_id)
        }

        fn lowest_unoccupied_concept_id(
            &self,
            _: &crate::context_delta::ContextDelta,
        ) -> usize {
            self.concepts.len()
        }

        fn get_label(
            &self,
            _: &ContextDelta,
            concept_id: usize,
        ) -> Option<String> {
            self.concept_labels.get_by_left(&concept_id).map(|s| s.to_string())
        }

        fn concept_from_label(
            &self,
            _: &ContextDelta,
            s: &str,
        ) -> Option<usize> {
            self.concept_labels.get_by_right(&s).cloned()
        }

        fn concrete_concept_id(
            &self,
            _: &ContextDelta,
            cc: ConcreteConceptType,
        ) -> Option<usize> {
            self.concrete_concepts.get_by_right(&cc).cloned()
        }

        fn concrete_concept_type(
            &self,
            _: &ContextDelta,
            concept_id: usize,
        ) -> Option<ConcreteConceptType> {
            self.concrete_concepts.get_by_left(&concept_id).cloned()
        }
    }
}
