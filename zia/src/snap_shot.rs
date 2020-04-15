use crate::{
    ast::SyntaxTree,
    concepts::Concept,
    constants::LABEL,
    context_delta::{ConceptDelta, ContextDelta},
    delta::Apply,
    errors::{ZiaError, ZiaResult},
};

pub trait Reader: Default {
    fn get_concept(&self, concept_id: usize) -> Option<&Concept>;
    fn read_concept(&self, delta: &ContextDelta, id: usize) -> Concept {
        delta
            .concept().get(&id)
            .and_then(|(cd, _, _)| match cd {
                ConceptDelta::Insert(c) => Some(c.clone()),
                ConceptDelta::Remove(_) => None,
                ConceptDelta::Update(d) => {
                    let mut concept = self
                        .get_concept(id)
                        .expect("Deltas imply that a concept that doesn't exist will be updated!")
                        .clone();
                    concept.apply(d.clone());
                    Some(concept)
                }
            })
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .unwrap_or_else(|| panic!("No concept with id = {}", id))
                    .clone()
            })
    }
    fn has_variable(&self, delta: &ContextDelta, variable_id: usize) -> bool;
    fn lowest_unoccupied_concept_id(&self, delta: &ContextDelta) -> usize;
    fn get_label(
        &self,
        delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String>;
    fn ast_from_symbol(&self, delta: &ContextDelta, s: &str) -> SyntaxTree {
        self.concept_from_label(delta, s).map_or_else(
            || s.into(),
            |concept| SyntaxTree::from(s).bind_concept(concept),
        )
    }
    fn true_id() -> usize {
        unimplemented!()
    }
    fn implication_id() -> usize {
        unimplemented!()
    }
    fn precedence_id() -> usize {
        unimplemented!()
    }
    fn greater_than_id() -> usize {
        unimplemented!()
    }
    fn default_id() -> usize {
        unimplemented!()
    }
    fn reduction_id() -> usize {
        unimplemented!()
    }
    fn false_id() -> usize {
        unimplemented!()
    }
    fn assoc_id() -> usize {
        unimplemented!()
    }
    fn right_id() -> usize {
        unimplemented!()
    }
    fn left_id() -> usize {
        unimplemented!()
    }
    fn exists_such_that_id() -> usize {
        unimplemented!()
    }
    fn define_id() -> usize {
        unimplemented!()
    }
    fn let_id() -> usize {
        unimplemented!()
    }
    fn label_id() -> usize {
        unimplemented!()
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
            .get_definition()
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
            && self.read_concept(delta, concept).get_definition().is_none()
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
                    .get_definition()
                    .filter(|(left, _)| *left != LABEL)
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
                self.read_concept(delta, **candidate)
                    .get_definition()
                    .expect("Candidate should have a definition!")
                    .0
                    == LABEL
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
            self.read_concept(delta, outer).get_definition()
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
                Err(ZiaError::InfiniteDefinition)
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
    #[cfg(test)]
    fn new_test_case() -> Self {
        let test_case = Self::default();
        let delta = ContextDelta::default();
        for id in 0..test_case.lowest_unoccupied_concept_id(&delta) {
            test_case.get_label(&delta, id).map(|s| {
                assert_eq!(test_case.concept_from_label(&delta, &s), Some(id))
            });
        }
        test_case
    }
}
