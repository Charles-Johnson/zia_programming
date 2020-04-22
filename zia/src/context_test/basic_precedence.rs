use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context::Context,
    context_delta::ContextDelta,
    context_search_test::check_order,
    delta::Apply,
    snap_shot::Reader as SnapShotReader,
};

struct BasicPrecedenceSnapShot {
    concepts: Vec<Concept>,
}

impl Default for BasicPrecedenceSnapShot {
    fn default() -> Self {
        let mut precedence_concept = (SpecificPart::Concrete, 0).into();
        let mut greater_than_concept = (SpecificPart::Concrete, 1).into();
        let mut default_concept = (SpecificPart::Concrete, 2).into();
        let mut true_concept = (SpecificPart::Concrete, 3).into();
        let mut abstract_concept = (SpecificPart::default(), 4).into();
        let mut precedence_of_abstract_concept = Concept::composition_of(
            5,
            &mut precedence_concept,
            &mut abstract_concept,
        );
        let mut greater_than_precedence_of_abstract_concept =
            Concept::composition_of(
                6,
                &mut greater_than_concept,
                &mut precedence_of_abstract_concept,
            );
        let mut precedence_of_abstract_concept_is_below_default =
            Concept::composition_of(
                7,
                &mut default_concept,
                &mut greater_than_precedence_of_abstract_concept,
            );
        precedence_of_abstract_concept_is_below_default
            .make_reduce_to(&mut true_concept);
        let assoc_concept: Concept = (SpecificPart::Concrete, 8).into();
        let left_concept: Concept = (SpecificPart::Concrete, 9).into();
        let right_concept: Concept = (SpecificPart::Concrete, 10).into();
        Self {
            concepts: check_order(&[
                precedence_concept,
                greater_than_concept,
                default_concept,
                true_concept,
                abstract_concept,
                precedence_of_abstract_concept,
                greater_than_precedence_of_abstract_concept,
                precedence_of_abstract_concept_is_below_default,
                assoc_concept,
                left_concept,
                right_concept,
            ]),
        }
    }
}

impl SnapShotReader for BasicPrecedenceSnapShot {
    fn implication_id() -> usize {
        13
    }

    fn false_id() -> usize {
        14
    }

    fn reduction_id() -> usize {
        11
    }

    fn assoc_id() -> usize {
        8
    }

    fn right_id() -> usize {
        10
    }

    fn left_id() -> usize {
        9
    }

    fn exists_such_that_id() -> usize {
        12
    }

    fn precedence_id() -> usize {
        0
    }

    fn greater_than_id() -> usize {
        1
    }

    fn default_id() -> usize {
        2
    }

    fn true_id() -> usize {
        3
    }

    fn concept_from_label(
        &self,
        _: &ContextDelta,
        label: &str,
    ) -> Option<usize> {
        match label {
            "a" => Some(4),
            _ => None,
        }
    }

    fn get_concept(&self, concept_id: usize) -> Option<&Concept> {
        self.concepts.get(concept_id)
    }

    fn lowest_unoccupied_concept_id(&self, _delta: &ContextDelta) -> usize {
        self.concepts.len()
    }

    fn get_label(
        &self,
        _delta: &ContextDelta,
        concept_id: usize,
    ) -> Option<String> {
        match concept_id {
            4 => Some("a".into()),
            _ => None,
        }
    }
}

impl Apply for BasicPrecedenceSnapShot {
    type Delta = ContextDelta;

    fn apply(&mut self, _: Self::Delta) {
        unimplemented!()
    }

    fn diff(&self, _: Self) -> Self::Delta {
        unimplemented!()
    }
}

#[test]
fn basic_precedence() {
    let mut context = Context::<BasicPrecedenceSnapShot>::new_test_case();

    assert_eq!(
        context.ast_from_expression("c b a"),
        Ok(SyntaxTree::from("(c b) a")
            .bind_pair(
                SyntaxTree::new_pair(
                    SyntaxTree::from("c"),
                    SyntaxTree::from("b")
                ),
                SyntaxTree::from("a").bind_concept(4)
            )
            .into())
    );
}
