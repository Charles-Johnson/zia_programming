use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_test::Context,
    mock_snap_shot::{ConceptId, MockSnapShot},
    multi_threaded::MultiThreadedContextCache,
};
use maplit::hashmap;
use std::collections::HashMap;

fn concepts() -> [Concept<ConceptId>; 11] {
    let mut precedence_concept = (ConcreteConceptType::Precedence, 0).into();
    let mut greater_than_concept = (ConcreteConceptType::GreaterThan, 1).into();
    let mut default_concept = (ConcreteConceptType::Default, 2).into();
    let mut true_concept = (ConcreteConceptType::True, 3).into();
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
    let assoc_concept: Concept<_> =
        (ConcreteConceptType::Associativity, 8).into();
    let left_concept: Concept<_> = (ConcreteConceptType::Left, 9).into();
    let right_concept: Concept<_> = (ConcreteConceptType::Right, 10).into();
    [
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
    ]
}

fn labels() -> HashMap<ConceptId, &'static str> {
    hashmap! {4 => "a"}
}

type Syntax = <MultiThreadedContextCache as ContextCache>::Syntax;

#[test]
fn basic_precedence() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &labels());
    let mut context: Context = snapshot.into();

    assert_eq!(
        context.ast_from_expression("c b a"),
        Ok(Syntax::from("(c b) a")
            .bind_pair(
                Syntax::new_pair(
                    Syntax::from("c").share(),
                    Syntax::from("b").into()
                )
                .into(),
                Syntax::from("a").bind_nonquantifier_concept(4).into()
            )
            .into())
    );
}
