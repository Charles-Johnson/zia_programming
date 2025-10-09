use super::Syntax;
use crate::{
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_test::Context,
    mock_snap_shot::MockSnapShot,
};
use maplit::hashmap;
use std::collections::HashMap;

fn concepts() -> [Concept<usize>; 11] {
    let mut c_concept = (SpecificPart::default(), 0).into();
    let mut precedes_concept = (ConcreteConceptType::Precedes, 1).into();
    let b_concept = (SpecificPart::default(), 2).into();
    let mut true_concept = (ConcreteConceptType::True, 3).into();
    let mut a_concept = (SpecificPart::default(), 4).into();
    let mut precedes_a_concept =
        Concept::composition_of(5, &mut precedes_concept, &mut a_concept);
    let mut c_precedes_a_concept =
        Concept::composition_of(6, &mut c_concept, &mut precedes_a_concept);
    c_precedes_a_concept.make_reduce_to(&mut true_concept);
    let assoc_concept: Concept<_> =
        (ConcreteConceptType::Associativity, 7).into();
    let left_concept: Concept<_> = (ConcreteConceptType::Left, 8).into();
    let right_concept: Concept<_> = (ConcreteConceptType::Right, 9).into();
    let label_of_concept: Concept<_> = (ConcreteConceptType::Label, 10).into();
    [
        c_concept,
        precedes_concept,
        b_concept,
        true_concept,
        a_concept,
        precedes_a_concept,
        c_precedes_a_concept,
        assoc_concept,
        left_concept,
        right_concept,
        label_of_concept,
    ]
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {0 => "c", 2 => "b", 4 => "a", 1 => "precedes"}
}

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
