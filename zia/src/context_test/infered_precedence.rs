use crate::{
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_test::Context,
    mock_snap_shot::MockSnapShot,
};
use maplit::hashmap;
use std::collections::HashMap;

#[allow(clippy::too_many_lines)]
fn concepts() -> [Concept<usize>; 33] {
    let mut true_concept = (ConcreteConceptType::True, 0).into();
    let mut precedes_concept = (ConcreteConceptType::Precedes, 1).into();
    let mut exists_such_that_concept =
        (ConcreteConceptType::ExistsSuchThat, 2).into();
    let mut implication_concept = (ConcreteConceptType::Implication, 3).into();
    let mut x = (SpecificPart::free_variable(), 4).into();
    let mut y = (SpecificPart::bound_variable(), 5).into();
    let mut z = (SpecificPart::free_variable(), 6).into();
    let mut precedes_z =
        Concept::composition_of(7, &mut precedes_concept, &mut z);
    let mut y_precedes_z = Concept::composition_of(8, &mut y, &mut precedes_z);
    let mut and_concept = (SpecificPart::default(), 9).into();
    let mut and_true =
        Concept::composition_of(10, &mut and_concept, &mut true_concept);
    let mut true_and_true =
        Concept::composition_of(11, &mut true_concept, &mut and_true);
    true_and_true.make_reduce_to(&mut true_concept);
    let mut and_y_precedes_z =
        Concept::composition_of(12, &mut and_concept, &mut y_precedes_z);
    let mut precedes_y =
        Concept::composition_of(13, &mut precedes_concept, &mut y);
    let mut x_precedes_y = Concept::composition_of(14, &mut x, &mut precedes_y);
    let mut x_precedes_y_and_y_precedes_z =
        Concept::composition_of(15, &mut x_precedes_y, &mut and_y_precedes_z);
    let mut y_exists_such_that =
        Concept::composition_of(16, &mut y, &mut exists_such_that_concept);
    let mut y_exists_such_that_x_precedes_y_and_y_precedes_z =
        Concept::composition_of(
            17,
            &mut y_exists_such_that,
            &mut x_precedes_y_and_y_precedes_z,
        );
    let mut x_precedes_z = Concept::composition_of(18, &mut x, &mut precedes_z);
    let mut implies_x_precedes_z = Concept::composition_of(
        19,
        &mut implication_concept,
        &mut x_precedes_z,
    );
    let mut
    y_exists_such_that_x_precedes_y_and_y_precedes_z_implies_x_precedes_z =
        Concept::composition_of(
            20,
            &mut y_exists_such_that_x_precedes_y_and_y_precedes_z,
            &mut implies_x_precedes_z,
        );
    y_exists_such_that_x_precedes_y_and_y_precedes_z_implies_x_precedes_z
        .make_reduce_to(&mut true_concept);
    let mut let_concept = (ConcreteConceptType::Let, 21).into();
    let mut reduction_concept = (ConcreteConceptType::Reduction, 22).into();
    let mut definition_concept = (ConcreteConceptType::Define, 23).into();
    let mut precedes_let =
        Concept::composition_of(24, &mut precedes_concept, &mut let_concept);
    let mut definition_precedes_let =
        Concept::composition_of(25, &mut definition_concept, &mut precedes_let);
    definition_precedes_let.make_reduce_to(&mut true_concept);
    let assoc_concept = (ConcreteConceptType::Associativity, 26).into();
    let right_id_concept = (ConcreteConceptType::Right, 27).into();
    let mut precedes_definition = Concept::composition_of(
        28,
        &mut precedes_concept,
        &mut definition_concept,
    );
    let mut reduction_precedes_definition = Concept::composition_of(
        29,
        &mut reduction_concept,
        &mut precedes_definition,
    );
    reduction_precedes_definition.make_reduce_to(&mut true_concept);
    [
        true_concept,
        precedes_concept,
        exists_such_that_concept,
        implication_concept,
        x,
        y,
        z,
        precedes_z,
        y_precedes_z,
        and_concept,
        and_true,
        true_and_true,
        and_y_precedes_z,
        precedes_y,
        x_precedes_y,
        x_precedes_y_and_y_precedes_z,
        y_exists_such_that,
        y_exists_such_that_x_precedes_y_and_y_precedes_z,
        x_precedes_z,
        implies_x_precedes_z,
        y_exists_such_that_x_precedes_y_and_y_precedes_z_implies_x_precedes_z,
        let_concept,
        reduction_concept,
        definition_concept,
        precedes_let,
        definition_precedes_let,
        assoc_concept,
        right_id_concept,
        precedes_definition,
        reduction_precedes_definition,
        (ConcreteConceptType::False, 30).into(), // false
        (ConcreteConceptType::Left, 31).into(),  // left
        (ConcreteConceptType::Label, 32).into(), // label_of
    ]
}

fn concept_labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "true",
        1 => "precedes",
        2 => ":",
        3 => "=>",
        4 => "_x_",
        5 => "_y_",
        6 => "_z_",
        9 => "and",
        21 => "let",
        22 => "->",
        26 => "assoc",
        27 => "right",
        30 => "false",
    }
}
#[test]
fn infered_precedence_test() {
    let snapshot = MockSnapShot::new_test_case(&concepts(), &concept_labels());
    let mut context: Context = snapshot.into();
    assert_eq!(context.execute("-> (precedes let)"), "true");
}
