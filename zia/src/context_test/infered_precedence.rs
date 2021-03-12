use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context::Context,
    snap_shot::mock::MockSnapShot,
};
use maplit::hashmap;
use std::collections::HashMap;

const CONCEPT_LEN: usize = 36;

fn concepts() -> [Concept; CONCEPT_LEN] {
    let mut true_concept = (ConcreteConceptType::True, 0).into();
    let mut greater_than_concept = (ConcreteConceptType::GreaterThan, 1).into();
    let mut exists_such_that_concept =
        (ConcreteConceptType::ExistsSuchThat, 2).into();
    let mut implication_concept = (ConcreteConceptType::Implication, 3).into();
    let mut x = (SpecificPart::variable(), 4).into();
    let mut y = (SpecificPart::variable(), 5).into();
    let mut z = (SpecificPart::variable(), 6).into();
    let mut greater_than_z =
        Concept::composition_of(7, &mut greater_than_concept, &mut z);
    let mut y_greater_than_z =
        Concept::composition_of(8, &mut y, &mut greater_than_z);
    let mut and_concept = (SpecificPart::default(), 9).into();
    let mut and_true =
        Concept::composition_of(10, &mut and_concept, &mut true_concept);
    let mut true_and_true =
        Concept::composition_of(11, &mut true_concept, &mut and_true);
    true_and_true.make_reduce_to(&mut true_concept);
    let mut and_y_greater_than_z =
        Concept::composition_of(12, &mut and_concept, &mut y_greater_than_z);
    let mut greater_than_y =
        Concept::composition_of(13, &mut greater_than_concept, &mut y);
    let mut x_greater_than_y =
        Concept::composition_of(14, &mut x, &mut greater_than_y);
    let mut x_greater_than_y_and_y_greater_than_z = Concept::composition_of(
        15,
        &mut x_greater_than_y,
        &mut and_y_greater_than_z,
    );
    let mut y_exists_such_that =
        Concept::composition_of(16, &mut y, &mut exists_such_that_concept);
    let mut y_exists_such_that_x_greater_than_y_and_y_greater_than_z =
        Concept::composition_of(
            17,
            &mut y_exists_such_that,
            &mut x_greater_than_y_and_y_greater_than_z,
        );
    let mut x_greater_than_z =
        Concept::composition_of(18, &mut x, &mut greater_than_z);
    let mut implies_x_greater_than_z = Concept::composition_of(
        19,
        &mut implication_concept,
        &mut x_greater_than_z,
    );
    let mut
    y_exists_such_that_x_greater_than_y_and_y_greater_than_z_implies_x_greater_than_z =
        Concept::composition_of(
            20,
            &mut y_exists_such_that_x_greater_than_y_and_y_greater_than_z,
            &mut implies_x_greater_than_z,
        );
    y_exists_such_that_x_greater_than_y_and_y_greater_than_z_implies_x_greater_than_z.make_reduce_to(&mut true_concept);
    let mut let_concept = (ConcreteConceptType::Let, 21).into();
    let mut reduction_concept = (ConcreteConceptType::Reduction, 22).into();
    let mut precedence_concept = (ConcreteConceptType::Precedence, 23).into();
    let mut default_concept = (ConcreteConceptType::Default, 24).into();
    let mut let_precedence =
        Concept::composition_of(25, &mut precedence_concept, &mut let_concept);
    let mut reduction_precedence = Concept::composition_of(
        26,
        &mut precedence_concept,
        &mut reduction_concept,
    );
    let mut greater_than_let_precedence = Concept::composition_of(
        27,
        &mut greater_than_concept,
        &mut let_precedence,
    );
    let assoc_concept = (ConcreteConceptType::Associativity, 28).into();
    let right_id_concept = (ConcreteConceptType::Right, 29).into();
    let mut greater_than_reduction_precedence = Concept::composition_of(
        30,
        &mut greater_than_concept,
        &mut reduction_precedence,
    );
    let mut default_greater_than_reduction_precedence = Concept::composition_of(
        31,
        &mut default_concept,
        &mut greater_than_reduction_precedence,
    );
    default_greater_than_reduction_precedence.make_reduce_to(&mut true_concept);
    let mut reduction_preceeds_let = Concept::composition_of(
        32,
        &mut reduction_precedence,
        &mut greater_than_let_precedence,
    );
    reduction_preceeds_let.make_reduce_to(&mut true_concept);
    [
            true_concept,
            greater_than_concept,
            exists_such_that_concept,
            implication_concept,
            x,
            y,
            z,
            greater_than_z,
            y_greater_than_z,
            and_concept,
            and_true,
            true_and_true,
            and_y_greater_than_z,
            greater_than_y,
            x_greater_than_y,
            x_greater_than_y_and_y_greater_than_z,
            y_exists_such_that,
            y_exists_such_that_x_greater_than_y_and_y_greater_than_z,
            x_greater_than_z,
            implies_x_greater_than_z,
            y_exists_such_that_x_greater_than_y_and_y_greater_than_z_implies_x_greater_than_z,
            let_concept,
            reduction_concept,
            precedence_concept,
            default_concept,
            let_precedence,
            reduction_precedence,
            greater_than_let_precedence,
            assoc_concept,
            right_id_concept,
            greater_than_reduction_precedence,
            default_greater_than_reduction_precedence,
            reduction_preceeds_let,
            (ConcreteConceptType::False, 33).into(), // false
            (ConcreteConceptType::Left, 34).into(), // left
            (ConcreteConceptType::Label, 35).into() // label_of
        ]
}

fn concept_labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "true",
        1 => ">",
        2 => ":",
        3 => "=>",
        4 => "_x_",
        5 => "_y_",
        6 => "_z_",
        9 => "and",
        21 => "let",
        22 => "->",
        23 => "prec",
        24 => "default",
        28 => "assoc",
        29 => "right",
        33 => "false",
    }
}

#[test]
fn infered_precedence_test() {
    let mut context =
        Context::<MockSnapShot>::new_test_case(&concepts(), &concept_labels());
    assert_eq!(
        context.ast_from_expression("let a b -> c"),
        Ok(SyntaxTree::new_pair(
            SyntaxTree::from("let").bind_nonquantifier_concept(21),
            SyntaxTree::new_pair(
                SyntaxTree::new_pair(
                    SyntaxTree::from("a"),
                    SyntaxTree::from("b")
                ),
                SyntaxTree::new_pair(
                    SyntaxTree::from("->").bind_nonquantifier_concept(22),
                    SyntaxTree::from("c")
                )
            )
        )
        .into())
    );
    assert_eq!(context.execute("default > (prec let)"), "true");
}
