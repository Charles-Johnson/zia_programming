use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search_test::check_order,
    context::Context,
    delta::Apply,
    snap_shot::Reader,
};

struct InferedPrecedenceSnapshot {
    concepts: Vec<Concept>,
}

const CONCEPT_LEN: usize = 36;

impl Default for InferedPrecedenceSnapshot {
    fn default() -> Self {
        let mut true_concept = (SpecificPart::Concrete, 0).into();
        let mut greater_than_concept = (SpecificPart::Concrete, 1).into();
        let mut exists_such_that_concept = (SpecificPart::Concrete, 2).into();
        let mut implication_concept = (SpecificPart::Concrete, 3).into();
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
        let mut and_y_greater_than_z = Concept::composition_of(
            12,
            &mut and_concept,
            &mut y_greater_than_z,
        );
        let mut greater_than_y =
            Concept::composition_of(13, &mut greater_than_concept, &mut y);
        let mut x_greater_than_y =
            Concept::composition_of(14, &mut x, &mut greater_than_y);
        let mut x_greater_than_y_and_y_greater_than_z = Concept::composition_of(
            15,
            &mut x_greater_than_y,
            &mut and_y_greater_than_z,
        );
        let mut exists_such_that_x_greater_than_y_and_y_greater_than_z =
            Concept::composition_of(
                16,
                &mut exists_such_that_concept,
                &mut x_greater_than_y_and_y_greater_than_z,
            );
        let mut y_exists_such_that_x_greater_than_y_and_y_greater_than_z =
            Concept::composition_of(
                17,
                &mut y,
                &mut exists_such_that_x_greater_than_y_and_y_greater_than_z,
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
        let mut let_concept = (SpecificPart::Concrete, 21).into();
        let mut reduction_concept = (SpecificPart::Concrete, 22).into();
        let mut precedence_concept = (SpecificPart::Concrete, 23).into();
        let mut default_concept = (SpecificPart::Concrete, 24).into();
        let mut let_precedence = Concept::composition_of(25, &mut precedence_concept, &mut let_concept);
        let mut reduction_precedence = Concept::composition_of(26, &mut precedence_concept, &mut reduction_concept);
        let mut greater_than_let_precedence = Concept::composition_of(27, &mut greater_than_concept, &mut let_precedence);
        let assoc_concept = (SpecificPart::Concrete, 28).into();
        let right_id_concept = (SpecificPart::Concrete, 29).into();
        let mut greater_than_reduction_precedence = Concept::composition_of(30, &mut greater_than_concept, &mut reduction_precedence);
        let mut default_greater_than_reduction_precedence = Concept::composition_of(31, &mut default_concept, &mut greater_than_reduction_precedence);
        default_greater_than_reduction_precedence.make_reduce_to(&mut true_concept);
        let mut reduction_preceeds_let = Concept::composition_of(32, &mut reduction_precedence, &mut greater_than_let_precedence);
        reduction_preceeds_let.make_reduce_to(&mut true_concept);
        let concepts: [_; CONCEPT_LEN] = [
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
            exists_such_that_x_greater_than_y_and_y_greater_than_z,
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
            (SpecificPart::Concrete, 33).into(), // false
            (SpecificPart::Concrete, 34).into(), // left
            (SpecificPart::Concrete, 35).into() // label_of
        ];
        Self {
            concepts: check_order(&concepts),
        }
    }
}

impl Reader for InferedPrecedenceSnapshot {
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
            0 => Some("true".into()),
            1 => Some(">".into()),
            2 => Some(":".into()),
            3 => Some("=>".into()),
            4 => Some("_x_".into()),
            5 => Some("_y_".into()),
            6 => Some("_z_".into()),
            9 => Some("and".into()),
            21 => Some("let".into()),
            22 => Some("->".into()),
            23 => Some("prec".into()),
            24 => Some("default".into()),
            28 => Some("assoc".into()),
            29 => Some("right".into()),
            33 => Some("false".into()),
            _ => None,
        }
    }

    fn concept_from_label(
        &self,
        _delta: &ContextDelta,
        s: &str,
    ) -> Option<usize> {
        match s {
            "true" => Some(0),
            ">" => Some(1),
            ":" => Some(2),
            "=>" => Some(3),
            "_x_" => Some(4),
            "_y_" => Some(5),
            "_z_" => Some(6),
            "and" => Some(9),
            "let" => Some(21),
            "->" => Some(22),
            "prec" => Some(23),
            "default" => Some(24),
            "assoc" => Some(28),
            "right" => Some(29),
            "false" => Some(33),
            _ => None,
        }
    }

    fn greater_than_id() -> usize {
        1
    }

    fn assoc_id() -> usize {
        28
    }

    fn left_id() -> usize {
        34
    }

    fn precedence_id() -> usize {
        23
    }

    fn right_id() -> usize {
        29
    }

    fn reduction_id() -> usize {
        22
    }

    fn exists_such_that_id() -> usize {
        2
    }

    fn implication_id() -> usize {
        3
    }

    fn true_id() -> usize {
        0
    }

    fn false_id() -> usize {
        33
    }

    fn let_id() -> usize {
        21
    }

    fn default_id() -> usize {
        24
    }

    fn label_id() -> usize {
        35
    }
}

impl Apply for InferedPrecedenceSnapshot {
    type Delta = ContextDelta;

    fn apply(&mut self, _: Self::Delta) {
        
    }

    fn diff(&self, _: Self) -> Self::Delta {
        ContextDelta::default()
    }
}

#[test]
fn comparison_existence_implication_rule_test() {
    let mut context = Context::<InferedPrecedenceSnapshot>::new_test_case();
    assert_eq!(context.ast_from_expression("let a b -> c"), Ok(
        SyntaxTree::new_pair(
            SyntaxTree::from("let").bind_concept(21),
            SyntaxTree::new_pair(
                SyntaxTree::new_pair(SyntaxTree::from("a"), SyntaxTree::from("b")),
                SyntaxTree::new_pair(
                    SyntaxTree::from("->").bind_concept(22),
                    SyntaxTree::from("c")
                )
            )
        ).into()
    ));
}
