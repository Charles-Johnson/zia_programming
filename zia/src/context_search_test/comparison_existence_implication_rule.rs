use crate::{
    concepts::{Concept, SpecificPart, ConcreteConceptType},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{
        Comparison, ComparisonReason, ContextSearch,
        ReductionReason,
    },
    context_search_test::check_order,
    snap_shot::Reader,
};
use maplit::hashmap;

struct ComparisonExistenceImplicationRuleSnapshot {
    concepts: Vec<Concept>,
}

const CONCEPT_LEN: usize = 30;

impl Default for ComparisonExistenceImplicationRuleSnapshot {
    fn default() -> Self {
        let mut true_concept = (ConcreteConceptType::True, 0).into();
        let mut greater_than_concept = (ConcreteConceptType::GreaterThan, 1).into();
        let mut exists_such_that_concept = (ConcreteConceptType::ExistsSuchThat, 2).into();
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
        let mut a = (SpecificPart::default(), 21).into();
        let mut b = (SpecificPart::default(), 22).into();
        let mut c = (SpecificPart::default(), 23).into();
        let mut greater_than_b =
            Concept::composition_of(24, &mut greater_than_concept, &mut b);
        let mut a_greater_than_b =
            Concept::composition_of(25, &mut a, &mut greater_than_b);
        a_greater_than_b.make_reduce_to(&mut true_concept);
        let mut greater_than_c =
            Concept::composition_of(26, &mut greater_than_concept, &mut c);
        let mut b_greater_than_c =
            Concept::composition_of(27, &mut b, &mut greater_than_c);
        b_greater_than_c.make_reduce_to(&mut true_concept);
        let assoc_concept = (ConcreteConceptType::Associativity, 28).into();
        let right_id_concept = (ConcreteConceptType::Right, 29).into();
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
            a,
            b,
            c,
            greater_than_b,
            a_greater_than_b,
            greater_than_c,
            b_greater_than_c,
            assoc_concept,
            right_id_concept
        ];
        Self {
            concepts: check_order(&concepts),
        }
    }
}

impl Reader for ComparisonExistenceImplicationRuleSnapshot {
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
            21 => Some("a".into()),
            22 => Some("b".into()),
            23 => Some("c".into()),
            28 => Some("assoc".into()),
            29 => Some("right".into()),
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
            "a" => Some(21),
            "b" => Some(22),
            "c" => Some(23),
            "assoc" => Some(28),
            "right" => Some(29),
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
        CONCEPT_LEN
    }

    fn precedence_id() -> usize {
        CONCEPT_LEN + 1
    }

    fn right_id() -> usize {
        29
    }

    fn reduction_id() -> usize {
        CONCEPT_LEN + 2
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
        CONCEPT_LEN + 3
    }
}

#[test]
fn comparison_existence_implication_rule_test() {
    let context_cache = ContextCache::default();
    let context_delta = ContextDelta::default();
    let context_snap_shot =
        ComparisonExistenceImplicationRuleSnapshot::new_test_case();
    let context_search = ContextSearch::from((
        &context_snap_shot,
        &context_delta,
        &context_cache,
    ));
    let a_syntax = context_search.to_ast(21);
    let c_syntax = context_search.to_ast(23);
    let true_syntax = context_search.to_ast(0);
    let variable_mask = hashmap! {
        4 => a_syntax.clone(), // x=a
        6 => c_syntax.clone() //z=c
    };
    let comparison_reason = Some(ReductionReason::Rule{
        generalisation: context_search.to_ast(18),
        variable_mask: variable_mask.clone(),
        reason: ReductionReason::Inference{
            implication: context_search.substitute(
                &context_search.to_ast(20), &variable_mask
            ),
            reason: ReductionReason::Existence{
                example: context_search.to_ast(22),
                reason: ReductionReason::Recursive{
                    syntax: context_search.to_ast(11),
                    reason: ReductionReason::Explicit.into(),
                    from: ReductionReason::Partial(hashmap! {
                        context_search.to_ast(25) => (true_syntax.clone(), ReductionReason::Explicit),
                        context_search.to_ast(27) => (true_syntax, ReductionReason::Explicit)
                    }).into()
                }.into()
            }.into()
        }.into()
    });
    assert_eq!(
        context_search.compare(&a_syntax, &c_syntax),
        (
            Comparison::GreaterThan,
            ComparisonReason::Reduction {
                reason: comparison_reason.clone(),
                reversed_reason: None
            }
        )
    );
    assert_eq!(
        context_search.compare(&c_syntax, &a_syntax),
        (
            Comparison::LessThan,
            ComparisonReason::Reduction {
                reason: None,
                reversed_reason: comparison_reason
            }
        )
    );
}
