use crate::{
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{
        Comparison, ComparisonReason, ContextSearch, ReductionReason,
    },
    snap_shot::{mock::MockSnapShot, Reader},
};
use maplit::hashmap;
use std::collections::HashMap;

#[test]
fn comparison_existence_implication_rule_test() {
    let context_cache = ContextCache::default();
    let context_delta = ContextDelta::default();
    let context_snap_shot = MockSnapShot::new_test_case(&concepts(), &labels());
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

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "true",
        1 => ">",
        2 => ":",
        3 => "=>",
        4 => "_x_",
        5 => "_y_",
        6 => "_z_",
        9 => "and",
        21 => "a",
        22 => "b",
        23 => "c",
        28 => "assoc",
        29 => "right"
    }
}

fn concepts() -> [Concept; 30] {
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
    ]
}