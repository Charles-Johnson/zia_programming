use super::Syntax;
use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_delta::NestedDelta,
    context_search::{ContextReferences, ContextSearch},
    mock_snap_shot::MockSnapShot,
    multi_threaded::{
        MultiThreadedContextCache, MultiThreadedReductionReason,
        SharedContextDelta, SharedDirectConceptDelta,
    },
};
use maplit::{hashmap, hashset};
use std::collections::HashMap;

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {
        0 => "=>",
        1 => "true",
        3 => "a",
        7 => "example",
        9 => "_y_",
        10 => "b",
        11 => "_x_",
        14 => "exists_such_that",
    }
}

type ReductionReason = MultiThreadedReductionReason<Syntax>;

#[test]
fn existence_inference_rule() {
    let context_cache = MultiThreadedContextCache::default();
    let context_delta =
        NestedDelta::<_, SharedDirectConceptDelta<_>, _>::default();
    let context_snap_shot = MockSnapShot::new_test_case(&concepts(), &labels());
    let bound_variables = hashset! {};
    let context_search = ContextSearch::from(ContextReferences {
        snap_shot: &context_snap_shot,
        delta: SharedContextDelta(context_delta.into()),
        cache: &context_cache,
        bound_variable_syntax: &bound_variables,
    });
    let example_syntax =
        Syntax::new_pair(context_search.to_ast(&7), context_search.to_ast(&10))
            .share();
    let true_syntax = Syntax::from("true").bind_nonquantifier_concept(1);
    let variable_mask = hashmap! {9.into() => context_search.to_ast(&7)};
    assert_eq!(
        context_search.reduce(&example_syntax),
        Some((
            true_syntax.into(),
            ReductionReason::Rule {
                generalisation: context_search.to_ast(&2),
                variable_mask: variable_mask.clone(),
                reason: ReductionReason::Inference {
                    implication: context_search
                        .substitute(&context_search.to_ast(&6), &variable_mask),
                    reason: ReductionReason::Existence{
                        generalisation: context_search
                            .substitute(&context_search.to_ast(&13), &variable_mask),
                        substitutions: hashmap!{context_search.to_ast(&11) => context_search.to_ast(&3)},
                    }
                    .into()
                }
                .into()
            }
        ))
    );
}

fn concepts() -> [Concept<usize>; 17] {
    let mut implication_concept = (ConcreteConceptType::Implication, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut concept_a = (SpecificPart::default(), 3).into();
    let mut example_concept = (SpecificPart::default(), 7).into();
    let mut free_variable_concept = (SpecificPart::free_variable(), 9).into();
    let mut concept_b = (SpecificPart::default(), 10).into();
    let mut bound_variable = (SpecificPart::bound_variable(), 11).into();
    let mut exists_such_that_concept =
        (ConcreteConceptType::ExistsSuchThat, 14).into();
    let mut bound_variable_composed_with_free_variable =
        Concept::composition_of(
            13,
            &mut bound_variable,
            &mut free_variable_concept,
        );
    let mut bound_variable_exists_such_that = Concept::composition_of(
        12,
        &mut bound_variable,
        &mut exists_such_that_concept,
    );
    let mut cause_concept = Concept::composition_of(
        4,
        &mut bound_variable_exists_such_that,
        &mut bound_variable_composed_with_free_variable,
    );
    let mut result_concept =
        Concept::composition_of(2, &mut free_variable_concept, &mut concept_b);
    let mut implies_result_concept = Concept::composition_of(
        5,
        &mut implication_concept,
        &mut result_concept,
    );
    let mut cause_implies_result_concept = Concept::composition_of(
        6,
        &mut cause_concept,
        &mut implies_result_concept,
    );
    cause_implies_result_concept.make_reduce_to(&mut true_concept);
    let mut example_composition =
        Concept::composition_of(8, &mut concept_a, &mut example_concept);
    example_composition.make_reduce_to(&mut true_concept);
    let assoc_concept = (ConcreteConceptType::Associativity, 15).into();
    let right_concept = (ConcreteConceptType::Right, 16).into();
    [
        implication_concept,
        true_concept,
        result_concept,
        concept_a,
        cause_concept,
        implies_result_concept,
        cause_implies_result_concept,
        example_concept,
        example_composition,
        free_variable_concept,
        concept_b,
        bound_variable,
        bound_variable_exists_such_that,
        bound_variable_composed_with_free_variable,
        exists_such_that_concept,
        assoc_concept,
        right_concept,
    ]
}
