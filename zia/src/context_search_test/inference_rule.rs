use crate::{
    ast::SyntaxTree,
    concepts::{Concept, ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::ContextDelta,
    context_search::{ContextSearch, ReductionReason},
    snap_shot::{mock::MockSnapShot, Reader},
};
use maplit::hashmap;
use std::collections::HashMap;

fn concepts() -> [Concept; 15] {
    let mut implication_concept = (ConcreteConceptType::Implication, 0).into();
    let mut true_concept = (ConcreteConceptType::True, 1).into();
    let mut concept_a = (SpecificPart::default(), 3).into();
    let mut example_concept = (SpecificPart::default(), 7).into();
    let mut variable_concept = (SpecificPart::variable(), 9).into();
    let mut concept_b = (SpecificPart::default(), 10).into();
    let mut cause_concept =
        Concept::composition_of(4, &mut concept_a, &mut variable_concept);
    let mut result_concept =
        Concept::composition_of(2, &mut variable_concept, &mut concept_b);
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
        variable_concept,
        concept_b,
        (ConcreteConceptType::Precedence, 11).into(),
        (ConcreteConceptType::Associativity, 12).into(),
        (ConcreteConceptType::Left, 13).into(),
        (ConcreteConceptType::Right, 14).into(),
    ]
}

fn labels() -> HashMap<usize, &'static str> {
    hashmap! {}
}

#[test]
fn inference_rule() {
    let context_cache = ContextCache::default();
    let context_delta = ContextDelta::default();
    let context_snap_shot = MockSnapShot::new_test_case(&concepts(), &labels());
    let context_search = ContextSearch::from((
        &context_snap_shot,
        &context_delta,
        &context_cache,
    ));
    let example_syntax = SyntaxTree::new_pair(
        SyntaxTree::new_concept(7),
        SyntaxTree::new_concept(10),
    );
    let true_syntax = SyntaxTree::new_concept(1);
    assert_eq!(
        context_search.reduce(&example_syntax.into()),
        Some((
            true_syntax.into(),
            ReductionReason::Rule {
                reason: ReductionReason::Inference {
                    implication: context_search.to_ast(6),
                    reason: ReductionReason::Explicit.into()
                }
                .into(),
                generalisation: context_search.to_ast(2),
                variable_mask: hashmap! {9 => context_search.to_ast(7)}
            }
        ))
    );
}
