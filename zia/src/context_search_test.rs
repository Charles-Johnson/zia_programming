use crate::{
    ast::SyntaxTree,
    concepts::{Concept, SpecificPart},
    context_delta::ContextDelta,
    context_search::{ContextSearch, ContextCache},
    snap_shot::SnapShotReader,
};
use std::{str::FromStr, sync::Arc};

#[derive(Default)]
struct BasicReductionSnapShot;

impl SnapShotReader for BasicReductionSnapShot {
    fn read_concept(&self, _delta: &ContextDelta, concept_id: usize) -> Concept {
        let mut concrete = (SpecificPart::Concrete, 0).into();
        let mut abstrct: Concept = (SpecificPart::default(), 1).into();
        abstrct.make_reduce_to(&mut concrete);
        match concept_id {
            0 => concrete,
            1 => abstrct,
            _ => panic!("No concepts with id: {}", concept_id)
        }
    }
    fn find_definition(&self, _delta: &ContextDelta, _left_id: usize, _right_id: usize) -> Option<usize> {
        None
    }
    fn has_variable(&self, _delta: &ContextDelta, _variable_id: usize) -> bool {
        false
    }
    fn concept_len(&self, _delta: &ContextDelta) -> usize {
        2
    }
    fn get_label(&self, _delta: &ContextDelta, concept_id: usize) -> Option<String> {
        match concept_id {
            0 => Some("concrete".into()),
            1 => Some("abstract".into()),
            _ => None
        }
    }
    fn ast_from_symbol(&self, _delta: &ContextDelta, symbol: &str) -> SyntaxTree {
        let syntax = SyntaxTree::from_str(symbol).unwrap();
        match symbol {
            "concrete" => syntax.bind_concept(0),
            "abstract" => syntax.bind_concept(1),
            _ => syntax,
        }
    }
}

#[test]
fn basic_reduction() {
    let snapshot = BasicReductionSnapShot::default();
    let delta = ContextDelta::default();
    let cache = ContextCache::default();
    let context_search = ContextSearch::<BasicReductionSnapShot>::from((&snapshot, &delta, &cache));
    let abstract_syntax = Arc::new(SyntaxTree::from_str("abstract").unwrap().bind_concept(1));
    let concrete_syntax = Arc::new(SyntaxTree::from_str("concrete").unwrap().bind_concept(0));
    assert_eq!(context_search.recursively_reduce(&abstract_syntax), concrete_syntax);
}