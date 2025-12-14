use crate::{
    ast::GenericSyntaxTree,
    context::Context as GenericContext,
    context_cache::GenericCache,
    context_search::ContextSearch,
    context_snap_shot::{ConceptId as ContextConceptId, ContextSnapShot},
    nester::SharedReference,
    reduction_reason::ReductionReason,
    variable_mask_list::VariableMaskList,
};
use lazy_static::lazy_static;
use std::{fmt::Debug, sync::Arc};

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ArcFamily;

impl SharedReference for ArcFamily {
    type Share<T> = Arc<T>;

    fn share<T>(owned: T) -> Self::Share<T> {
        Arc::new(owned)
    }

    fn make_mut<T: Clone>(refcounter: &mut Self::Share<T>) -> &mut T {
        Arc::make_mut(refcounter)
    }
}

pub type MTSyntaxTree<CI> = GenericSyntaxTree<CI, ArcFamily>;
pub type MTContextCache<CI> = GenericCache<CI, ArcFamily>;
pub type MTVariableMaskList<CI> = VariableMaskList<CI, ArcFamily>;
pub type MTReductionReason<CI> = ReductionReason<CI, ArcFamily>;

pub type Context =
    GenericContext<ContextSnapShot<ArcFamily>, ContextConceptId, ArcFamily>;

pub type MTContextSearch<'s, 'v, S, CCI> =
    ContextSearch<'s, 'v, S, CCI, ArcFamily>;

// Saves having to construct a new `Context` each time.
lazy_static! {
    pub static ref NEW_CONTEXT: Context = Context::new().unwrap();
}

#[cfg(test)]
mod tests {
    use crate::{nester::NestedSyntaxTree, ConceptKind};

    use super::Context;

    #[test]
    fn precendence_test() {
        let ctx = Context::new().unwrap();
        let lexeme = ctx.lex("(a b) c");
        let nested_syntax = Context::nest(lexeme).unwrap();
        assert_eq!(
            nested_syntax,
            NestedSyntaxTree::from_concept_kind(&ConceptKind::New, "a".into())
                .append_node(NestedSyntaxTree::from_concept_kind(
                    &ConceptKind::New,
                    "b".into()
                ))
                .nest()
                .append_node(NestedSyntaxTree::from_concept_kind(
                    &ConceptKind::New,
                    "c".into()
                ))
        );
    }
}
