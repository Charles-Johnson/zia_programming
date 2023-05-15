use std::{fmt::Debug, collections::HashMap};

use crate::{ast::SyntaxTree, context_search::{ComparisonReason, ReductionTruthResult}, substitute::Substitutions, variable_mask_list::VariableMask, context_cache::ContextCache};

pub trait ReductionReason
where
    Self: Clone + Debug + From<ComparisonReason<Self>> + PartialEq,
{
    type Syntax: SyntaxTree;

    fn simplify_reasoning(
        reason: Option<Self>,
        reversed_reason: Option<Self>,
    ) -> ComparisonReason<Self>;

    fn determine_reduction_truth(
        left: &RRSharedSyntax<Self>,
        right: &RRSharedSyntax<Self>,
        reduce: impl Fn(&RRSharedSyntax<Self>) -> ReductionResult<Self>,
    ) -> ReductionTruthResult<Self>;

    fn recursive_reason(
        previous: Option<Self>,
        last: Self,
        previously_reduced_syntax: &RRSharedSyntax<Self>,
    ) -> Self;

    fn existence(
        substitutions: Substitutions<RRSharedSyntax<Self>>,
        generalisation: RRSharedSyntax<Self>,
    ) -> Self;

    fn inference(
        implication: RRSharedSyntax<Self>,
        reason: Self,
    ) -> Self;

    fn explicit() -> Self;

    fn default(operator_id: ReductionReasonConceptId<Self>) -> Self;

    fn rule(
        generalisation: RRSharedSyntax<Self>,
        variable_mask: VariableMask<Self::Syntax>,
        reason: Self,
    ) -> Self;

    fn partial(
        partial_reductions: HashMap<
            RRSharedSyntax<Self>,
            Reduction<Self>,
        >,
    ) -> Self;
}

pub type Reduction<RR> = (RRSharedSyntax<RR>, RR);

pub type ReductionResult<RR> = Option<Reduction<RR>>;

pub type RRSharedSyntax<RR> =
    <RRSyntax<RR> as SyntaxTree>::SharedSyntax;
type ReductionReasonConceptId<RR> =
    <RRSyntax<RR> as SyntaxTree>::ConceptId;
pub type RRSyntax<RR> = <RR as ReductionReason>::Syntax;
pub type SharedSyntax<C> = <Syntax<C> as SyntaxTree>::SharedSyntax;
pub type Syntax<C> = RRSyntax<<C as ContextCache>::RR>;

macro_rules! impl_reduction_reason {
    ($refcounter:tt, $reduction_reason:tt) => {
        use crate::{
            context_search::{
                ComparisonReason, ReductionTruthResult,
            },
            substitute::Substitutions,
            reduction_reason::Reduction,
        };
        #[derive(Clone, PartialEq, Debug)]
        pub enum $reduction_reason<S: SyntaxTree> {
            Comparison($refcounter<ComparisonReason<Self>>),
            Explicit,
            Rule {
                generalisation: S::SharedSyntax,
                variable_mask: VariableMask<S>,
                reason: $refcounter<Self>,
            },
            Inference {
                implication: S::SharedSyntax,
                reason: $refcounter<Self>,
            },
            Default {
                operator: S::ConceptId,
            },
            Partial(HashMap<S::SharedSyntax, Reduction<Self>>),
            Existence {
                substitutions: Substitutions<S::SharedSyntax>,
                generalisation: S::SharedSyntax,
            },
            Recursive {
                syntax: S::SharedSyntax,
                reason: $refcounter<Self>,
                from: $refcounter<Self>,
            },
            SyntaxCannotReduceToItself,
            LeftReducesToRight {
                reason: $refcounter<Self>,
                left: S::SharedSyntax,
                right: S::SharedSyntax,
            },
            RightReducesToLeft {
                reason: $refcounter<Self>,
                left: S::SharedSyntax,
                right: S::SharedSyntax,
            },
        }

        impl<S: SyntaxTree> From<ComparisonReason<$reduction_reason<S>>>
            for $reduction_reason<S>
        {
            fn from(
                comparison_reason: ComparisonReason<$reduction_reason<S>>,
            ) -> Self {
                Self::Comparison(comparison_reason.into())
            }
        }

        impl<S: SyntaxTree> $reduction_reason<S> {
            fn determine_evidence_of_reduction(
                left: &S::SharedSyntax,
                right: &S::SharedSyntax,
                reduce: impl Fn(&S::SharedSyntax) -> ReductionResult<Self>,
            ) -> Option<Self> {
                reduce(left).and_then(|(reduced_left, reason)| {
                    if &reduced_left == right {
                        Some(reason)
                    } else {
                        Self::determine_evidence_of_reduction(
                            &reduced_left,
                            right,
                            reduce,
                        )
                        .map(|new_reason| Self::Recursive {
                            syntax: left.clone(),
                            reason: new_reason.into(),
                            from: reason.into(),
                        })
                    }
                })
            }
        }

        impl<S: SyntaxTree> ReductionReason for $reduction_reason<S> {
            type Syntax = S;

            fn simplify_reasoning(
                reason: Option<Self>,
                reversed_reason: Option<Self>,
            ) -> ComparisonReason<Self> {
                match (&reason, &reversed_reason) {
                    (Some(Self::Comparison(cr)), rr) => match cr.as_ref() {
                        ComparisonReason::Reduction {
                            reason: comparsion_reason,
                            reversed_reason: reversed_comparison_reason,
                        } if rr == reversed_comparison_reason => {
                            ComparisonReason::Reduction {
                                reversed_reason: reversed_comparison_reason
                                    .clone(),
                                reason: comparsion_reason.clone(),
                            }
                        },
                        _ => ComparisonReason::Reduction {
                            reason,
                            reversed_reason,
                        },
                    },
                    (r, Some(Self::Comparison(cr))) => match cr.as_ref() {
                        ComparisonReason::Reduction {
                            reason: reversed_comparison_reason,
                            reversed_reason: comparison_reason,
                        } if r == comparison_reason => {
                            ComparisonReason::Reduction {
                                reversed_reason: reversed_comparison_reason
                                    .clone(),
                                reason: comparison_reason.clone(),
                            }
                        },
                        _ => ComparisonReason::Reduction {
                            reason,
                            reversed_reason,
                        },
                    },
                    _ => ComparisonReason::Reduction {
                        reason,
                        reversed_reason,
                    },
                }
            }

            fn determine_reduction_truth(
                left: &S::SharedSyntax,
                right: &S::SharedSyntax,
                reduce: impl Fn(&S::SharedSyntax) -> ReductionResult<Self>,
            ) -> ReductionTruthResult<Self> {
                if left == right {
                    Some((false, Self::SyntaxCannotReduceToItself))
                } else {
                    match (
                        Self::determine_evidence_of_reduction(
                            left, right, &reduce,
                        ),
                        Self::determine_evidence_of_reduction(
                            right, left, reduce,
                        ),
                    ) {
                        (Some(_), Some(_)) => panic!(
                            "{:#?} and {:#?} reduce to each other?!",
                            left, right
                        ),
                        (Some(reason), None) => Some((
                            true,
                            Self::LeftReducesToRight {
                                reason: reason.into(),
                                left: left.clone(),
                                right: right.clone(),
                            },
                        )),
                        (None, Some(reason)) => Some((
                            false,
                            Self::RightReducesToLeft {
                                reason: reason.into(),
                                left: left.clone(),
                                right: right.clone(),
                            },
                        )),
                        (None, None) => None,
                    }
                }
            }

            fn recursive_reason(
                previous: Option<Self>,
                last: Self,
                previously_reduced_syntax: &S::SharedSyntax,
            ) -> Self {
                previous.map_or(last.clone(), |from| Self::Recursive {
                    from: from.into(),
                    reason: last.into(),
                    syntax: previously_reduced_syntax.clone(),
                })
            }

            fn existence(
                substitutions: Substitutions<S::SharedSyntax>,
                generalisation: S::SharedSyntax,
            ) -> Self {
                Self::Existence {
                    substitutions,
                    generalisation,
                }
            }

            fn inference(implication: S::SharedSyntax, reason: Self) -> Self {
                Self::Inference {
                    implication,
                    reason: reason.into(),
                }
            }

            fn explicit() -> Self {
                Self::Explicit
            }

            fn default(operator: S::ConceptId) -> Self {
                Self::Default {
                    operator,
                }
            }

            fn rule(
                generalisation: S::SharedSyntax,
                variable_mask: VariableMask<S>,
                reason: Self,
            ) -> Self {
                Self::Rule {
                    generalisation,
                    variable_mask: variable_mask.clone(),
                    reason: reason.into(),
                }
            }

            fn partial(
                partial_reductions: HashMap<S::SharedSyntax, Reduction<Self>>,
            ) -> Self {
                debug_assert!(partial_reductions.len() > 0);
                let mut flattened_partial_reductions = hashmap! {};
                for (unreduced_concept, (reduced_concept, reduction_reason)) in
                    partial_reductions
                {
                    if let Self::Partial(inner_partial_reductions) =
                        reduction_reason
                    {
                        flattened_partial_reductions
                            .extend(inner_partial_reductions);
                    } else {
                        flattened_partial_reductions.insert(
                            unreduced_concept,
                            (reduced_concept, reduction_reason),
                        );
                    }
                }
                Self::Partial(flattened_partial_reductions)
            }
        }
    };
}