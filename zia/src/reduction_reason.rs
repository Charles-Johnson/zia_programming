use std::{collections::HashMap, fmt::Debug, hash::Hash};

use maplit::hashmap;

use crate::{
    ast::{GenericSyntaxTree, SyntaxKey},
    context_search::{ComparisonReason, ReductionTruthResult},
    mixed_concept::ConceptId,
    nester::SharedReference,
    substitute::Substitutions,
    variable_mask_list::VariableMask,
};

pub trait ReductionReason<CI: ConceptId, SR: SharedReference>
where
    Self: Clone + From<ComparisonReason<CI, SR>>,
{
    fn simplify_reasoning(
        reason: Option<Self>,
        reversed_reason: Option<Self>,
    ) -> ComparisonReason<CI, SR>;

    fn determine_reduction_truth(
        left: &SharedSyntax<CI, SR>,
        right: &SharedSyntax<CI, SR>,
        reduce: impl Fn(&SharedSyntax<CI, SR>) -> ReductionResult<CI, SR>,
    ) -> ReductionTruthResult<Self>;

    fn recursive_reason(
        previous: Option<Self>,
        last: Self,
        previously_reduced_syntax: &SharedSyntax<CI, SR>,
    ) -> Self;

    fn existence(
        substitutions: Substitutions<CI, SR>,
        generalisation: SharedSyntax<CI, SR>,
    ) -> Self;

    fn inference(implication: SharedSyntax<CI, SR>, reason: Self) -> Self;

    fn explicit() -> Self;

    fn default(operator_id: CI) -> Self;

    fn rule(
        generalisation: SharedSyntax<CI, SR>,
        variable_mask: VariableMask<CI, SR>,
        reason: Self,
    ) -> Self;

    fn partial(
        partial_reductions: HashMap<SyntaxKey<CI>, Reduction<CI, SR>>,
    ) -> Self;
}

pub type Reduction<CI, SR> =
    (SharedSyntax<CI, SR>, GenericReductionReason<CI, SR>);

pub type ReductionResult<CI, SR> = Option<Reduction<CI, SR>>;

pub type SharedSyntax<CCI, SR> =
    <SR as SharedReference>::Share<GenericSyntaxTree<CCI, SR>>;
type PartialReductionReasons<CI, SR> =
    HashMap<SyntaxKey<CI>, Reduction<CI, SR>>;
#[derive(Clone)]
pub enum GenericReductionReason<CI: ConceptId, SR: SharedReference> {
    Comparison(SR::Share<ComparisonReason<CI, SR>>),
    Explicit,
    Rule {
        generalisation: SharedSyntax<CI, SR>,
        variable_mask: VariableMask<CI, SR>,
        reason: SR::Share<Self>,
    },
    Inference {
        implication: SharedSyntax<CI, SR>,
        reason: SR::Share<Self>,
    },
    Default {
        operator: CI,
    },
    Partial(PartialReductionReasons<CI, SR>),
    Existence {
        substitutions: Substitutions<CI, SR>,
        generalisation: SharedSyntax<CI, SR>,
    },
    Recursive {
        syntax: SharedSyntax<CI, SR>,
        reason: SR::Share<Self>,
        from: SR::Share<Self>,
    },
    SyntaxCannotReduceToItself,
    LeftReducesToRight {
        reason: SR::Share<Self>,
        left: SharedSyntax<CI, SR>,
        right: SharedSyntax<CI, SR>,
    },
    RightReducesToLeft {
        reason: SR::Share<Self>,
        left: SharedSyntax<CI, SR>,
        right: SharedSyntax<CI, SR>,
    },
}

impl<CI: ConceptId, SR: SharedReference> Eq for GenericReductionReason<CI, SR> {}

impl<CI: ConceptId, SR: SharedReference> Debug
    for GenericReductionReason<CI, SR>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comparison(arg0) => {
                f.debug_tuple("Comparison").field(arg0.as_ref()).finish()
            },
            Self::Explicit => write!(f, "Explicit"),
            Self::Rule {
                generalisation,
                variable_mask,
                reason,
            } => f
                .debug_struct("Rule")
                .field("generalisation", generalisation.as_ref())
                .field(
                    "variable_mask",
                    &convert_to_syntax_keys::<CI, CI, SR>(variable_mask),
                )
                .field("reason", reason.as_ref())
                .finish(),
            Self::Inference {
                implication,
                reason,
            } => f
                .debug_struct("Inference")
                .field("implication", implication.as_ref())
                .field("reason", reason.as_ref())
                .finish(),
            Self::Default {
                operator,
            } => f.debug_struct("Default").field("operator", operator).finish(),
            Self::Partial(arg0) => f
                .debug_tuple("Partial")
                .field(
                    &arg0
                        .iter()
                        .map(|(k, (v0, v1))| (k, (v0.as_ref(), v1)))
                        .collect::<HashMap<_, _>>(),
                )
                .finish(),
            Self::Existence {
                substitutions,
                generalisation,
            } => f
                .debug_struct("Existence")
                .field(
                    "substitutions",
                    &convert_to_syntax_keys::<SyntaxKey<CI>, CI, SR>(
                        substitutions,
                    ),
                )
                .field("generalisation", generalisation.as_ref())
                .finish(),
            Self::Recursive {
                syntax,
                reason,
                from,
            } => f
                .debug_struct("Recursive")
                .field("syntax", syntax.as_ref())
                .field("reason", reason.as_ref())
                .field("from", from.as_ref())
                .finish(),
            Self::SyntaxCannotReduceToItself => {
                write!(f, "SyntaxCannotReduceToItself")
            },
            Self::LeftReducesToRight {
                reason,
                left,
                right,
            } => f
                .debug_struct("LeftReducesToRight")
                .field("reason", reason.as_ref())
                .field("left", left.as_ref())
                .field("right", right.as_ref())
                .finish(),
            Self::RightReducesToLeft {
                reason,
                left,
                right,
            } => f
                .debug_struct("RightReducesToLeft")
                .field("reason", reason.as_ref())
                .field("left", left.as_ref())
                .field("right", right.as_ref())
                .finish(),
        }
    }
}
// TODO: implement more efficient way of comparing HashMaps with SharedSyntax values
pub fn convert_to_syntax_keys<
    K: Eq + Hash + Clone,
    CI: ConceptId,
    SR: SharedReference,
>(
    h: &HashMap<K, SharedSyntax<CI, SR>>,
) -> HashMap<K, SyntaxKey<CI>> {
    h.iter().map(|(k, v)| (k.clone(), v.key())).collect()
}

type PartialReductionReasonKeys<CI, SR> =
    HashMap<SyntaxKey<CI>, (SyntaxKey<CI>, GenericReductionReason<CI, SR>)>;

// TODO: implement more efficient way of comparing PartialReductionReasons
fn collect_to_syntax_keys<CI: ConceptId, SR: SharedReference>(
    h: &PartialReductionReasons<CI, SR>,
) -> PartialReductionReasonKeys<CI, SR> {
    h.iter()
        .map(|(k, (v1, v2))| (k.clone(), (v1.key(), v2.clone())))
        .collect::<HashMap<_, _>>()
}
impl<CI: ConceptId, SR: SharedReference> PartialEq
    for GenericReductionReason<CI, SR>
{
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Comparison(c1) => {
                matches!(other, Self::Comparison(c2) if c1.as_ref() == c2.as_ref())
            },
            Self::Explicit => matches!(other, Self::Explicit),
            Self::Rule {
                generalisation: g1,
                variable_mask: vm1,
                reason: r1,
            } => {
                matches!(other, Self::Rule{generalisation: g2, variable_mask: vm2,
            reason:r2} if g1.key() == g2.key() && convert_to_syntax_keys::<CI, CI, SR>(vm1) == convert_to_syntax_keys::<CI, CI, SR>(vm2) && r1.as_ref() == r2.as_ref())
            },
            Self::Inference {
                implication: i1,
                reason: r1,
            } => {
                matches!(other, Self::Inference {implication:i2, reason:r2} if i1.as_ref() == i2.as_ref() && r1.as_ref() == r2.as_ref())
            },
            Self::Default {
                operator: o1,
            } => matches!(other, Self::Default { operator: o2 } if o1 == o2),
            Self::Partial(p1) => {
                matches!(other, Self::Partial(p2) if collect_to_syntax_keys(p1) == collect_to_syntax_keys(p2))
            },
            Self::Existence {
                substitutions: s1,
                generalisation: g1,
            } => {
                matches!(other, Self::Existence { substitutions: s2, generalisation: g2 } if convert_to_syntax_keys::<SyntaxKey<CI>, CI, SR>(s1)== convert_to_syntax_keys::<SyntaxKey<CI>, CI, SR>(s2) && g1.key() == g2.key())
            },
            Self::Recursive {
                syntax: s1,
                reason: r1,
                from: f1,
            } => {
                matches!(other, Self::Recursive { syntax: s2, reason: r2, from: f2 } if s1.key() == s2.key() && r1.as_ref() == r2.as_ref() && f1.as_ref() == f2.as_ref())
            },
            Self::SyntaxCannotReduceToItself => todo!(),
            Self::LeftReducesToRight {
                reason: _,
                left: _,
                right: _,
            } => todo!(),
            Self::RightReducesToLeft {
                reason: r1,
                left: l1,
                right: right1,
            } => {
                matches!(other, Self::RightReducesToLeft { reason: r2, left: l2, right: right2 } if r1.as_ref() == r2.as_ref() && l1.key() == l2.key() && right1.key() == right2.key())
            },
        }
    }
}

impl<CI: ConceptId, SR: SharedReference> From<ComparisonReason<CI, SR>>
    for GenericReductionReason<CI, SR>
{
    fn from(comparison_reason: ComparisonReason<CI, SR>) -> Self {
        Self::Comparison(SR::share(comparison_reason))
    }
}

impl<CI: ConceptId, SR: SharedReference> GenericReductionReason<CI, SR> {
    fn determine_evidence_of_reduction(
        left: &SharedSyntax<CI, SR>,
        right: &SharedSyntax<CI, SR>,
        reduce: impl Fn(&SharedSyntax<CI, SR>) -> ReductionResult<CI, SR>,
    ) -> Option<Self> {
        reduce(left).and_then(|(reduced_left, reason)| {
            if reduced_left.key() == right.key() {
                Some(reason)
            } else {
                Self::determine_evidence_of_reduction(
                    &reduced_left,
                    right,
                    reduce,
                )
                .map(|new_reason| Self::Recursive {
                    syntax: left.clone(),
                    reason: SR::share(new_reason),
                    from: SR::share(reason),
                })
            }
        })
    }
}

impl<CI: ConceptId, SR: SharedReference> ReductionReason<CI, SR>
    for GenericReductionReason<CI, SR>
{
    fn simplify_reasoning(
        reason: Option<Self>,
        reversed_reason: Option<Self>,
    ) -> ComparisonReason<CI, SR> {
        match (&reason, &reversed_reason) {
            (Some(Self::Comparison(cr)), rr) => match cr.as_ref() {
                ComparisonReason::Reduction {
                    reason: comparsion_reason,
                    reversed_reason: reversed_comparison_reason,
                } if rr == reversed_comparison_reason => {
                    ComparisonReason::Reduction {
                        reversed_reason: reversed_comparison_reason.clone(),
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
                } if r == comparison_reason => ComparisonReason::Reduction {
                    reversed_reason: reversed_comparison_reason.clone(),
                    reason: comparison_reason.clone(),
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
        left: &SharedSyntax<CI, SR>,
        right: &SharedSyntax<CI, SR>,
        reduce: impl Fn(&SharedSyntax<CI, SR>) -> ReductionResult<CI, SR>,
    ) -> ReductionTruthResult<Self> {
        if left.key() == right.key() {
            Some((false, Self::SyntaxCannotReduceToItself))
        } else {
            match (
                Self::determine_evidence_of_reduction(left, right, &reduce),
                Self::determine_evidence_of_reduction(right, left, reduce),
            ) {
                (Some(_), Some(_)) => panic!(
                    "{:#?} and {:#?} reduce to each other?!",
                    left.as_ref(),
                    right.as_ref()
                ),
                (Some(reason), None) => Some((
                    true,
                    Self::LeftReducesToRight {
                        reason: SR::share(reason),
                        left: left.clone(),
                        right: right.clone(),
                    },
                )),
                (None, Some(reason)) => Some((
                    false,
                    Self::RightReducesToLeft {
                        reason: SR::share(reason),
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
        previously_reduced_syntax: &SharedSyntax<CI, SR>,
    ) -> Self {
        previous.map_or(last.clone(), |from| Self::Recursive {
            from: SR::share(from),
            reason: SR::share(last),
            syntax: previously_reduced_syntax.clone(),
        })
    }

    fn existence(
        substitutions: Substitutions<CI, SR>,
        generalisation: SharedSyntax<CI, SR>,
    ) -> Self {
        Self::Existence {
            substitutions,
            generalisation,
        }
    }

    fn inference(implication: SharedSyntax<CI, SR>, reason: Self) -> Self {
        Self::Inference {
            implication,
            reason: SR::share(reason),
        }
    }

    fn explicit() -> Self {
        Self::Explicit
    }

    fn default(operator: CI) -> Self {
        Self::Default {
            operator,
        }
    }

    fn rule(
        generalisation: SharedSyntax<CI, SR>,
        variable_mask: VariableMask<CI, SR>,
        reason: Self,
    ) -> Self {
        Self::Rule {
            generalisation,
            variable_mask,
            reason: SR::share(reason),
        }
    }

    fn partial(
        partial_reductions: HashMap<SyntaxKey<CI>, Reduction<CI, SR>>,
    ) -> Self {
        debug_assert!(!partial_reductions.is_empty());
        let mut flattened_partial_reductions = hashmap! {};
        for (unreduced_concept, (reduced_concept, reduction_reason)) in
            partial_reductions
        {
            if let Self::Partial(inner_partial_reductions) = reduction_reason {
                flattened_partial_reductions.extend(inner_partial_reductions);
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
