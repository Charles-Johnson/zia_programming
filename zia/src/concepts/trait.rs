use std::{
    fmt::{Debug, Display},
    hash::Hash,
    iter::Map,
};

use crate::{
    context_delta::{
        Composition, DirectConceptDelta, IndirectConceptDelta, ValueChange,
    },
    errors::ZiaResult,
    ZiaError,
};

use super::{
    CompositePart, ConcreteConceptType, Hand, LeafCharacter, MaybeComposition,
};

pub trait Concept: Sized {
    type Id: Copy + Display + Eq + Hash + Debug;
    type IdPairIterator<'a>: Iterator<Item = (Self::Id, Self::Id)>
    where
        Self: 'a;
    type IdIterator<'a>: Iterator<Item = Self::Id>
    where
        Self: 'a;
    type OwnedIdPairIterator: Iterator<Item = (Self::Id, Self::Id)> + 'static;
    fn id(&self) -> Self::Id;

    fn maybe_composition(&self) -> Option<MaybeComposition<Self::Id>>;

    fn compose_delta(
        &self,
        left_id: Self::Id,
        right_id: Self::Id,
    ) -> crate::errors::ZiaResult<
        crate::context_delta::DirectConceptDelta<Self::Id>,
    > {
        let after = Composition {
            left_id,
            right_id,
        };
        self.maybe_composition().map_or(
            Err(ZiaError::SettingCompositionOfConcrete),
            |mc| {
                Ok(DirectConceptDelta::Compose {
                    change: match mc {
                        MaybeComposition::Composition(CompositePart {
                            lefthand,
                            righthand,
                            ..
                        }) => ValueChange::Update {
                            before: Composition {
                                left_id: lefthand,
                                right_id: righthand,
                            },
                            after,
                        },
                        MaybeComposition::Leaf(LeafCharacter::Constant) => {
                            ValueChange::Create(after)
                        },
                        MaybeComposition::Leaf(_) => {
                            panic!(
                                "Not sure what you are trying to do here ..."
                            )
                        },
                    },
                    composition_id: self.id(),
                })
            },
        )
    }

    fn change_reduction(&mut self, change: ValueChange<Self::Id>);

    fn apply_indirect(&mut self, delta: &IndirectConceptDelta<Self::Id>);

    /// Either a free variable or a concept that is composed of at least one free variable
    /// without a quantifying operator like `exists_such_that`
    fn free_variable(&self) -> bool;

    /// Either a bounded variable or a concept that is composed of at least one bounded variable
    /// without a quantifying operator like `exists_such_that`
    fn bounded_variable(&self) -> bool;

    /// Either a free or bounded variable or a concept that is composed of at least one bounded or free variable
    /// without a quantifying operator like `exists_such_that`
    fn anonymous_variable(&self) -> bool;

    fn find_what_reduces_to_it(&self) -> Self::IdIterator<'_>;

    /// Gets the `String` value associated with `self` if it is a string concept. Otherwise returns `None`.
    fn get_string(&self) -> Option<String>;

    /// Iterates over the IDs of concepts which this concept is the `hand` of
    fn iter_composition_ids(
        &self,
        hand: Hand,
    ) -> Map<Self::IdPairIterator<'_>, SelectElementFromPairFn<Self::Id>> {
        // the argument is dropped in the function but destructors cannot run in const contexts
        #[allow(clippy::missing_const_for_fn)]
        fn f<Id>(p: (Id, Id)) -> Id {
            p.1
        }
        self.iter_hand_of(hand).map(f::<Self::Id>)
    }
    fn into_iter_composition_ids(
        self,
        hand: Hand,
    ) -> Map<Self::OwnedIdPairIterator, SelectElementFromPairFn<Self::Id>> {
        // the argument is dropped in the function but destructors cannot run in const contexts
        #[allow(clippy::missing_const_for_fn)]
        fn f<Id>(p: (Id, Id)) -> Id {
            p.1
        }
        self.into_iter_hand_of(hand).map(f::<Self::Id>)
    }

    fn iter_hand_of(&self, hand: Hand) -> Self::IdPairIterator<'_>;
    fn into_iter_hand_of(self, hand: Hand) -> Self::OwnedIdPairIterator;

    /// Gets the index of the concept that `self` may reduce to.
    fn get_reduction(&self) -> Option<Self::Id>;

    /// If concept is abstract and has a composition returns the indices of the left and right concepts that compose it as `Some((left, right))`. Otherwise returns `None`.
    fn get_composition(&self) -> Option<(Self::Id, Self::Id)>;

    fn change_composition(
        &mut self,
        change: ValueChange<[&mut Self; 2]>,
    ) -> ZiaResult<()>;

    /// Returns the ID of the composition of `self.id()` and `other_id`, if exists where `hand` is the hand of `self.id()` in the composition
    fn find_as_hand_in_composition_with(
        &self,
        other_id: Self::Id,
        hand: Hand,
    ) -> Option<Self::Id>;

    fn get_concrete_concept_type(&self) -> Option<ConcreteConceptType>;
}

type SelectElementFromPairFn<T> = fn((T, T)) -> T;
