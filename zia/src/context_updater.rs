use crate::{
    and_also::AndAlso,
    ast::{GenericSyntaxTree, SyntaxTree},
    concepts::{ConceptTrait, ConcreteConceptType, Hand},
    context_cache::GenericCache,
    context_delta::{
        Composition, DirectConceptDelta, NestedDelta, NewConceptDelta,
        SharedDelta, ValueChange,
    },
    errors::ZiaResult,
    nester::SharedReference,
    reduction_reason::SharedSyntax,
    snap_shot::Reader,
    ZiaError,
};

use std::{fmt::Debug, marker::PhantomData};

pub struct ContextUpdater<
    'a,
    S: Reader<SDCD, SR>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>
        + Debug,
    D: SharedDelta<NestedDelta = NestedDelta<S::ConceptId, SDCD, D, SR>>,
    SR: SharedReference,
> {
    pub cache: &'a mut GenericCache<S::ConceptId, SR>,
    pub delta: &'a mut NestedDelta<S::ConceptId, SDCD, D, SR>,
    pub snap_shot: &'a S,
    pub phantom: PhantomData<D>,
    pub phantom2: PhantomData<SR>,
}

impl<
        'a,
        S: Reader<SDCD, SR>,
        SDCD: Clone
            + AsRef<DirectConceptDelta<S::ConceptId>>
            + From<DirectConceptDelta<S::ConceptId>>
            + Debug,
        D: SharedDelta<NestedDelta = NestedDelta<S::ConceptId, SDCD, D, SR>>,
        SR: SharedReference,
    > ContextUpdater<'a, S, SDCD, D, SR>
where
    GenericSyntaxTree<S::ConceptId, SR>:
        SyntaxTree<SR, ConceptId = S::ConceptId>,
{
    pub fn redefine(
        &mut self,
        concept: &S::ConceptId,
        left: &SharedSyntax<S::ConceptId, SR>,
        right: &SharedSyntax<S::ConceptId, SR>,
    ) -> ZiaResult<()> {
        if let Some((left_concept, right_concept)) =
            self.snap_shot.read_concept(self.delta, *concept).get_composition()
        {
            self.relabel(left_concept, &left.to_string())?;
            self.relabel(right_concept, &right.to_string())
        } else {
            let left_concept = self.concept_from_ast(left)?;
            let right_concept = self.concept_from_ast(right)?;
            self.insert_composition(*concept, left_concept, right_concept)
        }
    }

    pub fn cleanly_delete_composition(
        &mut self,
        concept: &S::ConceptId,
    ) -> ZiaResult<()> {
        match self
            .snap_shot
            .read_concept(self.delta, *concept)
            .get_composition()
        {
            None => Err(ZiaError::RedundantCompositionRemoval),
            Some((left, right)) => {
                self.try_delete_concept(*concept)?;
                self.try_delete_concept(left)?;
                self.try_delete_concept(right)
            },
        }
    }

    /// Returns the index of a concept labelled by `syntax` and composed of concepts from `left` and `right`.
    pub fn define_new_syntax(
        &mut self,
        syntax: &str,
        left: &SharedSyntax<S::ConceptId, SR>,
        right: &SharedSyntax<S::ConceptId, SR>,
    ) -> ZiaResult<()> {
        let new_syntax_tree = left
            .get_concept()
            .and_also(&right.get_concept())
            .and_then(|(l, r)| {
                self.snap_shot
                    .read_concept(self.delta, *l)
                    .find_as_hand_in_composition_with(*r, Hand::Left)
                    .map(|concept| {
                        let syntax =
                            GenericSyntaxTree::<S::ConceptId, SR>::from(syntax);
                        self.snap_shot
                            .bind_concept_to_syntax(self.delta, syntax, concept)
                    })
            })
            .unwrap_or_else(|| syntax.into())
            .bind_pair(left.clone(), right.clone());
        self.concept_from_ast(&new_syntax_tree)?;
        Ok(())
    }

    pub fn concept_from_ast(
        &mut self,
        ast: &GenericSyntaxTree<S::ConceptId, SR>,
    ) -> ZiaResult<S::ConceptId> {
        if let Some(c) = ast.get_concept() {
            Ok(c)
        } else if let Some(c) =
            self.snap_shot.concept_from_label(self.delta, &ast.to_string())
        {
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => {
                    let label_id = self
                        .snap_shot
                        .concrete_concept_id(
                            self.delta,
                            ConcreteConceptType::Label,
                        )
                        .ok_or(ZiaError::NoLabelConcept)?;
                    Ok(self.new_labelled_concept(string, None, Some(label_id)))
                },
                Some((ref left, ref right)) => {
                    let leftc = self.concept_from_ast(left)?;
                    let rightc = self.concept_from_ast(right)?;
                    let concept =
                        self.find_or_insert_composition(leftc, rightc);
                    if !string.contains(' ') {
                        self.label(concept, string)?;
                    }
                    Ok(concept)
                },
            }
        }
    }

    pub fn new_labelled_concept(
        &mut self,
        string: &str,
        concrete_type: Option<ConcreteConceptType>,
        label_id: Option<S::ConceptId>,
    ) -> S::ConceptId {
        debug_assert!(
            label_id.is_some()
                || concrete_type == Some(ConcreteConceptType::Label)
        );
        let new_concept_label_id = {
            let direct_delta =
                DirectConceptDelta::New(NewConceptDelta::String(string.into()));
            self.delta.update_concept_delta(direct_delta, self.cache)
        };
        let composition_id = {
            let direct_delta =
                DirectConceptDelta::New(NewConceptDelta::ReducesTo {
                    reduction: new_concept_label_id,
                });
            self.delta.update_concept_delta(direct_delta, self.cache)
        };
        {
            let direct_delta = DirectConceptDelta::New(label_id.map_or(
                NewConceptDelta::Double {
                    composition_id,
                    concrete_type,
                },
                |left_id| NewConceptDelta::Right {
                    composition_id,
                    left_id,
                    concrete_type,
                },
            ));
            self.delta.update_concept_delta(direct_delta, self.cache)
        }
    }

    pub fn relabel(
        &mut self,
        concept: S::ConceptId,
        new_label: &str,
    ) -> ZiaResult<()> {
        self.unlabel(concept)?;
        self.label(concept, new_label)
    }

    pub fn unlabel(&mut self, concept: S::ConceptId) -> ZiaResult<()> {
        let concept_of_label = self
            .snap_shot
            .get_concept_of_label(self.delta, concept)
            .expect("No label to remove");
        self.delete_reduction(concept_of_label)
    }

    pub fn delete_reduction(
        &mut self,
        concept_id: S::ConceptId,
    ) -> ZiaResult<()> {
        let reducted_concept_id = self
            .snap_shot
            .read_concept(self.delta, concept_id)
            .remove_reduction()?;
        // update self.delta to include deletion of composition
        // and invalidate cache
        self.delta.update_concept_delta(
            DirectConceptDelta::Reduce {
                change: ValueChange::Remove(reducted_concept_id),
                unreduced_id: concept_id,
            },
            self.cache,
        );
        Ok(())
    }

    pub fn label(
        &mut self,
        concept: S::ConceptId,
        string: &str,
    ) -> ZiaResult<()> {
        let label_id = self
            .snap_shot
            .concrete_concept_id(self.delta, ConcreteConceptType::Label)
            .ok_or(ZiaError::NoLabelConcept)?;
        let composition = self.find_or_insert_composition(label_id, concept);
        let string_id = self.new_string(string);
        self.update_reduction(composition, string_id)
    }

    pub fn update_reduction(
        &mut self,
        concept: S::ConceptId,
        reduction: S::ConceptId,
    ) -> ZiaResult<()> {
        let maybe_normal_form =
            self.snap_shot.get_normal_form(self.delta, reduction);
        if maybe_normal_form == Some(concept) {
            Err(ZiaError::CyclicReduction)
        } else if let Some(result) = self
            .snap_shot
            .read_concept(self.delta, concept)
            .get_reduction()
            .and_then(|r| {
                (r == reduction).then_some(Err(ZiaError::RedundantReduction))
            })
        {
            result
        } else if reduction
            == self.snap_shot.get_reduction_of_composition(self.delta, concept)
        {
            Err(ZiaError::RedundantReduction)
        } else {
            let change = maybe_normal_form.map_or_else(
                || ValueChange::Create(reduction),
                |before| ValueChange::Update {
                    before,
                    after: reduction,
                },
            );
            let delta = DirectConceptDelta::Reduce {
                unreduced_id: concept,
                change,
            };
            self.delta.update_concept_delta(delta, self.cache);
            Ok(())
        }
    }

    fn new_string(
        &mut self,
        string: impl Into<String> + Clone,
    ) -> S::ConceptId {
        self.delta.update_concept_delta(
            DirectConceptDelta::New(NewConceptDelta::String(string.into())),
            self.cache,
        )
    }

    pub fn find_or_insert_composition(
        &mut self,
        lefthand: S::ConceptId,
        righthand: S::ConceptId,
    ) -> S::ConceptId {
        let pair = self
            .snap_shot
            .read_concept(self.delta, lefthand)
            .find_as_hand_in_composition_with(righthand, Hand::Left);
        match pair {
            None => self.delta.update_concept_delta(
                DirectConceptDelta::New(NewConceptDelta::Composition(
                    Composition {
                        left_id: lefthand,
                        right_id: righthand,
                    },
                )),
                self.cache,
            ),
            Some(def) => def,
        }
    }

    pub fn insert_composition(
        &mut self,
        composition: S::ConceptId,
        lefthand: S::ConceptId,
        righthand: S::ConceptId,
    ) -> ZiaResult<()> {
        if self.snap_shot.contains(self.delta, lefthand, composition)
            || self.snap_shot.contains(self.delta, righthand, composition)
        {
            Err(ZiaError::InfiniteComposition)
        } else {
            self.snap_shot.check_reductions(
                self.delta,
                composition,
                lefthand,
            )?;
            self.snap_shot.check_reductions(
                self.delta,
                composition,
                righthand,
            )?;
            let composition_concept =
                self.snap_shot.read_concept(self.delta, composition);
            self.delta.update_concept_delta(
                composition_concept.compose_delta(lefthand, righthand)?,
                self.cache,
            );
            Ok(())
        }
    }

    fn remove_string(&mut self, string: &str) {
        let index = self
            .delta
            .get_string(string)
            .and_then(|sd| match sd {
                ValueChange::Update {
                    after,
                    ..
                } => Some(after),
                ValueChange::Create(index) => Some(index),
                ValueChange::Remove(_) => None,
            })
            .expect("string already removed or doesn't exist");
        self.blindly_remove_concept(index);
    }

    pub fn blindly_remove_concept(&mut self, id: S::ConceptId) {
        self.delta
            .update_concept_delta(DirectConceptDelta::Remove(id), self.cache);
    }

    pub fn try_delete_concept(
        &mut self,
        concept: S::ConceptId,
    ) -> ZiaResult<()> {
        if self.snap_shot.is_disconnected(self.delta, concept) {
            self.unlabel(concept)?;
            self.remove_concept(concept);
        }
        Ok(())
    }

    fn remove_concept(&mut self, concept: S::ConceptId) {
        if let Some(ref s) =
            self.snap_shot.read_concept(self.delta, concept).get_string()
        {
            self.remove_string(s);
        }
        self.blindly_remove_concept(concept);
    }
}
