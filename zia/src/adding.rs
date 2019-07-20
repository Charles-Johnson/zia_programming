/*  Library for the Zia programming language.
    Copyright (C) 2018 to 2019 Charles Johnson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

use constants::LABEL;
use delta::Delta;
use errors::{ZiaError, ZiaResult};
use logging::Logger;
use reading::DisplayJoint;
use reading::FindWhatReducesToIt;
use reading::{FindDefinition, MaybeString, MightExpand};
use std::{fmt, rc::Rc};
use translating::SyntaxFinder;
use writing::{
    DeleteReduction, GetDefinition, GetDefinitionOf, GetNormalForm, GetReduction, InsertDefinition,
    MakeReduceFrom, MaybeConcept, NoLongerReducesFrom, RemoveReduction, SetAsDefinitionOf,
    SetDefinition, SetReduction, UpdateReduction,
};

pub trait ExecuteReduction<T>
where
    Self: ConceptMaker<T> + DeleteReduction<T> + Logger,
    T: SetReduction
        + From<Self::C>
        + From<Self::A>
        + MakeReduceFrom
        + GetDefinitionOf
        + From<String>
        + RemoveReduction
        + NoLongerReducesFrom
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString
        + FindWhatReducesToIt + Clone,
    Self::S: Container + PartialEq + DisplayJoint,
    Self::Delta: Clone + fmt::Debug,
{
    fn execute_reduction(&mut self, syntax: &Self::S, normal_form: &Self::S) -> ZiaResult<String> {
        info!(
            self.logger(),
            "execute_reduction({}, {})",
            syntax.display_joint(),
            normal_form.display_joint()
        );
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            let deltas = self.try_removing_reduction::<Self::S>(vec!(), syntax)?;
            self.apply_all(&deltas);
            Ok("".to_string())
        } else {
            let (deltas1, syntax_concept) = self.concept_from_ast(vec!(), syntax)?;
            self.apply_all(&deltas1);
            let (deltas2, normal_form_concept) = self.concept_from_ast(vec!(), normal_form)?;
            let more_deltas = self.update_reduction(&deltas2, syntax_concept, normal_form_concept)?;
            self.apply_all(&more_deltas);
            Ok("".to_string())
        }
    }
}

impl<S, T> ExecuteReduction<T> for S
where
    S: ConceptMaker<T> + DeleteReduction<T> + Logger,
    T: SetReduction
        + MakeReduceFrom
        + GetDefinitionOf
        + From<S::C>
        + From<S::A>
        + From<String>
        + RemoveReduction
        + NoLongerReducesFrom
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString
        + FindWhatReducesToIt + Clone,
    S::S: Container + PartialEq<S::S> + DisplayJoint,
    S::Delta: Clone + fmt::Debug,
{
}

pub trait Container
where
    Self: MightExpand<Self> + PartialEq<Rc<Self>> + Sized,
{
    fn contains(&self, other: &Self) -> bool {
        if let Some((ref left, ref right)) = self.get_expansion() {
            other == left || other == right || left.contains(other) || right.contains(other)
        } else {
            false
        }
    }
}

impl<T> Container for T where T: MightExpand<T> + PartialEq<Rc<T>> + Sized {}

pub trait ConceptMaker<T>
where
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + SetReduction
        + MakeReduceFrom
        + GetDefinitionOf
        + GetDefinition
        + SetDefinition
        + SetAsDefinitionOf
        + MaybeString
        + GetReduction
        + FindWhatReducesToIt
        + Clone,
    Self: Labeller<T> + GetNormalForm<T> + Logger + SyntaxFinder<T>,
    Self::S: DisplayJoint,
    Self::Delta: Clone + fmt::Debug,
{
    type S: MightExpand<Self::S> + MaybeConcept + fmt::Display;
    fn concept_from_ast(&self, deltas: Vec<Self::Delta>, ast: &Self::S) -> ZiaResult<(Vec<Self::Delta>, usize)> {
        if let Some(c) = ast.get_concept() {
            Ok((deltas, c))
        } else if let Some(c) = self.concept_from_label(&deltas, &ast.display_joint()) {
            Ok((deltas, c))
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => {
                    let (new_concept, new_deltas) = self.new_labelled_default(&deltas, string)?;
                    Ok((new_deltas, new_concept))
                }
                Some((ref left, ref right)) => {
                    let (deltas1, mut leftc) = self.concept_from_ast(deltas, left)?;
                    let (deltas2, mut rightc) = self.concept_from_ast(deltas1, right)?;
                    let (deltas3, concept) = self.find_or_insert_definition(&deltas2, leftc, rightc)?;
                    if !string.contains(' ') {
                        let deltas4 = self.label(&deltas3, concept, string)?;
                        Ok((deltas4, concept))
                    } else {
                        Ok((deltas3, concept))
                    }
                }
            }
        }
    }
}

/// Preparing a context by labelling concrete concepts.
pub trait ContextMaker<T>
where
    Self: Labeller<T> + Default + Logger,
    T: GetDefinitionOf
        + From<String>
        + From<Self::C>
        + From<Self::A>
        + SetReduction
        + MakeReduceFrom
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + MaybeString
        + FindWhatReducesToIt + Clone,
    Self::Delta: Clone + fmt::Debug,
{
    fn new() -> Self {
        let mut cont = Self::default();
        let deltas = cont.setup().unwrap();
        cont.apply_all(&deltas);
        info!(cont.logger(), "Setup a new context");
        cont
    }
}

impl<S, T> ContextMaker<T> for S
where
    S: Labeller<T> + Default + Logger,
    T: GetDefinitionOf
        + From<String>
        + From<Self::C>
        + From<Self::A>
        + SetReduction
        + MakeReduceFrom
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + MaybeString
        + FindWhatReducesToIt + Clone,
    Self::Delta: Clone + fmt::Debug,
{
}

pub trait Labeller<T>
where
    T: SetReduction
        + MakeReduceFrom
        + From<String>
        + GetDefinitionOf
        + SetDefinition
        + SetAsDefinitionOf
        + GetReduction
        + GetDefinition
        + GetReduction
        + MaybeString
        + From<Self::C>
        + From<Self::A> + Clone,
    Self: StringMaker<T> + FindOrInsertDefinition<T> + UpdateReduction<T>,
    Self::Delta: Clone + fmt::Debug,
{
    type C: Default;
    fn label(&self, previous_deltas: &[Self::Delta], concept: usize, string: &str) -> ZiaResult<Vec<Self::Delta>> {
        let (deltas, definition) = self.find_or_insert_definition(previous_deltas, LABEL, concept)?;
        let (string_id, new_deltas) = self.new_string(&deltas, string);
        self.update_reduction(&new_deltas, definition, string_id)
    }
    fn new_labelled_default(&self, previous_deltas: &[Self::Delta], string: &str) -> ZiaResult<(usize, Vec<Self::Delta>)> {
        let (new_default, deltas) = self.new_default::<Self::A>(previous_deltas);
        let more_deltas = self.label(&deltas, new_default, string)?;
        Ok((new_default, more_deltas))
    }
    fn setup(&mut self) -> ZiaResult<Vec<Self::Delta>> {
        let (label_concept, deltas1) = self.new_default::<Self::C>(&[]);
        let (define_concept, deltas2) = self.new_default::<Self::C>(&deltas1);
        let (reduction_concept, deltas3) = self.new_default::<Self::C>(&deltas2);
        let (let_concept, deltas4) = self.new_default::<Self::C>(&deltas3);
        let deltas5 = self.label(&deltas4, label_concept, "label_of")?;
        let deltas6 = self.label(&deltas5, define_concept, ":=")?;
        let deltas7 = self.label(&deltas6, reduction_concept, "->")?;
        self.label(&deltas7,let_concept, "let")
    }
}

pub trait FindOrInsertDefinition<T>
where
    T: From<Self::A>
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinitionOf + Clone,
    Self: DefaultMaker<T> + InsertDefinition<T> + FindDefinition<T>,
    Self::Delta: Clone + fmt::Debug,
{
    type A: Default;
    fn find_or_insert_definition(&self, deltas: &[Self::Delta], lefthand: usize, righthand: usize) -> ZiaResult<(Vec<Self::Delta>, usize)> {
        let pair = self.find_definition(deltas, lefthand, righthand);
        match pair {
            None => {
                let (definition, more_deltas) = self.new_default::<Self::A>(deltas);
                let new_deltas = self.insert_definition(more_deltas, definition, lefthand, righthand)?;
                Ok((new_deltas, definition))
            }
            Some(def) => Ok((deltas.to_vec(), def)),
        }
    }
}

pub trait StringMaker<T>
where
    T: From<String>,
    Self: ConceptAdderDelta<T> + StringAdderDelta,
{
    fn new_string(&self, deltas: &[Self::Delta], string: &str) -> (usize, Vec<Self::Delta>) {
        let string_concept = string.to_string().into();
        let (index, mut new_deltas) = self.add_concept_deltas(deltas, string_concept);
        let string_delta = Self::add_string_delta(index, string);
        new_deltas.push(string_delta);
        (index, new_deltas)
    }
}

impl<S, T> StringMaker<T> for S
where
    T: From<String>,
    S: ConceptAdderDelta<T> + StringAdderDelta,
{
}

pub trait DefaultMaker<T>
where
    Self: ConceptAdderDelta<T>,
{
    fn new_default<V: Default + Into<T>>(&self, deltas: &[Self::Delta]) -> (usize, Vec<Self::Delta>) {
        let concept: T = V::default().into();
        self.add_concept_deltas(deltas, concept)
    }
}

impl<S, T> DefaultMaker<T> for S where S: ConceptAdderDelta<T> {}

pub trait StringAdderDelta
where
    Self: Delta,
{
    fn add_string_delta(usize, &str) -> Self::Delta;
}

pub trait StringAdder {
    fn add_string(&mut self, usize, &str);
}

pub trait ConceptAdderDelta<T>
where
    Self: Delta,
{
    fn add_concept_deltas(&self, &[Self::Delta], T) -> (usize, Vec<Self::Delta>);
}

pub trait ConceptAdder<T> {
    fn add_concept(&mut self, T) -> usize;
}
