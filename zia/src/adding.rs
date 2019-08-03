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
    MakeReduceFrom, MaybeConcept, SetAsDefinitionOf,
    SetDefinition, SetReduction, UpdateReduction,
};

pub trait ExecuteReduction<T>
where
    Self: ConceptMaker<T> + DeleteReduction<T>,
    T: SetReduction
        + From<Self::C>
        + From<Self::A>
        + MakeReduceFrom
        + GetDefinitionOf
        + From<String>
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString
        + FindWhatReducesToIt + Clone,
    Self::S: Container + PartialEq + DisplayJoint,
    Self::Delta: Clone + fmt::Debug,
{
    fn execute_reduction(&self, deltas: Vec<Self::Delta>, syntax: &Self::S, normal_form: &Self::S) -> ZiaResult<Vec<Self::Delta>> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            self.try_removing_reduction::<Self::S>(deltas, syntax)
        } else {
            let (deltas1, syntax_concept) = self.concept_from_ast(deltas, syntax)?;
            let (deltas2, normal_form_concept) = self.concept_from_ast(deltas1, normal_form)?;
            self.update_reduction(&deltas2, syntax_concept, normal_form_concept)
        }
    }
}

impl<S, T> ExecuteReduction<T> for S
where
    S: ConceptMaker<T> + DeleteReduction<T>,
    T: SetReduction
        + MakeReduceFrom
        + GetDefinitionOf
        + From<S::C>
        + From<S::A>
        + From<String>
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
                None => self.new_labelled_default(deltas, string),
                Some((ref left, ref right)) => {
                    let (deltas1, leftc) = self.concept_from_ast(deltas, left)?;
                    let (deltas2, rightc) = self.concept_from_ast(deltas1, right)?;
                    let (deltas3, concept) = self.find_or_insert_definition(deltas2, leftc, rightc)?;
                    Ok((if !string.contains(' ') {
                        self.label(deltas3, concept, string)?
                    } else {
                        deltas3
                    }, concept))
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
        info!(cont.logger(), "Setup a new context: {:?}", deltas);
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
    fn label(&self, previous_deltas: Vec<Self::Delta>, concept: usize, string: &str) -> ZiaResult<Vec<Self::Delta>> {
        let (deltas, definition) = self.find_or_insert_definition(previous_deltas, LABEL, concept)?;
        let (string_id, new_deltas) = self.new_string(deltas, string);
        self.update_reduction(&new_deltas, definition, string_id)
    }
    fn new_labelled_default(&self, mut previous_deltas: Vec<Self::Delta>, string: &str) -> ZiaResult<(Vec<Self::Delta>, usize)> {
        let new_default = self.new_default::<Self::A>(&mut previous_deltas);
        let more_deltas = self.label(previous_deltas, new_default, string)?;
        Ok((more_deltas, new_default))
    }
    fn setup(&mut self) -> ZiaResult<Vec<Self::Delta>> {
        let mut deltas = vec![];
        let concrete_constructor = |x: &mut Vec<Self::Delta>| self.new_default::<Self::C>(x);
        let concepts = Self::repeat(&mut deltas, concrete_constructor, 4);
        let deltas5 = self.label(deltas, concepts[0], "label_of")?;
        let deltas6 = self.label(deltas5, concepts[1], ":=")?;
        let deltas7 = self.label(deltas6, concepts[2], "->")?;
        self.label(deltas7, concepts[3], "let")
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
    fn find_or_insert_definition(&self, mut deltas: Vec<Self::Delta>, lefthand: usize, righthand: usize) -> ZiaResult<(Vec<Self::Delta>, usize)> {
        let pair = self.find_definition(&deltas, lefthand, righthand);
        match pair {
            None => {
                let definition = self.new_default::<Self::A>(&mut deltas);
                let new_deltas = self.insert_definition(deltas, definition, lefthand, righthand)?;
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
    fn new_string(&self, mut deltas: Vec<Self::Delta>, string: &str) -> (usize, Vec<Self::Delta>) {
        let string_concept = string.to_string().into();
        let (delta, index) = self.add_concept_delta(&deltas, string_concept);
        deltas.push(delta);
        let string_delta = Self::add_string_delta(index, string);
        deltas.push(string_delta);
        (index, deltas)
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
    fn new_default<V: Default + Into<T>>(&self, deltas: &mut Vec<Self::Delta>) -> usize {
        let concept: T = V::default().into();
        let (delta, index) = self.add_concept_delta(deltas, concept);
        deltas.push(delta);
        index
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
    fn add_concept_delta(&self, &[Self::Delta], T) -> (Self::Delta, usize);
}

pub trait ConceptAdder<T> {
    fn add_concept(&mut self, T) -> usize;
}
