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
use delta::{ApplyDelta, Delta};
use errors::{ZiaError, ZiaResult};
use logging::Logger;
use reading::FindWhatReducesToIt;
use reading::{FindDefinition, MaybeString, MightExpand};
use std::{fmt, rc::Rc};
use translating::SyntaxFinder;
use writing::{
    DeleteReduction, GetDefinition, GetDefinitionOf, GetNormalForm, GetReduction, InsertDefinition,
    MakeReduceFrom, MaybeConcept, SetAsDefinitionOf, SetDefinition, SetReduction, UpdateReduction,
};

pub trait ExecuteReduction<T, U>
where
    Self: ConceptMaker<T, U> + DeleteReduction<T, U>,
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
        + FindWhatReducesToIt
        + Clone,
    U: Container + PartialEq + MaybeConcept + fmt::Display,
    Self::Delta: Clone + fmt::Debug + Default + Delta,
{
    fn execute_reduction(
        &self,
        deltas: &mut Self::Delta,
        syntax: &U,
        normal_form: &U,
    ) -> ZiaResult<()> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            self.try_removing_reduction(deltas, syntax)
        } else {
            let syntax_concept = self.concept_from_ast(deltas, syntax)?;
            let normal_form_concept = self.concept_from_ast(deltas, normal_form)?;
            self.update_reduction(deltas, syntax_concept, normal_form_concept)
        }
    }
}

impl<S, T, U> ExecuteReduction<T, U> for S
where
    S: ConceptMaker<T, U> + DeleteReduction<T, U>,
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
        + FindWhatReducesToIt
        + Clone,
    U: Container + PartialEq + MaybeConcept + fmt::Display,
    S::Delta: Clone + fmt::Debug + Default + Delta,
{
}

pub trait Container
where
    Self: MightExpand + PartialEq<Rc<Self>> + Sized,
{
    fn contains(&self, other: &Self) -> bool {
        if let Some((ref left, ref right)) = self.get_expansion() {
            other == left || other == right || left.contains(other) || right.contains(other)
        } else {
            false
        }
    }
}

impl<T> Container for T where T: MightExpand + PartialEq<Rc<T>> + Sized {}

pub trait ConceptMaker<T, U>
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
    Self::Delta: Clone + fmt::Debug + Default + Delta,
    U: MightExpand + MaybeConcept + fmt::Display,
{
    fn concept_from_ast(&self, deltas: &mut Self::Delta, ast: &U) -> ZiaResult<usize> {
        if let Some(c) = ast.get_concept() {
            Ok(c)
        } else if let Some(c) = self.concept_from_label(deltas, &ast.to_string()) {
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => self.new_labelled_default(deltas, string),
                Some((ref left, ref right)) => {
                    let leftc = self.concept_from_ast(deltas, left)?;
                    let rightc = self.concept_from_ast(deltas, right)?;
                    let ls = left.to_string();
                    let rs = right.to_string();
                    let concept = self.find_or_insert_definition(
                        deltas,
                        leftc,
                        rightc,
                        ls.starts_with('_') && ls.ends_with('_')
                            || rs.starts_with('_') && rs.ends_with('_'),
                    )?;
                    if !string.contains(' ') {
                        self.label(deltas, concept, string)?;
                    }
                    Ok(concept)
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
        + FindWhatReducesToIt
        + Clone,
    Self::Delta: Clone + fmt::Debug + Default + Delta,
{
    fn new() -> Self {
        let mut cont = Self::default();
        let delta = cont.setup().unwrap();
        info!(cont.logger(), "Setup a new context: {:?}", &delta);
        cont.apply(delta);
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
        + FindWhatReducesToIt
        + Clone,
    Self::Delta: Clone + fmt::Debug + Default + Delta,
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
        + From<Self::A>
        + Clone,
    Self: StringMaker<T> + FindOrInsertDefinition<T> + UpdateReduction<T>,
    Self::Delta: Clone + fmt::Debug + Sized + Default + Delta,
{
    type C: Default;
    fn label(&self, deltas: &mut Self::Delta, concept: usize, string: &str) -> ZiaResult<()> {
        if string.starts_with('_') && string.ends_with('_') {
            Ok(())
        } else {
            let definition = self.find_or_insert_definition(deltas, LABEL, concept, false)?;
            let string_id = self.new_string(deltas, string);
            self.update_reduction(deltas, definition, string_id)
        }
    }
    fn new_labelled_default(&self, deltas: &mut Self::Delta, string: &str) -> ZiaResult<usize> {
        let new_default =
            self.new_default::<Self::A>(deltas, string.starts_with('_') && string.ends_with('_'));
        self.label(deltas, new_default, string)?;
        Ok(new_default)
    }
    fn setup(&mut self) -> ZiaResult<Self::Delta> {
        let mut delta = Self::Delta::default();
        let concrete_constructor =
            |local_delta: &mut Self::Delta| self.new_default::<Self::C>(local_delta, false);
        let labels = vec![
            "label_of", ":=", "->", "let", "true", "false", "assoc", "right", "left", ">-",
        ];
        let concepts = delta.repeat(concrete_constructor, labels.len());
        let label = |local_delta: &mut Self::Delta, concept: usize, string: &str| {
            self.label(local_delta, concept, string)
        };
        delta.multiply(label, concepts, labels)?;
        Ok(delta)
    }
}

pub trait FindOrInsertDefinition<T>
where
    T: From<Self::A>
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinitionOf
        + Clone,
    Self: DefaultMaker<T> + InsertDefinition<T> + FindDefinition<T>,
    Self::Delta: Clone + fmt::Debug + Delta,
{
    type A: Default;
    fn find_or_insert_definition(
        &self,
        deltas: &mut Self::Delta,
        lefthand: usize,
        righthand: usize,
        variable: bool,
    ) -> ZiaResult<usize> {
        let pair = self.find_definition(deltas, lefthand, righthand);
        match pair {
            None => {
                let definition = self.new_default::<Self::A>(deltas, variable);
                self.insert_definition(deltas, definition, lefthand, righthand)?;
                Ok(definition)
            }
            Some(def) => Ok(def),
        }
    }
}

pub trait StringMaker<T>
where
    T: From<String>,
    Self: ConceptAdderDelta<T> + StringAdderDelta,
    Self::Delta: Delta,
{
    fn new_string(&self, original_delta: &mut Self::Delta, string: &str) -> usize {
        let string_concept = string.to_string().into();
        let (delta, index) = self.add_concept_delta(original_delta, string_concept, false);
        original_delta.combine(delta);
        let string_delta = Self::add_string_delta(index, string);
        original_delta.combine(string_delta);
        index
    }
}

impl<S, T> StringMaker<T> for S
where
    T: From<String>,
    S: ConceptAdderDelta<T> + StringAdderDelta,
    S::Delta: Delta,
{
}

pub trait DefaultMaker<T>
where
    Self: ConceptAdderDelta<T>,
    Self::Delta: Delta,
{
    fn new_default<V: Default + Into<T>>(
        &self,
        original_delta: &mut Self::Delta,
        variable: bool,
    ) -> usize {
        let concept: T = V::default().into();
        let (delta, index) = self.add_concept_delta(original_delta, concept, variable);
        original_delta.combine(delta);
        index
    }
}

impl<S, T> DefaultMaker<T> for S
where
    S: ConceptAdderDelta<T>,
    S::Delta: Delta,
{
}

pub trait StringAdderDelta
where
    Self: ApplyDelta,
{
    fn add_string_delta(usize, &str) -> Self::Delta;
}

pub trait StringAdder {
    fn add_string(&mut self, usize, &str);
}

pub trait ConceptAdderDelta<T>
where
    Self: ApplyDelta,
{
    fn add_concept_delta(&self, &Self::Delta, T, bool) -> (Self::Delta, usize);
}

pub trait ConceptAdder<T> {
    fn add_concept(&mut self, T);
}
