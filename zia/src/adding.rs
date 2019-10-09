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

use delta::{ApplyDelta, Delta};
use errors::{ZiaError, ZiaResult};
use logging::Logger;
use reading::FindWhatReducesToIt;
use reading::{FindDefinition, MaybeString, MightExpand};
use std::{fmt, rc::Rc};
use translating::SyntaxFinder;
use writing::{
    DeleteReduction, GetDefinition, GetDefinitionOf, GetReduction, InsertDefinition,
    MakeReduceFrom, MaybeConcept, SetAsDefinitionOf, SetDefinition, SetReduction, UpdateReduction,
};

pub trait ExecuteReduction<T, U>
where
    Self: ConceptMaker<T, U> + DeleteReduction<U>,
    T: SetReduction
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
    S: ConceptMaker<T, U> + DeleteReduction<U>,
    T: SetReduction
        + MakeReduceFrom
        + GetDefinitionOf
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
    Self: Labeller<T> + Logger + SyntaxFinder<T>,
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

pub trait Labeller<T>
where
    Self: StringMaker + FindOrInsertDefinition<T> + UpdateReduction,
{
    fn label(&self, deltas: &mut Self::Delta, concept: usize, string: &str) -> ZiaResult<()>;
    fn new_labelled_default(&self, deltas: &mut Self::Delta, string: &str) -> ZiaResult<usize>;
    fn setup(&mut self) -> ZiaResult<Self::Delta>;
}

pub trait FindOrInsertDefinition<T>
where
    Self: DefaultMaker<T> + InsertDefinition + FindDefinition,
{
    type A: Default;
    fn find_or_insert_definition(
        &self,
        deltas: &mut Self::Delta,
        lefthand: usize,
        righthand: usize,
        variable: bool,
    ) -> ZiaResult<usize>;
}

pub trait StringMaker: ApplyDelta {
    fn new_string(&self, original_delta: &mut Self::Delta, string: &str) -> usize;
}

pub trait DefaultMaker<T>: ApplyDelta {
    fn new_default<V: Default + Into<T>>(
        &self,
        original_delta: &mut Self::Delta,
        variable: bool,
    ) -> usize;
}
