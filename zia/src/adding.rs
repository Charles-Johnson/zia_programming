/*  Library for the Zia programming language.
    Copyright (C) 2018  Charles Johnson

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
use errors::{ZiaError, ZiaResult};
use reading::{FindDefinition, MaybeString, MightExpand};
use std::{fmt, rc::Rc};
use writing::{
    DeleteReduction, GetDefinition, GetDefinitionOf, GetNormalForm, GetReduction, InsertDefinition,
    MakeReduceFrom, MaybeConcept, NoLongerReducesFrom, RemoveReduction, SetAsDefinitionOf,
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
        + RemoveReduction
        + NoLongerReducesFrom
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString,
    Self::S: Container + PartialEq,
{
    fn execute_reduction(&mut self, syntax: &Self::S, normal_form: &Self::S) -> ZiaResult<String> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            try!(self.try_removing_reduction::<Self::S>(syntax));
            Ok("".to_string())
        } else {
            let syntax_concept = try!(self.concept_from_ast(syntax));
            let normal_form_concept = try!(self.concept_from_ast(normal_form));
            try!(self.update_reduction(syntax_concept, normal_form_concept));
            Ok("".to_string())
        }
    }
}

impl<S, T> ExecuteReduction<T> for S
where
    S: ConceptMaker<T> + DeleteReduction<T>,
    T: SetReduction
        + MakeReduceFrom
        + GetDefinitionOf
        + From<Self::C>
        + From<Self::A>
        + From<String>
        + RemoveReduction
        + NoLongerReducesFrom
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString,
    Self::S: Container + PartialEq<Self::S>,
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
        + GetReduction,
    Self: Labeller<T> + GetNormalForm<T>,
{
    type S: MightExpand<Self::S> + MaybeConcept + fmt::Display;
    fn concept_from_ast(&mut self, ast: &Self::S) -> ZiaResult<usize> {
        if let Some(c) = ast.get_concept() {
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => self.new_labelled_default(string),
                Some((ref left, ref right)) => {
                    let mut leftc = try!(self.concept_from_ast(left));
                    let mut rightc = try!(self.concept_from_ast(right));
                    let concept = try!(self.find_or_insert_definition(leftc, rightc));
                    if !string.contains(' ') {
                        try!(self.label(concept, string));
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
    Self: Labeller<T> + Default,
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
        + MaybeString,
{
    fn new() -> Self {
        let mut cont = Self::default();
        cont.setup().unwrap();
        cont
    }
}

impl<S, T> ContextMaker<T> for S
where
    S: Labeller<T> + Default,
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
        + MaybeString,
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
        + From<Self::A>,
    Self: StringMaker<T> + FindOrInsertDefinition<T> + UpdateReduction<T>,
{
    type C: Default;
    fn label(&mut self, concept: usize, string: &str) -> ZiaResult<()> {
        let definition = try!(self.find_or_insert_definition(LABEL, concept));
        let string_id = self.new_string(string);
        self.update_reduction(definition, string_id)
    }
    fn new_labelled_default(&mut self, string: &str) -> ZiaResult<usize> {
        let new_default = self.new_default::<Self::A>();
        try!(self.label(new_default, string));
        Ok(new_default)
    }
    fn setup(&mut self) -> ZiaResult<()> {
        let label_concept = self.new_default::<Self::C>();
        let define_concept = self.new_default::<Self::C>();
        let reduction_concept = self.new_default::<Self::C>();
        let let_concept = self.new_default::<Self::C>();
		try!(self.label(label_concept, "label_of"));
        try!(self.label(define_concept, ":="));
        try!(self.label(reduction_concept, "->"));
        self.label(let_concept, "let")
    }
}

pub trait FindOrInsertDefinition<T>
where
    T: From<Self::A>
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinitionOf,
    Self: DefaultMaker<T> + InsertDefinition<T> + FindDefinition<T>,
{
    type A: Default;
    fn find_or_insert_definition(&mut self, lefthand: usize, righthand: usize) -> ZiaResult<usize> {
        let pair = self.find_definition(lefthand, righthand);
        match pair {
            None => {
                let definition = self.new_default::<Self::A>();
                try!(self.insert_definition(definition, lefthand, righthand));
                Ok(definition)
            }
            Some(def) => Ok(def),
        }
    }
}

pub trait StringMaker<T>
where
    T: From<String>,
    Self: ConceptAdder<T> + StringAdder,
{
    fn new_string(&mut self, string: &str) -> usize {
        let string_concept = string.to_string().into();
        let index = self.add_concept(string_concept);
        self.add_string(index, string);
        index
    }
}

impl<S, T> StringMaker<T> for S
where
    T: From<String>,
    S: ConceptAdder<T> + StringAdder,
{
}

pub trait DefaultMaker<T>
where
    Self: ConceptAdder<T>,
{
    fn new_default<V: Default + Into<T>>(&mut self) -> usize {
        let concept: T = V::default().into();
        self.add_concept(concept)
    }
}

impl<S, T> DefaultMaker<T> for S where S: ConceptAdder<T> {}

pub trait StringAdder {
    fn add_string(&mut self, usize, &str);
}

pub trait ConceptAdder<T> {
    fn add_concept(&mut self, T) -> usize;
}
