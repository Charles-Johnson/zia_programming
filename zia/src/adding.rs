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
        + FindWhatReducesToIt,
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
        + FindWhatReducesToIt,
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
        + FindWhatReducesToIt,
    Self: Labeller<T> + GetNormalForm<T> + Logger + SyntaxFinder<T>,
    Self::S: DisplayJoint,
    Self::Delta: Clone + fmt::Debug,
{
    type S: MightExpand<Self::S> + MaybeConcept + fmt::Display;
    fn concept_from_ast(&mut self, ast: &Self::S) -> ZiaResult<usize> {
        if let Some(c) = ast.get_concept() {
            info!(
                self.logger(),
                "concept_from_ast({}) -> Ok({}): already included in ast",
                ast.display_joint(),
                c
            );
            Ok(c)
        } else if let Some(c) = self.concept_from_label(&ast.display_joint()) {
            info!(
                self.logger(),
                "concept_from_ast({}) -> Ok({}): derived from label",
                ast.display_joint(),
                c
            );
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => {
                    let new_concept = try!(self.new_labelled_default(string));
                    info!(
                        self.logger(),
                        "concept_from_ast({}) -> Ok({}): new concept created",
                        ast.display_joint(),
                        new_concept
                    );
                    Ok(new_concept)
                }
                Some((ref left, ref right)) => {
                    let mut leftc = try!(self.concept_from_ast(left));
                    let mut rightc = try!(self.concept_from_ast(right));
                    let (deltas, concept) = try!(self.find_or_insert_definition(leftc, rightc));
                    self.apply_all(&deltas);
                    if !string.contains(' ') {
                        try!(self.label(concept, string));
                    }
                    info!(
                        self.logger(),
                        "concept_from_ast({}) -> Ok({}): derived from definition",
                        ast.display_joint(),
                        concept
                    );
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
        + FindWhatReducesToIt,
    Self::Delta: Clone + fmt::Debug,
{
    fn new() -> Self {
        let mut cont = Self::default();
        cont.setup().unwrap();
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
        + FindWhatReducesToIt,
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
        + From<Self::A>,
    Self: StringMaker<T> + FindOrInsertDefinition<T> + UpdateReduction<T>,
    Self::Delta: Clone + fmt::Debug,
{
    type C: Default;
    fn label(&mut self, concept: usize, string: &str) -> ZiaResult<()> {
        let (mut deltas, definition) = try!(self.find_or_insert_definition(LABEL, concept));
        let (string_id, more_deltas) = self.new_string(&deltas, string);
        deltas.extend(more_deltas);
        self.apply_all(&deltas);
        self.update_reduction(definition, string_id)
    }
    fn new_labelled_default(&mut self, string: &str) -> ZiaResult<usize> {
        let (new_default, delta) = self.new_default::<Self::A>(&[]);
        self.apply(&delta);
        try!(self.label(new_default, string));
        Ok(new_default)
    }
    fn setup(&mut self) -> ZiaResult<()> {
        let mut deltas = Vec::<Self::Delta>::new();
        deltas.reserve(4);
        let (label_concept, delta1) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta1);
        let (define_concept, delta2) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta2);
        let (reduction_concept, delta3) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta3);
        let (let_concept, delta4) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta4);
        self.apply_all(&deltas);
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
    Self::Delta: Clone + fmt::Debug,
{
    type A: Default;
    fn find_or_insert_definition(&self, lefthand: usize, righthand: usize) -> ZiaResult<(Vec<Self::Delta>, usize)> {
        let pair = self.find_definition(lefthand, righthand);
        match pair {
            None => {
                let mut deltas = Vec::<Self::Delta>::new();
                let (definition, delta) = self.new_default::<Self::A>(&[]);
                deltas.push(delta);
                let new_deltas = try!(self.insert_definition(&deltas, definition, lefthand, righthand));
                Ok((new_deltas, definition))
            }
            Some(def) => Ok((vec!(), def)),
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
        let (index, concept_delta) = self.add_concept_delta(deltas, string_concept);
        let string_delta = Self::add_string_delta(index, string);
        let new_deltas = vec![concept_delta, string_delta];
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
    fn new_default<V: Default + Into<T>>(&self, deltas: &[Self::Delta]) -> (usize, Self::Delta) {
        let concept: T = V::default().into();
        self.add_concept_delta(deltas, concept)
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
    fn add_concept_delta(&self, &[Self::Delta], T) -> (usize, Self::Delta);
}

pub trait ConceptAdder<T> {
    fn add_concept(&mut self, T) -> usize;
}
